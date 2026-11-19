package util.sail;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import jakarta.annotation.Nonnull;
import util.sail.SailSettings.KeepProfiles;

/**
 * The device profile folders of SAIL runs (a {@code StartDevice} step's browser profile): where they go, and when they
 * are deleted, so that a machine running SAIL all day doesn't fill its disk with them.
 * <ul>
 * <li><b>Where.</b> Every profile folder is {@code <root>/<run token>/<profile>}, with the root from
 * 		{@value SailSettings#PROFILES} ({@link SailSettings#DEFAULT_PROFILES_DIR} under the working directory by
 * 		default).</li>
 * <li><b>When a run ends</b>, after its browsers have quit ({@link SailWorkflow} ends its runs): a run that passed
 * 		deletes {@code <root>/<run token>}, and a run that failed keeps it and prints where it is. With
 * 		{@value SailSettings#PROFILES_KEEP} ({@link KeepProfiles}), every run keeps it, or none does.</li>
 * <li><b>The sweep</b>, once for each root in a JVM, before the first profile folder under it is given out, deletes
 * 		what earlier runs left: runs that were killed, stopped with Ctrl-C or never closed, and the failed runs kept. It
 * 		deletes a folder under the root only if it is named like a run token (base-36 letters and digits; anything else
 * 		is left alone, and a link is never followed), is older than {@value SailSettings#PROFILES_KEEP_HOURS} by both
 * 		its token's time and the newest last-modified time of anything in it, and no browser has one of its profiles
 * 		open: a Chrome profile in use has a {@code SingletonLock} link to {@code host-pid}, and the sweep leaves it alone
 * 		while that process lives. On Windows, where the lock is a file, a profile in use can't be deleted, and is
 * 		skipped.</li>
 * </ul>
 * Several JVMs (parallel lanes) may share a root: the sweep deletes only runs past the keep time, never this JVM's own,
 * and leaves alone a run whose profile a browser has open, so it is safe beside another lane's run. Housekeeping never
 * fails or skips a test: a folder that can't be deleted, or checked, gets a one-line warning naming it and why, and is
 * left for a later sweep.
 * <p>
 * Thread-safe.
 */
public final class SailProfiles {
	/** A run token: the run's start time in milliseconds in base 36, 8 lower-case letters and digits since 1973. */
	private static final Pattern RUN_TOKEN = Pattern.compile("[0-9a-z]{8,13}");
	/** What Chrome holds in a profile it has open: a link to {@code host-pid} (a file on Windows). */
	private static final String SINGLETON_LOCK = "SingletonLock";
	/** The profiles of each root that a folder was given out under, by root. */
	private static final Map<Path, SailProfiles> ROOTS = new HashMap<>();

	private final Path root;
	private final Clock clock;
	/** Prints a run's kept folder and the warnings. */
	private final Consumer<String> out;
	/** True once the root has been swept. */
	private boolean swept;
	/** The runs of this JVM with folders under the root that have not ended, with what each keeps when it ends. */
	private final Map<String, KeepProfiles> open = new HashMap<>();
	/** Every run of this JVM given a folder under the root: the sweep never deletes them. */
	private final Set<String> mine = new HashSet<>();

	/**
	 * Creates the profiles of a root.
	 *
	 * @param root	the folder the run folders go in
	 * @param clock	tells the sweep the time now
	 * @param out	prints a kept run's folder and the warnings, one line each
	 */
	SailProfiles(@Nonnull Path root, @Nonnull Clock clock, @Nonnull Consumer<String> out) {
		this.root = root.toAbsolutePath().normalize();
		this.clock = clock;
		this.out = out;
	}

	/**
	 * Returns the folder of a run's device profile, {@code <root>/<run token>/<profile>}, with the root, and what the run
	 * keeps when it ends, from the settings. The first time a folder under a root is asked for in this JVM, the root is
	 * swept first. The folder is not made: the browser makes it.
	 *
	 * @param settings	the leg's settings: {@value SailSettings#PROFILES}, {@value SailSettings#PROFILES_KEEP} and
	 * 					{@value SailSettings#PROFILES_KEEP_HOURS}
	 * @param run	the run token
	 * @param profile	the device's name
	 * @return the folder, absolute
	 */
	public static @Nonnull Path folder(@Nonnull SailSettings settings, @Nonnull String run, @Nonnull String profile) {
		SailProfiles profiles;
		synchronized (ROOTS) {
			profiles = ROOTS.computeIfAbsent(settings.profiles().toAbsolutePath().normalize(),
												root -> new SailProfiles(root, Clock.systemUTC(), System.out::println));
		}
		return profiles.give(run, profile, settings.profilesKeep(), settings.profilesKeepHours());
	}

	/**
	 * Ends a run's device profiles, under every root it was given a folder under in this JVM; call it after the run's
	 * browsers have quit. A run that passed deletes its folder; a run that failed keeps it, and prints
	 * {@code SAIL device profiles kept for the failed run <run>: <folder>}; or as {@value SailSettings#PROFILES_KEEP}
	 * says. A run with no folder does nothing.
	 *
	 * @param run	the run token
	 * @param passed	true if the run passed
	 */
	static void end(@Nonnull String run, boolean passed) {
		List<SailProfiles> all;
		synchronized (ROOTS) {
			all = new ArrayList<>(ROOTS.values());
		}
		for (SailProfiles profiles : all) {
			profiles.endRun(run, passed);
		}
	}

	/**
	 * Deletes a folder and everything in it, if it exists, following no links: a link is deleted, not what it points to.
	 *
	 * @param folder	the folder
	 * @throws IOException	if something in it can't be deleted; what was deleted before stays deleted
	 */
	public static void delete(@Nonnull Path folder) throws IOException {
		if (! Files.exists(folder, LinkOption.NOFOLLOW_LINKS)) {
			return;
		}
		List<Path> files;
		try (Stream<Path> walk = Files.walk(folder)) {
			files = walk.sorted(Comparator.reverseOrder()).toList();
		}
		catch (UncheckedIOException e) {
			throw e.getCause();
		}
		for (Path file : files) {
			Files.delete(file);
		}
	}

	/**
	 * Gives out a run's profile folder, sweeping the root first if this is the first.
	 *
	 * @param run	the run token
	 * @param profile	the device's name
	 * @param keep	what the run keeps when it ends; the last folder given out for the run says
	 * @param keepHours	how many hours old a run must be for the sweep to delete it
	 * @return {@code <root>/<run>/<profile>}
	 */
	synchronized @Nonnull Path give(@Nonnull String run, @Nonnull String profile, @Nonnull KeepProfiles keep, int keepHours) {
		mine.add(run);
		open.put(run, keep);
		if (! swept) {
			swept = true;
			sweep(Duration.ofHours(keepHours));
		}
		return root.resolve(run).resolve(profile);
	}

	/**
	 * Ends a run's folder under this root, if a folder was given out for it and the run has not ended already.
	 *
	 * @param run	the run token
	 * @param passed	true if the run passed
	 */
	synchronized void endRun(@Nonnull String run, boolean passed) {
		KeepProfiles keep = open.remove(run);
		Path folder = root.resolve(run);
		if ((keep == null) || (! Files.exists(folder, LinkOption.NOFOLLOW_LINKS))) {
			return;
		}
		if ((keep == KeepProfiles.ALL) || ((keep == KeepProfiles.FAILED) && (! passed))) {
			if (! passed) {
				out.accept("SAIL device profiles kept for the failed run " + run + ": " + folder);
			}
			return;
		}
		deleteOrWarn(folder);
	}

	/**
	 * Deletes each run folder under the root that is past the keep time and not in use; see the class comment.
	 *
	 * @param keep	how old a run must be, by its token and by its files
	 */
	private void sweep(@Nonnull Duration keep) {
		if (! Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS)) {
			return;
		}
		Instant cutoff = clock.instant().minus(keep);
		List<Path> folders = new ArrayList<>();
		try (DirectoryStream<Path> list = Files.newDirectoryStream(root)) {
			list.forEach(folders::add);
		}
		catch (IOException | UncheckedIOException e) {
			warn("could not sweep", root, e);
			return;
		}
		for (Path folder : folders) {
			if (sweepable(folder, cutoff)) {
				deleteOrWarn(folder);
			}
		}
	}

	/**
	 * Indicates if the sweep deletes a folder under the root: a real folder (not a link) named like a run token that is
	 * not this JVM's, older than the cutoff by its token and its newest file, with no profile in use.
	 */
	private boolean sweepable(@Nonnull Path folder, @Nonnull Instant cutoff) {
		String name = folder.getFileName().toString();
		if ((! RUN_TOKEN.matcher(name).matches()) || mine.contains(name) || (! Files.isDirectory(folder, LinkOption.NOFOLLOW_LINKS))) {
			return false;
		}
		try {
			return Instant.ofEpochMilli(Long.parseLong(name, Character.MAX_RADIX)).isBefore(cutoff) &&
					newest(folder).isBefore(cutoff) &&
					(! inUse(folder));
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) { // too big for a time: not a run token
			return false;
		}
		catch (IOException | UncheckedIOException e) {
			warn("could not check", folder, e);
			return false;
		}
	}

	/** Returns the newest last-modified time of the folder and anything in it, following no links. */
	private static @Nonnull Instant newest(@Nonnull Path folder) throws IOException {
		Instant result = Instant.MIN;
		List<Path> files;
		try (Stream<Path> walk = Files.walk(folder)) {
			files = walk.toList();
		}
		for (Path file : files) {
			Instant modified = Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS).toInstant();
			if (modified.isAfter(result)) {
				result = modified;
			}
		}
		return result;
	}

	/**
	 * Indicates if a browser has one of a run's profiles open: its {@code SingletonLock} is a link to {@code host-pid}
	 * naming a process that is alive.
	 */
	private static boolean inUse(@Nonnull Path run) throws IOException {
		List<Path> profiles;
		try (Stream<Path> list = Files.list(run)) {
			profiles = list.toList();
		}
		for (Path profile : profiles) {
			Path lock = profile.resolve(SINGLETON_LOCK);
			if (Files.isSymbolicLink(lock)) {
				String target = Files.readSymbolicLink(lock).toString();
				try {
					long pid = Long.parseLong(target.substring(target.lastIndexOf('-') + 1));
					if (ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(Boolean.FALSE).booleanValue()) {
						return true;
					}
				}
				catch (@SuppressWarnings("unused") NumberFormatException e) {
					// names no process
				}
			}
		}
		return false;
	}

	/** Deletes a folder, or prints a warning naming it and why it could not be deleted. */
	private void deleteOrWarn(@Nonnull Path folder) {
		try {
			delete(folder);
		}
		catch (IOException | UncheckedIOException e) {
			warn("could not delete", folder, e);
		}
	}

	private void warn(@Nonnull String what, @Nonnull Path folder, @Nonnull Exception e) {
		Throwable cause = (e instanceof UncheckedIOException u) ? u.getCause() : e;
		out.accept("SAIL device profiles: " + what + ' ' + folder + ": " + cause);
	}
}

package util.sail;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import jakarta.annotation.Nonnull;
import util.sail.SailSettings.KeepProfiles;

/**
 * The device profile folders of SAIL runs (a {@code StartDevice} step's browser profile): where they go, and when they
 * are deleted, so that a machine running SAIL all day doesn't fill its disk with them.
 * <ul>
 * <li><b>Where.</b> Every profile folder is {@code <root>/<run token>-<pid>/<profile>}, with the root from
 * 		{@value SailSettings#PROFILES} ({@link SailSettings#DEFAULT_PROFILES_DIR} under the working directory by
 * 		default), and the pid this JVM's, as two JVMs can start a run in the same millisecond. The run folder is made
 * 		with a marker file in it, {@value #MARKER}, so the sweep knows it is a run folder.</li>
 * <li><b>When a run ends</b>, after its browsers have quit ({@link SailWorkflow} ends its runs): a run that passed
 * 		deletes its run folder, and a run that failed keeps it and prints where it is. With
 * 		{@value SailSettings#PROFILES_KEEP} ({@link KeepProfiles}), every run keeps it, or none does. A run folder that
 * 		holds only its marker (no browser started on a profile in it) is deleted either way.</li>
 * <li><b>The sweep</b>, once for each root in a JVM, before the first profile folder under it is given out, deletes
 * 		what earlier runs left: runs that were killed, stopped with Ctrl-C or never closed, and the failed runs kept. It
 * 		deletes a folder under the root only if it is a run folder: named {@code <run token>-<pid>} (base-36 letters and
 * 		digits, a hyphen and digits; anything else is left alone, and a link is never followed) with the marker in it.
 * 		And only if it is older than {@value SailSettings#PROFILES_KEEP_HOURS} by both its token's time and the newest
 * 		last-modified time of anything in it, and no browser has one of its profiles open: a Chrome profile in use has a
 * 		{@code SingletonLock} link to {@code host-pid}, and the sweep leaves it alone while that process lives.</li>
 * <li><b>Deleting</b> a run folder first renames it under the root, to {@code <run token>-<pid>.deleting-<pid>} (the
 * 		second pid the deleting JVM's), then deletes it, its marker last. On Windows, where Chrome's lock is a file, a
 * 		folder with a profile in use can't be renamed, so it is left whole rather than partly deleted; and two JVMs can't
 * 		both delete one folder (the one whose rename comes second leaves it to the other, saying nothing). A delete that stopped halfway leaves the renamed folder, with its marker, for a later
 * 		sweep.</li>
 * </ul>
 * Several JVMs (parallel lanes) may share a root: the sweep deletes only runs past the keep time, never this JVM's own,
 * and leaves alone a run whose profile a browser has open, so it is safe beside another lane's run. Housekeeping never
 * fails or skips a test: a folder that can't be deleted, or checked, gets a one-line warning naming it and why, and is
 * left for a later sweep.
 * <p>
 * Thread-safe.
 */
public final class SailProfiles {
	/**
	 * A run folder's name: the run token (the run's start time in milliseconds in base 36, 8 lower-case letters and
	 * digits since 1973), a hyphen and the pid of its JVM; and, once a JVM has begun deleting it, {@code .deleting-}
	 * and that JVM's pid.
	 */
	private static final Pattern RUN_FOLDER = Pattern.compile("([0-9a-z]{8,13})-[0-9]{1,19}(?:\\.deleting-[0-9]{1,19})?");
	/** The file in each run folder SailProfiles makes, which marks it as one for the sweep. */
	static final String MARKER = ".sail-run";
	/** What a run folder's name ends with once a JVM has begun deleting it, before that JVM's pid. */
	private static final String DELETING = ".deleting-";
	/** What Chrome holds in a profile it has open: a link to {@code host-pid} (a file on Windows). */
	private static final String SINGLETON_LOCK = "SingletonLock";
	/** The profiles of each root that a folder was given out under, by root. */
	private static final Map<Path, SailProfiles> ROOTS = new HashMap<>();

	private final Path root;
	private final Clock clock;
	/** The pid of this JVM, in its run folders' names. */
	private final long pid;
	/** Prints a run's kept folder and the warnings. */
	private final Consumer<String> out;
	/** True once the root has been swept. */
	private boolean swept;
	/** The runs of this JVM with folders under the root that have not ended, with what each keeps when it ends. */
	private final Map<String, KeepProfiles> open = new HashMap<>();
	/** The run folders of this JVM's runs under the root, by name: the sweep never deletes them. */
	private final Set<String> mine = new HashSet<>();

	/**
	 * Creates the profiles of a root.
	 *
	 * @param root	the folder the run folders go in
	 * @param clock	tells the sweep the time now
	 * @param pid	the pid to name this JVM's run folders with
	 * @param out	prints a kept run's folder and the warnings, one line each
	 */
	SailProfiles(@Nonnull Path root, @Nonnull Clock clock, long pid, @Nonnull Consumer<String> out) {
		this.root = root.toAbsolutePath().normalize();
		this.clock = clock;
		this.pid = pid;
		this.out = out;
	}

	/**
	 * Returns the folder of a run's device profile, {@code <root>/<run token>-<pid>/<profile>}, with the root, and what
	 * the run keeps when it ends, from the settings; and makes the run folder, with its marker. The first time a folder
	 * under a root is asked for in this JVM, the root is swept first. The profile folder is not made: the browser makes
	 * it.
	 *
	 * @param settings	the leg's settings: {@value SailSettings#PROFILES}, {@value SailSettings#PROFILES_KEEP} and
	 * 					{@value SailSettings#PROFILES_KEEP_HOURS}
	 * @param run	the run token
	 * @param profile	the device's name
	 * @return the folder, absolute
	 * @throws UncheckedIOException	if the run folder or its marker can't be made
	 */
	public static @Nonnull Path folder(@Nonnull SailSettings settings, @Nonnull String run, @Nonnull String profile) {
		SailProfiles profiles;
		synchronized (ROOTS) {
			profiles = ROOTS.computeIfAbsent(settings.profiles().toAbsolutePath().normalize(),
												root -> new SailProfiles(root, Clock.systemUTC(), ProcessHandle.current().pid(), System.out::println));
		}
		return profiles.give(run, profile, settings.profilesKeep(), settings.profilesKeepHours());
	}

	/**
	 * Returns where a run's folder is under the settings' root, {@code <root>/<run token>-<pid>}, without giving it out:
	 * nothing is made, swept or noted for the run to end. For a step that checks a profile folder is the run's.
	 *
	 * @param settings	the leg's settings, for {@value SailSettings#PROFILES}
	 * @param run	the run token
	 * @return the run folder, absolute, which may not exist
	 */
	public static @Nonnull Path runFolder(@Nonnull SailSettings settings, @Nonnull String run) {
		return settings.profiles().toAbsolutePath().normalize().resolve(name(run, ProcessHandle.current().pid()));
	}

	/** Returns the name of a run's folder, {@code <run token>-<pid>}. */
	private static @Nonnull String name(@Nonnull String run, long pid) {
		return run + '-' + pid;
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
	 * Gives out a run's profile folder, sweeping the root first if this is the first, and makes the run folder with its
	 * marker.
	 *
	 * @param run	the run token
	 * @param profile	the device's name
	 * @param keep	what the run keeps when it ends; the last folder given out for the run says
	 * @param keepHours	how many hours old a run must be for the sweep to delete it
	 * @return {@code <root>/<run>-<pid>/<profile>}
	 * @throws UncheckedIOException	if the run folder or its marker can't be made
	 */
	synchronized @Nonnull Path give(@Nonnull String run, @Nonnull String profile, @Nonnull KeepProfiles keep, int keepHours) {
		String name = name(run, pid);
		mine.add(name);
		open.put(run, keep);
		if (! swept) {
			swept = true;
			sweep(Duration.ofHours(keepHours));
		}
		Path folder = root.resolve(name);
		try {
			Path marker = Files.createDirectories(folder).resolve(MARKER);
			if (! Files.exists(marker, LinkOption.NOFOLLOW_LINKS)) {
				Files.createFile(marker);
			}
		}
		catch (IOException e) {
			throw new UncheckedIOException("Could not make the SAIL device profiles folder " + folder, e);
		}
		return folder.resolve(profile);
	}

	/**
	 * Ends a run's folder under this root, if a folder was given out for it and the run has not ended already.
	 *
	 * @param run	the run token
	 * @param passed	true if the run passed
	 */
	synchronized void endRun(@Nonnull String run, boolean passed) {
		KeepProfiles keep = open.remove(run);
		Path folder = root.resolve(name(run, pid));
		if ((keep == null) || (! Files.exists(folder, LinkOption.NOFOLLOW_LINKS))) {
			return;
		}
		if (((keep == KeepProfiles.ALL) || ((keep == KeepProfiles.FAILED) && (! passed))) && (! onlyMarker(folder))) {
			if (! passed) {
				out.accept("SAIL device profiles kept for the failed run " + run + ": " + folder);
			}
			return;
		}
		deleteRun(folder);
	}

	/** Indicates whether a run folder holds nothing but its marker: no browser made a profile in it. */
	private boolean onlyMarker(@Nonnull Path folder) {
		try (Stream<Path> list = Files.list(folder)) {
			return list.allMatch(file -> MARKER.equals(file.getFileName().toString()));
		}
		catch (IOException | UncheckedIOException e) {
			warn("could not check", folder, e);
			return false;
		}
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
				deleteRun(folder);
			}
		}
	}

	/**
	 * Indicates if the sweep deletes a folder under the root: a real folder (not a link) named like a run folder, with
	 * the marker in it, that is not this JVM's, older than the cutoff by its token and its newest file, with no profile
	 * in use.
	 */
	private boolean sweepable(@Nonnull Path folder, @Nonnull Instant cutoff) {
		String name = folder.getFileName().toString();
		Matcher run = RUN_FOLDER.matcher(name);
		if ((! run.matches()) || mine.contains(name) || (! Files.isDirectory(folder, LinkOption.NOFOLLOW_LINKS)) ||
				(! Files.isRegularFile(folder.resolve(MARKER), LinkOption.NOFOLLOW_LINKS))) {
			return false;
		}
		try {
			return Instant.ofEpochMilli(Long.parseLong(run.group(1), Character.MAX_RADIX)).isBefore(cutoff) &&
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

	/**
	 * Deletes a run folder, or prints a warning naming it and why it could not be deleted. It is renamed under the root
	 * first: on Windows that fails while a browser has a profile in it open, so the folder is left whole, and it is how
	 * this JVM claims it from another sweeping at the same time. Its marker goes last, so a delete that stops halfway
	 * leaves a folder the sweep still knows.
	 */
	private void deleteRun(@Nonnull Path folder) {
		String name = folder.getFileName().toString();
		int deleting = name.indexOf(DELETING);
		Path renamed = root.resolve(((deleting < 0) ? name : name.substring(0, deleting)) + DELETING + pid);
		Path current;
		try {
			current = Files.move(folder, renamed, StandardCopyOption.ATOMIC_MOVE);
		}
		catch (@SuppressWarnings("unused") NoSuchFileException e) {
			return; // another JVM's rename came first: it claimed the folder, and deletes it
		}
		catch (IOException e) {
			warn("could not delete", folder, e);
			return;
		}
		try {
			List<Path> files;
			try (Stream<Path> list = Files.list(current)) {
				files = list.toList();
			}
			Path marker = current.resolve(MARKER);
			for (Path file : files) {
				if (! file.equals(marker)) {
					delete(file);
				}
			}
			delete(current);
		}
		catch (IOException | UncheckedIOException e) {
			warn("could not delete", current, e);
		}
	}

	private void warn(@Nonnull String what, @Nonnull Path folder, @Nonnull Exception e) {
		Throwable cause = (e instanceof UncheckedIOException u) ? u.getCause() : e;
		out.accept("SAIL device profiles: " + what + ' ' + folder + ": " + cause);
	}
}

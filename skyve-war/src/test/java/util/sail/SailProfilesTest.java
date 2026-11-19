package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import util.sail.SailSettings.KeepProfiles;

/**
 * Tests the device profile housekeeping of {@link SailProfiles} on a temporary root, with no browser: a run's folder is
 * deleted or kept when the run ends, as {@code sail.profiles.keep} says, and the sweep deletes only old, unused run
 * folders, leaving everything else alone. Housekeeping that can't delete a folder warns, and never fails.
 */
class SailProfilesTest {
	/** The time now, for the sweep. */
	private static final Instant NOW = Instant.now().truncatedTo(ChronoUnit.SECONDS);
	/** Older than the 12 hours a run is kept. */
	private static final Instant OLD = NOW.minus(Duration.ofHours(13));
	/** Newer than the 12 hours a run is kept. */
	private static final Instant RECENT = NOW.minus(Duration.ofHours(1));
	private static final int KEEP_HOURS = 12;

	@TempDir
	Path directory;

	/** What the profiles printed. */
	private final List<String> messages = new ArrayList<>();

	private SailProfiles profiles() {
		return new SailProfiles(directory.resolve("profiles"), Clock.fixed(NOW, ZoneOffset.UTC), messages::add);
	}

	/** Returns the run token of a run that started at a time. */
	private static String token(Instant started) {
		return Long.toString(started.toEpochMilli(), Character.MAX_RADIX);
	}

	/** Makes a run's profile folder with a file in it, as a browser would, and returns the run's folder. */
	private Path run(String token, String profile) throws IOException {
		Path folder = directory.resolve("profiles").resolve(token);
		Files.writeString(Files.createDirectories(folder.resolve(profile).resolve("Default")).resolve("Preferences"), "{}");
		return folder;
	}

	/** Sets the last-modified time of a folder and everything in it, links included (not what they point to). */
	private static void age(Path folder, Instant time) throws IOException, InterruptedException {
		List<Path> files;
		try (Stream<Path> walk = Files.walk(folder)) {
			files = walk.sorted(Comparator.reverseOrder()).toList();
		}
		for (Path file : files) { // what is in a folder first, then the folder
			if (Files.isSymbolicLink(file)) {
				// the JDK can't set a link's own time on macOS (it opens what the link points to): touch -h can
				String stamp = DateTimeFormatter.ofPattern("yyyyMMddHHmm.ss").withZone(ZoneId.systemDefault()).format(time);
				assertEquals(0, new ProcessBuilder("touch", "-h", "-t", stamp, file.toString()).inheritIO().start().waitFor(), "touch -h " + file);
			}
			else {
				Files.setLastModifiedTime(file, FileTime.from(time));
			}
		}
	}

	/** Gives Chrome's lock to a profile, naming a process, as Chrome does while it has the profile open. */
	private static void lock(Path run, String profile, long pid) throws IOException {
		Files.createSymbolicLink(run.resolve(profile).resolve("SingletonLock"), Path.of("a-host.local-" + pid));
	}

	@Test
	void aPassedRunsFolderIsDeletedAndAFailedRunsIsKept() throws IOException {
		SailProfiles profiles = profiles();
		Path passed = profiles.give("passedrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS).getParent();
		Path failed = profiles.give("failedrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS).getParent();
		run("passedrun", "phone1");
		run("passedrun", "phone2");
		run("failedrun", "phone1");
		profiles.endRun("passedrun", true);
		profiles.endRun("failedrun", false);
		profiles.endRun("failedrun", false); // a run ends once
		assertAll(() -> assertEquals(directory.resolve("profiles").resolve("passedrun"), passed, "<root>/<run>"),
					() -> assertFalse(Files.exists(passed, LinkOption.NOFOLLOW_LINKS), "the passed run's folder is deleted"),
					() -> assertTrue(Files.isDirectory(failed.resolve("phone1")), "the failed run's folder is kept"),
					() -> assertEquals(List.of("SAIL device profiles kept for the failed run failedrun: " + failed), messages, "and printed, once"));
	}

	@Test
	void keepAllKeepsEveryRunsFolderAndKeepNoneDeletesEveryRuns() throws IOException {
		SailProfiles profiles = profiles();
		Path allPassed = profiles.give("allpassed", "p", KeepProfiles.ALL, KEEP_HOURS).getParent();
		Path allFailed = profiles.give("allfailed", "p", KeepProfiles.ALL, KEEP_HOURS).getParent();
		Path nonePassed = profiles.give("nonepassed", "p", KeepProfiles.NONE, KEEP_HOURS).getParent();
		Path noneFailed = profiles.give("nonefailed", "p", KeepProfiles.NONE, KEEP_HOURS).getParent();
		for (String token : List.of("allpassed", "allfailed", "nonepassed", "nonefailed")) {
			run(token, "p");
		}
		profiles.endRun("allpassed", true);
		profiles.endRun("allfailed", false);
		profiles.endRun("nonepassed", true);
		profiles.endRun("nonefailed", false);
		assertAll(() -> assertTrue(Files.isDirectory(allPassed), "all: a passed run's is kept"),
					() -> assertTrue(Files.isDirectory(allFailed), "all: a failed run's is kept"),
					() -> assertFalse(Files.exists(nonePassed), "none: a passed run's is deleted"),
					() -> assertFalse(Files.exists(noneFailed), "none: a failed run's is deleted"),
					() -> assertEquals(List.of("SAIL device profiles kept for the failed run allfailed: " + allFailed), messages,
										"only a failed run's kept folder is printed"));
	}

	/** A run that never had a folder, or whose browser never made one, ends with nothing to do. */
	@Test
	void aRunWithoutAFolderEndsQuietly() {
		SailProfiles profiles = profiles();
		profiles.give("nofolder", "p", KeepProfiles.FAILED, KEEP_HOURS);
		profiles.endRun("nofolder", false);
		profiles.endRun("unknown", false);
		assertEquals(List.of(), messages);
	}

	/**
	 * The sweep, before the first folder is given out, deletes the run folders past the keep time by both their token and
	 * their newest file that no live browser has open; it leaves everything else alone.
	 */
	@Test
	void theSweepDeletesOnlyOldUnusedRunFolders() throws IOException, InterruptedException {
		Path old = run(token(OLD), "phone1");
		age(old, OLD);
		Path freshFile = run(token(OLD.minusSeconds(1)), "phone1");
		age(freshFile, OLD);
		Files.setLastModifiedTime(freshFile.resolve("phone1").resolve("Default").resolve("Preferences"), FileTime.from(RECENT));
		Path recentToken = run(token(RECENT), "phone1");
		age(recentToken, OLD);
		Path inUse = run(token(OLD.minusSeconds(2)), "phone1");
		run(token(OLD.minusSeconds(2)), "phone2");
		lock(inUse, "phone2", ProcessHandle.current().pid());
		age(inUse, OLD);
		Path deadLock = run(token(OLD.minusSeconds(3)), "phone1");
		lock(deadLock, "phone1", deadPid());
		age(deadLock, OLD);
		Path notARun = run("not-a-run", "phone1");
		age(notARun, OLD);
		Path tooShort = run("abc123", "phone1");
		age(tooShort, OLD);
		Path outside = Files.createDirectories(directory.resolve("outside"));
		Files.writeString(outside.resolve("precious"), "keep me");
		age(outside, OLD);
		Path link = Files.createSymbolicLink(directory.resolve("profiles").resolve(token(OLD.minusSeconds(4))), outside);
		age(link, OLD);
		Path linkInside = run(token(OLD.minusSeconds(5)), "phone1");
		Files.createSymbolicLink(linkInside.resolve("phone1").resolve("elsewhere"), outside);
		age(linkInside, OLD);
		Path current = run(token(OLD.minusSeconds(6)), "phone1");
		age(current, OLD);

		SailProfiles profiles = profiles();
		profiles.give(current.getFileName().toString(), "phone1", KeepProfiles.FAILED, KEEP_HOURS); // this JVM's run
		Path later = run(token(OLD.minusSeconds(7)), "phone1");
		age(later, OLD);
		profiles.give("anotherrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS); // the sweep ran already

		assertAll(() -> assertFalse(Files.exists(old, LinkOption.NOFOLLOW_LINKS), "an old run is deleted"),
					() -> assertTrue(Files.isDirectory(freshFile), "an old token with a recent file is kept"),
					() -> assertTrue(Files.isDirectory(recentToken), "a recent token with old files is kept"),
					() -> assertTrue(Files.isDirectory(inUse), "a run whose lock names a live process is kept"),
					() -> assertFalse(Files.exists(deadLock, LinkOption.NOFOLLOW_LINKS), "a run whose lock names a dead process is deleted"),
					() -> assertTrue(Files.isDirectory(notARun), "a folder not named like a run token is left alone"),
					() -> assertTrue(Files.isDirectory(tooShort), "a name too short for a run token is left alone"),
					() -> assertTrue(Files.isSymbolicLink(link), "a link named like a run token is left alone"),
					() -> assertFalse(Files.exists(linkInside, LinkOption.NOFOLLOW_LINKS), "an old run with a link in it is deleted"),
					() -> assertEquals("keep me", Files.readString(outside.resolve("precious")), "what a link points to is never touched"),
					() -> assertTrue(Files.isDirectory(current), "this JVM's run is never swept"),
					() -> assertTrue(Files.isDirectory(later), "the sweep runs once"),
					() -> assertEquals(List.of(), messages, "no warnings"));
	}

	/** A sweep of a root that doesn't exist yet does nothing. */
	@Test
	void theSweepOfARootThatDoesNotExistDoesNothing() {
		Path folder = profiles().give(token(NOW), "phone1", KeepProfiles.FAILED, KEEP_HOURS);
		assertAll(() -> assertEquals(directory.resolve("profiles").resolve(token(NOW)).resolve("phone1"), folder),
					() -> assertFalse(Files.exists(directory.resolve("profiles"))),
					() -> assertEquals(List.of(), messages));
	}

	/**
	 * A folder that can't be deleted (on Windows, a profile a browser has open; here, a folder that can't be written)
	 * gets a warning naming it, whether a run ended or the sweep found it, and nothing fails.
	 */
	@Test
	void aFolderThatCannotBeDeletedGetsAWarningNotAFailure() throws IOException, InterruptedException {
		assumeTrue(Files.getFileAttributeView(directory, PosixFileAttributeView.class) != null, "needs POSIX permissions");
		Path old = run(token(OLD), "phone1");
		age(old, OLD);
		Path locked = old.resolve("phone1").resolve("Default");
		Path passed = run("passedrun", "phone1");
		Path lockedToo = passed.resolve("phone1").resolve("Default");
		Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("r-xr-xr-x"));
		Files.setPosixFilePermissions(lockedToo, PosixFilePermissions.fromString("r-xr-xr-x"));
		try {
			SailProfiles profiles = profiles();
			profiles.give("passedrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS);
			profiles.endRun("passedrun", true);
			assertAll(() -> assertEquals(2, messages.size(), messages.toString()),
						() -> assertTrue(messages.get(0).startsWith("SAIL device profiles: could not delete " + old + ": java.nio.file.AccessDeniedException"),
											messages.get(0)),
						() -> assertTrue(messages.get(1).startsWith("SAIL device profiles: could not delete " + passed + ": java.nio.file.AccessDeniedException"),
											messages.get(1)),
						() -> assertTrue(Files.isDirectory(locked), "the old run is left for a later sweep"),
						() -> assertTrue(Files.isDirectory(lockedToo), "and the passed run too"));
		}
		finally {
			Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("rwxr-xr-x"));
			Files.setPosixFilePermissions(lockedToo, PosixFilePermissions.fromString("rwxr-xr-x"));
		}
	}

	/** Returns the pid of a process that has ended. */
	private static long deadPid() throws IOException, InterruptedException {
		Process process = new ProcessBuilder("true").start();
		process.waitFor();
		return process.pid();
	}
}

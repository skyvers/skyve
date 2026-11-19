package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
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
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

import util.sail.SailSettings.KeepProfiles;

/**
 * Tests the device profile housekeeping of {@link SailProfiles} on a temporary root, with no browser: a run's folder is
 * {@code <run token>-<pid>} with a marker in it, and is deleted or kept when the run ends, as {@code sail.profiles.keep}
 * says; the sweep deletes only old, unused, marked run folders, leaving everything else alone, and renames a folder
 * before it deletes it. Housekeeping that can't delete a folder warns, and never fails.
 * <p>
 * The sweep tests make links, age them with {@code touch -h} and run {@code true}, so they don't run on Windows; the
 * permission tests need a user that file permissions bind (not root).
 */
class SailProfilesTest {
	/** The time now, for the sweep. */
	private static final Instant NOW = Instant.now().truncatedTo(ChronoUnit.SECONDS);
	/** Older than the 12 hours a run is kept. */
	private static final Instant OLD = NOW.minus(Duration.ofHours(13));
	/** Newer than the 12 hours a run is kept. */
	private static final Instant RECENT = NOW.minus(Duration.ofHours(1));
	private static final int KEEP_HOURS = 12;
	/** The pid of the JVM the profiles under test belong to. */
	private static final long PID = 4242;
	/** The pid of another JVM sharing the root. */
	private static final long OTHER_PID = 777;

	@TempDir
	Path directory;

	/** What the profiles printed. */
	private final List<String> messages = new ArrayList<>();

	private Path root() {
		return directory.resolve("profiles");
	}

	private SailProfiles profiles() {
		return profiles(PID);
	}

	private SailProfiles profiles(long pid) {
		return new SailProfiles(root(), Clock.fixed(NOW, ZoneOffset.UTC), pid, messages::add);
	}

	/** Returns the run token of a run that started at a time. */
	private static String token(Instant started) {
		return Long.toString(started.toEpochMilli(), Character.MAX_RADIX);
	}

	/** Returns the name of another JVM's run folder for a run that started at a time. */
	private static String other(Instant started) {
		return token(started) + '-' + OTHER_PID;
	}

	/** Makes a run folder with its marker and a profile with a file in it, as SailProfiles and a browser would. */
	private Path run(String name, String profile) throws IOException {
		Path folder = unmarked(name, profile);
		Files.writeString(folder.resolve(SailProfiles.MARKER), "");
		return folder;
	}

	/** Makes a folder under the root with a profile in it, but no marker. */
	private Path unmarked(String name, String profile) throws IOException {
		Path folder = root().resolve(name);
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
		boolean marked = Files.isRegularFile(passed.resolve(SailProfiles.MARKER));
		run("passedrun-" + PID, "phone1");
		run("passedrun-" + PID, "phone2");
		run("failedrun-" + PID, "phone1");
		profiles.endRun("passedrun", true);
		profiles.endRun("failedrun", false);
		profiles.endRun("failedrun", false); // a run ends once
		assertAll(() -> assertEquals(root().resolve("passedrun-" + PID), passed, "<root>/<run>-<pid>"),
					() -> assertTrue(marked, "the run folder is made with its marker"),
					() -> assertFalse(Files.exists(passed, LinkOption.NOFOLLOW_LINKS), "the passed run's folder is deleted"),
					() -> assertTrue(Files.isDirectory(failed.resolve("phone1")), "the failed run's folder is kept"),
					() -> assertEquals(List.of("SAIL device profiles kept for the failed run failedrun: " + failed), messages, "and printed, once"),
					() -> assertEquals(List.of(failed.getFileName()), list(root()), "and nothing else is left, not even the renamed passed run"));
	}

	@Test
	void keepAllKeepsEveryRunsFolderAndKeepNoneDeletesEveryRuns() throws IOException {
		SailProfiles profiles = profiles();
		Path allPassed = profiles.give("allpassed", "p", KeepProfiles.ALL, KEEP_HOURS).getParent();
		Path allFailed = profiles.give("allfailed", "p", KeepProfiles.ALL, KEEP_HOURS).getParent();
		Path nonePassed = profiles.give("nonepassed", "p", KeepProfiles.NONE, KEEP_HOURS).getParent();
		Path noneFailed = profiles.give("nonefailed", "p", KeepProfiles.NONE, KEEP_HOURS).getParent();
		for (String token : List.of("allpassed", "allfailed", "nonepassed", "nonefailed")) {
			run(token + '-' + PID, "p");
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

	/**
	 * A run whose browser never made a profile has nothing to keep: its run folder, holding only the marker, is deleted
	 * quietly even when the run failed. A run that was never given a folder ends with nothing to do.
	 */
	@Test
	void aRunWithoutAProfileEndsQuietly() {
		SailProfiles profiles = profiles();
		Path folder = profiles.give("noprofile", "p", KeepProfiles.FAILED, KEEP_HOURS).getParent();
		profiles.endRun("noprofile", false);
		profiles.endRun("unknown", false);
		assertAll(() -> assertFalse(Files.exists(folder), "the run folder with only its marker is deleted"),
					() -> assertEquals(List.of(), messages));
	}

	/**
	 * Two JVMs that start a run in the same millisecond have the same run token, but their run folders differ by pid,
	 * so one's devices are not the other's, and one passing doesn't delete the other's folder.
	 */
	@Test
	void runsOfTwoJvmsWithOneTokenHaveFoldersOfTheirOwn() throws IOException {
		SailProfiles mine = profiles();
		SailProfiles theirs = profiles(OTHER_PID);
		Path myFolder = mine.give("sametoken", "phone1", KeepProfiles.FAILED, KEEP_HOURS);
		Path theirFolder = theirs.give("sametoken", "phone1", KeepProfiles.FAILED, KEEP_HOURS);
		Files.createDirectories(myFolder);
		Files.createDirectories(theirFolder);
		mine.endRun("sametoken", true);
		assertAll(() -> assertNotEquals(myFolder, theirFolder, "each JVM's own folder"),
					() -> assertEquals("sametoken-" + OTHER_PID, theirFolder.getParent().getFileName().toString(), "<token>-<pid>"),
					() -> assertFalse(Files.exists(myFolder.getParent()), "the passed run's folder is deleted"),
					() -> assertTrue(Files.isDirectory(theirFolder), "the other JVM's run of the same token is left"));
	}

	/**
	 * The sweep, before the first folder is given out, deletes the run folders past the keep time by both their token and
	 * their newest file that no live browser has open, and never this JVM's runs.
	 */
	@Test
	@DisabledOnOs(OS.WINDOWS)
	void theSweepDeletesOnlyOldUnusedRunFolders() throws IOException, InterruptedException {
		Path old = run(other(OLD), "phone1");
		age(old, OLD);
		Path freshFile = run(other(OLD.minusSeconds(1)), "phone1");
		age(freshFile, OLD);
		Files.setLastModifiedTime(freshFile.resolve("phone1").resolve("Default").resolve("Preferences"), FileTime.from(RECENT));
		Path recentToken = run(other(RECENT), "phone1");
		age(recentToken, OLD);
		Path inUse = run(other(OLD.minusSeconds(2)), "phone1");
		run(other(OLD.minusSeconds(2)), "phone2");
		lock(inUse, "phone2", ProcessHandle.current().pid());
		age(inUse, OLD);
		Path deadLock = run(other(OLD.minusSeconds(3)), "phone1");
		lock(deadLock, "phone1", deadPid());
		age(deadLock, OLD);
		String current = token(OLD.minusSeconds(6));
		Path currentFolder = run(current + '-' + PID, "phone1");
		age(currentFolder, OLD);

		SailProfiles profiles = profiles();
		profiles.give(current, "phone1", KeepProfiles.FAILED, KEEP_HOURS); // this JVM's run
		Path later = run(other(OLD.minusSeconds(7)), "phone1");
		age(later, OLD);
		profiles.give("anotherrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS); // the sweep ran already

		assertAll(() -> assertFalse(Files.exists(old, LinkOption.NOFOLLOW_LINKS), "an old run is deleted"),
					() -> assertTrue(Files.isDirectory(freshFile), "an old token with a recent file is kept"),
					() -> assertTrue(Files.isDirectory(recentToken), "a recent token with old files is kept"),
					() -> assertTrue(Files.isDirectory(inUse), "a run whose lock names a live process is kept"),
					() -> assertFalse(Files.exists(deadLock, LinkOption.NOFOLLOW_LINKS), "a run whose lock names a dead process is deleted"),
					() -> assertTrue(Files.isDirectory(currentFolder.resolve("phone1")), "this JVM's run is never swept"),
					() -> assertTrue(Files.isDirectory(later), "the sweep runs once"),
					() -> assertEquals(List.of(), messages, "no warnings"));
	}

	/**
	 * The sweep deletes only run folders: a real folder named {@code <run token>-<pid>} with the marker in it. Anything
	 * else under the root is left alone, a link is never followed, and a folder a delete left halfway is deleted.
	 */
	@Test
	@DisabledOnOs(OS.WINDOWS)
	void theSweepLeavesAloneWhatIsNotARunFolder() throws IOException, InterruptedException {
		Path unmarked = unmarked(other(OLD), "phone1");
		age(unmarked, OLD);
		Path tokenOnly = run(token(OLD.minusSeconds(1)), "phone1");
		age(tokenOnly, OLD);
		Path notARun = run("not-a-run", "phone1");
		age(notARun, OLD);
		Path tooShort = run("abc123-" + OTHER_PID, "phone1");
		age(tooShort, OLD);
		Path outside = Files.createDirectories(directory.resolve("outside"));
		Files.writeString(outside.resolve("precious"), "keep me");
		Files.writeString(outside.resolve(SailProfiles.MARKER), "");
		age(outside, OLD);
		Path link = Files.createSymbolicLink(root().resolve(other(OLD.minusSeconds(4))), outside);
		age(link, OLD);
		Path linkInside = run(other(OLD.minusSeconds(5)), "phone1");
		Files.createSymbolicLink(linkInside.resolve("phone1").resolve("elsewhere"), outside);
		age(linkInside, OLD);
		Path halfDeleted = run(other(OLD.minusSeconds(6)) + ".deleting-" + OTHER_PID, "phone1");
		age(halfDeleted, OLD);

		profiles().give("anotherrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS);

		assertAll(() -> assertTrue(Files.isDirectory(unmarked.resolve("phone1")), "a folder named like a run folder without the marker is left alone"),
					() -> assertTrue(Files.isDirectory(tokenOnly), "a folder named like a run token alone is left alone"),
					() -> assertTrue(Files.isDirectory(notARun), "a folder not named like a run folder is left alone"),
					() -> assertTrue(Files.isDirectory(tooShort), "a name too short for a run token is left alone"),
					() -> assertTrue(Files.isSymbolicLink(link), "a link named like a run folder is left alone"),
					() -> assertFalse(Files.exists(linkInside, LinkOption.NOFOLLOW_LINKS), "an old run with a link in it is deleted"),
					() -> assertEquals("keep me", Files.readString(outside.resolve("precious")), "what a link points to is never touched"),
					() -> assertFalse(Files.exists(halfDeleted, LinkOption.NOFOLLOW_LINKS), "a run folder a delete left halfway is deleted"),
					() -> assertEquals(List.of(), messages, "no warnings"));
	}

	/** A sweep of a root that doesn't exist yet does nothing, and the run's folder is made with its marker. */
	@Test
	void theSweepOfARootThatDoesNotExistDoesNothing() {
		Path folder = profiles().give(token(NOW), "phone1", KeepProfiles.FAILED, KEEP_HOURS);
		assertAll(() -> assertEquals(root().resolve(token(NOW) + '-' + PID).resolve("phone1"), folder),
					() -> assertEquals(List.of(Path.of(SailProfiles.MARKER)), list(folder.getParent()), "only the marker; the browser makes the profile"),
					() -> assertEquals(List.of(), messages));
	}

	/**
	 * The query for a run's folder gives where {@code folder} puts it without making it, or sweeping the root, or noting
	 * the run to end.
	 */
	@Test
	void theRunFolderQueryMakesNothing() {
		SailSettings settings = SailSettings.resolve(new SailSettings.Environment(Map.of(), Map.of(), directory.resolve("home"), directory),
														List.of(SailSettings.code().profiles(root()).layer()));
		String run = token(NOW);
		Path queried = SailProfiles.runFolder(settings, run);
		boolean made = Files.exists(root());
		Path given = SailProfiles.folder(settings, run, "phone1");
		SailProfiles.end(run, true);
		assertAll(() -> assertEquals(root().resolve(run + '-' + ProcessHandle.current().pid()), queried, "<root>/<run>-<pid>"),
					() -> assertFalse(made, "nothing made"),
					() -> assertEquals(queried, given.getParent(), "where folder puts it"));
	}

	/**
	 * A folder that can't be deleted (on Windows, a profile a browser has open; here, a folder that can't be written)
	 * gets a warning naming it, whether a run ended or the sweep found it, and nothing fails. The run folder was
	 * renamed before the delete began, and keeps its marker, so a later sweep deletes the rest.
	 */
	@Test
	void aFolderThatCannotBeDeletedGetsAWarningNotAFailure() throws IOException, InterruptedException {
		assumeTrue(Files.getFileAttributeView(directory, PosixFileAttributeView.class) != null, "needs POSIX permissions");
		Path old = run(other(OLD), "phone1");
		age(old, OLD);
		Path locked = old.resolve("phone1").resolve("Default");
		Path passed = run("passedrun-" + PID, "phone1");
		Path lockedToo = passed.resolve("phone1").resolve("Default");
		Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("r-xr-xr-x"));
		Files.setPosixFilePermissions(lockedToo, PosixFilePermissions.fromString("r-xr-xr-x"));
		try {
			assumeFalse(Files.isWritable(locked), "a user that file permissions bind (not root)");
			SailProfiles profiles = profiles();
			profiles.give("passedrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS);
			profiles.endRun("passedrun", true);
			Path oldRenamed = root().resolve(old.getFileName() + ".deleting-" + PID);
			Path passedRenamed = root().resolve(passed.getFileName() + ".deleting-" + PID);
			assertAll(() -> assertEquals(2, messages.size(), messages.toString()),
						() -> assertTrue(messages.get(0).startsWith("SAIL device profiles: could not delete " + oldRenamed + ": java.nio.file.AccessDeniedException"),
											messages.get(0)),
						() -> assertTrue(messages.get(1).startsWith("SAIL device profiles: could not delete " + passedRenamed + ": java.nio.file.AccessDeniedException"),
											messages.get(1)),
						() -> assertTrue(Files.isDirectory(oldRenamed.resolve("phone1").resolve("Default")), "the old run is left for a later sweep"),
						() -> assertTrue(Files.isRegularFile(oldRenamed.resolve(SailProfiles.MARKER)), "with its marker"),
						() -> assertTrue(Files.isDirectory(passedRenamed.resolve("phone1").resolve("Default")), "and the passed run too"));
		}
		finally {
			makeWritable(directory);
		}
	}

	/**
	 * A run folder is renamed before anything in it is deleted, so a folder that can't be renamed (on Windows, a profile
	 * a browser has open; here, under a root that can't be written) is left whole, not partly deleted.
	 */
	@Test
	void aRunFolderThatCannotBeRenamedIsLeftWhole() throws IOException {
		assumeTrue(Files.getFileAttributeView(directory, PosixFileAttributeView.class) != null, "needs POSIX permissions");
		SailProfiles profiles = profiles();
		Path passed = profiles.give("passedrun", "phone1", KeepProfiles.FAILED, KEEP_HOURS).getParent();
		run(passed.getFileName().toString(), "phone1");
		Files.setPosixFilePermissions(root(), PosixFilePermissions.fromString("r-xr-xr-x"));
		try {
			assumeFalse(Files.isWritable(root()), "a user that file permissions bind (not root)");
			profiles.endRun("passedrun", true);
			assertAll(() -> assertEquals(1, messages.size(), messages.toString()),
						() -> assertTrue(messages.get(0).startsWith("SAIL device profiles: could not delete " + passed + ": "), messages.get(0)),
						() -> assertTrue(Files.isRegularFile(passed.resolve("phone1").resolve("Default").resolve("Preferences")), "nothing in it is deleted"));
		}
		finally {
			makeWritable(directory);
		}
	}

	/** Lets the owner write every folder under a folder again, so the temporary folder can be deleted. */
	private static void makeWritable(Path folder) throws IOException {
		List<Path> folders;
		try (Stream<Path> walk = Files.walk(folder)) {
			folders = walk.filter(f -> Files.isDirectory(f, LinkOption.NOFOLLOW_LINKS)).toList();
		}
		for (Path f : folders) {
			Files.setPosixFilePermissions(f, PosixFilePermissions.fromString("rwxr-xr-x"));
		}
	}

	/** Returns the names in a folder, in order. */
	private static List<Path> list(Path folder) throws IOException {
		try (Stream<Path> names = Files.list(folder)) {
			return names.map(Path::getFileName).sorted().toList();
		}
	}

	/** Returns the pid of a process that has ended. */
	private static long deadPid() throws IOException, InterruptedException {
		Process process = new ProcessBuilder("true").start();
		process.waitFor();
		return process.pid();
	}
}

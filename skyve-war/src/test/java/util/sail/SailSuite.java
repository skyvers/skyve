package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import org.junit.jupiter.api.function.Executable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Runs a SAIL suite: an ordered set of script files in one browser session, bracketed by set up files (e.g. a
 * precondition check and a backup) and a restore file, for the admin suites of each renderer.
 * <ul>
 * <li>The set up files always run first, in order, then the suite's files.
 * 		{@value #ONLY_PROPERTY}{@code =03,04} runs only the suite files with those prefixes (the set up and restore still run).</li>
 * <li>The run stops at the first file that fails, as later files build on earlier ones.</li>
 * <li>The restore file runs last, even after a failure, but only if each of the files it requires has passed:
 * 		for example the backup that the restore restores, as the restore takes the first (newest) backup listed.
 * 		This is decided by which files passed, not by failure message text.</li>
 * <li>The test strategy override ({@value InterpretedWebDriverExecutor#TEST_STRATEGY_PROPERTY}) is refused: under Verify a
 * 		failed precondition would not stop the suite, and under None nothing would be checked before the restore.</li>
 * </ul>
 * The test then fails with every outcome (the first failing file and the restore's, if either failed).
 */
public final class SailSuite {
	/** System property naming the suite files (by prefix, comma separated) to run, e.g. {@code 03,04}. */
	public static final String ONLY_PROPERTY = "sail.suite.only";

	private final String folder;
	private final List<String> setUp = new ArrayList<>();
	private final List<String> files = new ArrayList<>();
	private String restore;
	private final List<String> restoreRequires = new ArrayList<>();

	/**
	 * Creates a suite of the scripts in a test classpath folder.
	 *
	 * @param folder	the folder, ending with {@code /}, e.g. {@code sail/admin/pf/suite/}
	 */
	public SailSuite(@Nonnull String folder) {
		this.folder = folder;
	}

	/**
	 * Sets the files that always run first, in order.
	 *
	 * @param names	the file names in the folder
	 * @return this suite
	 */
	public @Nonnull SailSuite setUp(@Nonnull String... names) {
		setUp.addAll(Arrays.asList(names));
		return this;
	}

	/**
	 * Sets the suite's files, in order.
	 *
	 * @param names	the file names in the folder
	 * @return this suite
	 */
	public @Nonnull SailSuite files(@Nonnull String... names) {
		files.addAll(Arrays.asList(names));
		return this;
	}

	/**
	 * Sets the file that runs last, even after a failure, provided each of the required files has passed.
	 *
	 * @param name	the restore file name in the folder
	 * @param requires	the set up files that must have passed for the restore to run
	 * @return this suite
	 */
	public @Nonnull SailSuite restore(@Nonnull String name, @Nonnull String... requires) {
		restore = name;
		restoreRequires.addAll(Arrays.asList(requires));
		return this;
	}

	/**
	 * Runs the suite.
	 *
	 * @param sailResource	runs one script, given its test classpath resource name
	 * @throws AssertionError	listing the first failing file and the restore's failure, if any
	 * @throws IllegalStateException	if the test strategy override is set
	 */
	public void run(@Nonnull Consumer<String> sailResource) {
		refuseTestStrategyOverride("A SAIL admin suite");

		String only = System.getProperty(ONLY_PROPERTY);
		List<String> prefixes = (only == null) ? List.of() :
				Arrays.stream(only.split(",")).map(String::trim).filter(prefix -> ! prefix.isEmpty()).toList();
		List<String> toRun = new ArrayList<>(setUp);
		for (String file : files) {
			if (prefixes.isEmpty() || prefixes.stream().anyMatch(file::startsWith)) {
				toRun.add(file);
			}
		}

		List<Executable> outcomes = new ArrayList<>(2);
		Set<String> passed = new HashSet<>();
		for (String file : toRun) {
			Throwable failure = runFile(sailResource, file);
			if (failure != null) {
				outcomes.add(failed(file, failure));
				break;
			}
			passed.add(file);
		}

		if (restore != null) {
			if (passed.containsAll(restoreRequires)) {
				Throwable failure = runFile(sailResource, restore);
				if (failure != null) {
					outcomes.add(failed(restore, failure));
				}
			}
			else {
				System.out.println("SAIL suite: " + restore + " held back, as " + restoreRequires + " did not all pass");
			}
		}

		assertAll(outcomes);
	}

	private @Nullable Throwable runFile(@Nonnull Consumer<String> sailResource, @Nonnull String file) {
		SailMedia.label(file); // names the file's snapshot and video folders, when they are on
		try {
			sailResource.accept(folder + file);
			return null;
		}
		catch (AssertionError | RuntimeException e) { // a step failure, or a missing or invalid script
			return e;
		}
		finally {
			SailMedia.label(null);
		}
	}

	private static @Nonnull Executable failed(@Nonnull String file, @Nonnull Throwable failure) {
		return () -> {
			throw new AssertionError(file + " failed: " + failure.getMessage(), failure);
		};
	}

	/**
	 * Fails fast if the test strategy override ({@value InterpretedWebDriverExecutor#TEST_STRATEGY_PROPERTY}) is set,
	 * for tests that it would weaken or break.
	 *
	 * @param what	what refuses it, for the message
	 * @throws IllegalStateException	if the property is set
	 */
	public static void refuseTestStrategyOverride(@Nonnull String what) {
		String override = System.getProperty(InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY);
		if ((override != null) && (! override.isBlank())) {
			throw new IllegalStateException(String.format("%s runs with its scripts' own test strategies: unset -D%s (it is %s)",
															what, InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY, override));
		}
	}
}

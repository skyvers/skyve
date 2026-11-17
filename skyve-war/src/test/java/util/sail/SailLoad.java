package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntFunction;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

import org.junit.jupiter.api.function.Executable;
import org.opentest4j.MultipleFailuresError;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.util.XMLMetaData.SAILRoot;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.SailLoadReport.Iteration;
import util.sail.SailLoadReport.Recorder;
import util.sail.SailLoadReport.Sample;
import util.sail.SailLoadReport.Violation;

/**
 * Runs SAIL as many virtual users at once, for a load or stress test: a composable utility that any test can use, whatever
 * it extends, e.g.
 *
 * <pre>
 * SailLoad.users(10).rampUp(Duration.ofSeconds(30)).forDuration(Duration.ofMinutes(5))
 * 		.settings(settings()) // the test's code-level settings, if it has them
 * 		.run("sail/kitchenSink/workflow/kitchensink-workflow.xml");
 * </pre>
 *
 * <ul>
 * <li><b>Virtual users.</b> Each is a thread of its own with its own browser (its own {@link SailWorkflow} engine, with the
 * 		test's settings, the view roles given here and the browsers given here), and runs iterations one after another.
 * 		An iteration runs every entry of {@link #run(String...)} in order (a {@code <suite>}, a {@code <workflow>} or an
 * 		{@code <automation>}, as {@code SailRunIT} loads them), or a {@link VirtualUserTask} of custom Java. Each workflow
 * 		run, and so each iteration, has its own run token, so {@code {run}} data never collides between users or
 * 		iterations. A user's thread gets a persistence with the test thread's user, as the test bases set it up, so the
 * 		executors build views as they do on the test thread.</li>
 * <li><b>Load shape.</b> The users start spread evenly over the ramp-up: user {@code n} of {@code N} starts
 * 		{@code rampUp * (n - 1) / N} after the first. The run starts new iterations until its duration (from its start,
 * 		ramp-up included) ends, or until each user has run its iterations, whichever comes first; with neither, each
 * 		user runs one. Iterations in flight at the end finish, up to the time limit; then the users still running are
 * 		interrupted, and the run fails.</li>
 * <li><b>The summary</b>, printed to the trace and written as HTML and CSV under {@code target/sail/load/}: per
 * 		interaction (by name across users) its count, failures and median, 95th percentile and longest durations; per
 * 		user its iterations passed and failed; the throughput; and the first failure of each kind. The executor times
 * 		each interaction as it runs it (see {@link SailLoadProbe}).</li>
 * <li><b>The verdict.</b> The test fails, with {@code assertAll}, if any iteration failed, unless the failure rate is
 * 		within {@value SailSettings#LOAD_MAX_FAILURE_RATE}; and if any interaction's 95th percentile is above
 * 		{@value SailSettings#LOAD_MAX_P95}, for a performance gate.</li>
 * <li><b>Settings.</b> The load settings ({@value SailSettings#LOAD_USERS}, {@value SailSettings#LOAD_RAMP_UP},
 * 		{@value SailSettings#LOAD_DURATION}, {@value SailSettings#LOAD_ITERATIONS}, {@value SailSettings#LOAD_TIME_LIMIT},
 * 		{@value SailSettings#LOAD_MAX_FAILURE_RATE}, {@value SailSettings#LOAD_MAX_P95}) resolve as every SAIL setting
 * 		does: this class's methods are the code level, so {@code -D} and the environment beat them.</li>
 * <li><b>Suites with a bracket</b> (set-ups or a restore, such as the admin suite's backup and restore) are refused, as
 * 		each virtual user would run the bracket at the same time as the others, unless {@link #allowSuiteBracket()} says
 * 		the bracket is safe to run concurrently. Load workflows and suites without a bracket.</li>
 * </ul>
 * The trace of each user's thread is prefixed with the user (e.g. {@code [user 03] }) while the run lasts. Evidence
 * (failure screenshots, media) names its files and folders for the user and iteration, as the users share the
 * directories. The server lock ({@link SailServerLock}) is taken once for the JVM, so the users share it, as a load run
 * should.
 * <p>
 * The scripts must be safe to run concurrently against one server: a script that changes shared data (a user's own
 * details, a sign in with a wrong password that locks the account for everyone) makes the users fail each other.
 * <p>
 * Not thread-safe: a test configures it on its own thread, and must not change the shared settings while it runs.
 */
public final class SailLoad {
	/** How long a run with a duration may take beyond it, by default, for the iterations in flight at its end. */
	static final Duration DEFAULT_FINISH = Duration.ofMinutes(5);
	/** Where the reports go, under the working directory. */
	static final Path REPORT_DIR = Path.of("target", "sail", "load");
	private static final DateTimeFormatter FOLDER_TIME = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS");
	/** The virtual user of this thread, for the trace's prefix; null on other threads. */
	private static final ThreadLocal<String> TRACE_USER = new ThreadLocal<>();

	/**
	 * Custom Java that a virtual user runs in each iteration, with its own browser through {@link VirtualUser#workflow()}.
	 */
	@FunctionalInterface
	public interface VirtualUserTask {
		/**
		 * Runs one iteration.
		 *
		 * @param user	the virtual user, with its iteration and engine
		 * @throws Exception	if the iteration fails; so does an {@link AssertionError}
		 */
		@SuppressWarnings("java:S112") // a task's own Java may throw anything: any exception fails the iteration
		void run(@Nonnull VirtualUser user) throws Exception;
	}

	/**
	 * Work that {@link VirtualUser#measure(String, Measured)} times.
	 */
	@FunctionalInterface
	public interface Measured {
		/**
		 * Does the work.
		 *
		 * @throws Exception	if it fails
		 */
		@SuppressWarnings("java:S112") // a task's own Java may throw anything
		void run() throws Exception;
	}

	/**
	 * A virtual user, as its task sees it in an iteration.
	 */
	public static final class VirtualUser {
		private final int number;
		private final SailWorkflow workflow;
		private final LongSupplier clock;
		private final Recorder recorder;
		private int iteration;

		VirtualUser(int number, @Nonnull SailWorkflow workflow, @Nonnull LongSupplier clock, @Nonnull Recorder recorder) {
			this.number = number;
			this.workflow = workflow;
			this.clock = clock;
			this.recorder = recorder;
		}

		/**
		 * Returns the user's number.
		 *
		 * @return from 1
		 */
		public int number() {
			return number;
		}

		/**
		 * Returns the user's name, as the summary and the trace show it.
		 *
		 * @return e.g. {@code user 03}
		 */
		public @Nonnull String name() {
			return userName(number);
		}

		/**
		 * Returns the iteration the user is running.
		 *
		 * @return from 1
		 */
		public int iteration() {
			return iteration;
		}

		/**
		 * Returns the user's own engine, to run SAIL with its own browser; each run has its own run token.
		 *
		 * @return the engine
		 */
		public @Nonnull SailWorkflow workflow() {
			return workflow;
		}

		/**
		 * Times work as an interaction of the summary, as the executor times each SAIL interaction.
		 *
		 * @param interaction	the name the summary shows it by
		 * @param work	the work
		 * @throws Exception	what the work throws, after it is noted as failed
		 */
		@SuppressWarnings("java:S112") // rethrows what the work throws
		public void measure(@Nonnull String interaction, @Nonnull Measured work) throws Exception {
			long started = clock.getAsLong();
			boolean passed = false;
			try {
				work.run();
				passed = true;
			}
			finally {
				recorder.sample(new Sample(interaction, number, iteration, started, clock.getAsLong(), ! passed, null));
			}
		}
	}

	/**
	 * Starts and waits for the virtual users; replaced in tests so that no real time passes.
	 */
	interface Scheduler {
		/**
		 * Starts a virtual user after a delay.
		 *
		 * @param user	runs the user's iterations
		 * @param delay	how long after the run's start
		 */
		void schedule(@Nonnull Runnable user, @Nonnull Duration delay);

		/**
		 * Waits for every user to end.
		 *
		 * @param limit	the longest to wait, or null for no limit
		 * @return true if they all ended, false if the limit passed first
		 * @throws InterruptedException	if the waiting thread is interrupted
		 */
		boolean awaitEnd(@Nullable Duration limit) throws InterruptedException;

		/**
		 * Interrupts the users still running, and waits for them to end.
		 *
		 * @param limit	the longest to wait
		 * @return true if they all ended
		 * @throws InterruptedException	if the waiting thread is interrupted
		 */
		boolean stop(@Nonnull Duration limit) throws InterruptedException;
	}

	/**
	 * The scheduler of a real run: a scheduled thread pool with a thread for each virtual user.
	 */
	static final class ExecutorScheduler implements Scheduler {
		private final ScheduledExecutorService executor;

		/**
		 * Creates the pool.
		 *
		 * @param users	how many threads
		 */
		ExecutorScheduler(int users) {
			AtomicInteger count = new AtomicInteger();
			ThreadFactory threads = task -> new Thread(task, "sail-load-" + count.incrementAndGet());
			executor = Executors.newScheduledThreadPool(users, threads);
		}

		@Override
		public void schedule(@Nonnull Runnable user, @Nonnull Duration delay) {
			executor.schedule(user, delay.toNanos(), TimeUnit.NANOSECONDS);
		}

		@Override
		public boolean awaitEnd(@Nullable Duration limit) throws InterruptedException {
			executor.shutdown(); // the users already scheduled still start
			// the run's end: every user has ended its last iteration
			return executor.awaitTermination((limit == null) ? Long.MAX_VALUE : limit.toNanos(), TimeUnit.NANOSECONDS);
		}

		@Override
		public boolean stop(@Nonnull Duration limit) throws InterruptedException {
			executor.shutdownNow();
			// the interrupted users' ends: each blocked wait ends within the settings' timeout
			return executor.awaitTermination(limit.toNanos(), TimeUnit.NANOSECONDS);
		}
	}

	/**
	 * A run's shape and thresholds, from the settings.
	 *
	 * @param users	how many virtual users
	 * @param rampUp	how long they take to start
	 * @param duration	how long new iterations start for, or null
	 * @param iterations	how many iterations each user runs, or 0 for as many as the duration allows
	 * @param timeLimit	the longest the run may take, or null for none
	 * @param maxFailureRate	the percentage of iterations that may fail
	 * @param maxP95	the longest 95th percentile an interaction may have, or null
	 * @param stopLimit	how long interrupted users may take to end
	 */
	record Plan(int users,
				@Nonnull Duration rampUp,
				@Nullable Duration duration,
				int iterations,
				@Nullable Duration timeLimit,
				double maxFailureRate,
				@Nullable Duration maxP95,
				@Nonnull Duration stopLimit) {
		/**
		 * Makes the plan of resolved settings.
		 *
		 * @param settings	the settings
		 * @return the plan
		 * @throws IllegalArgumentException	if there are no users, the ramp-up is not shorter than the duration, or the
		 * 			time limit is not longer than it
		 */
		static @Nonnull Plan of(@Nonnull SailSettings settings) {
			int users = settings.loadUsers();
			if (users == 0) {
				throw new IllegalArgumentException("SAIL load: set the number of virtual users, with SailLoad.users(n), -D" +
													SailSettings.LOAD_USERS + " or " + SailSettings.environmentName(SailSettings.LOAD_USERS));
			}
			Duration rampUp = settings.loadRampUp();
			Duration duration = settings.loadDuration();
			Duration timeLimit = settings.loadTimeLimit();
			if (duration != null) {
				if (rampUp.compareTo(duration) >= 0) {
					throw new IllegalArgumentException("SAIL load: the ramp-up " + described(settings, SailSettings.LOAD_RAMP_UP) +
														" must be shorter than the duration " + described(settings, SailSettings.LOAD_DURATION) +
														", which includes it");
				}
				if (timeLimit == null) {
					timeLimit = duration.plus(DEFAULT_FINISH);
				}
				else if (timeLimit.compareTo(duration) <= 0) {
					throw new IllegalArgumentException("SAIL load: the time limit " + described(settings, SailSettings.LOAD_TIME_LIMIT) +
														" must be longer than the duration " + described(settings, SailSettings.LOAD_DURATION));
				}
			}
			int iterations = settings.loadIterations();
			if ((iterations == 0) && (duration == null)) {
				iterations = 1;
			}
			return new Plan(users, rampUp, duration, iterations, timeLimit, settings.loadMaxFailureRate(), settings.loadMaxP95(),
							settings.timeout());
		}

		/** Returns a setting as {@code value (source)}. */
		private static @Nonnull String described(@Nonnull SailSettings settings, @Nonnull String key) {
			SailSettings.Setting setting = settings.setting(key);
			return (setting == null) ? "(default)" : setting.value() + " (" + setting.source() + ')';
		}

		/**
		 * Returns how long after the run's start a user starts: the ramp-up spread evenly over the users.
		 *
		 * @param user	from 1
		 * @return the delay
		 */
		@Nonnull Duration delay(int user) {
			return rampUp.multipliedBy(user - 1L).dividedBy(users);
		}

		/** Describes the plan for the trace. */
		@Override
		public @Nonnull String toString() {
			StringBuilder result = new StringBuilder(128).append(users).append(" virtual users");
			if (! rampUp.isZero()) {
				result.append(", ramp-up ").append(rampUp);
			}
			if (duration != null) {
				result.append(", duration ").append(duration);
			}
			if (iterations > 0) {
				result.append(", ").append(iterations).append(" iteration(s) each");
			}
			if (timeLimit != null) {
				result.append(", time limit ").append(timeLimit);
			}
			if (maxFailureRate > 0.0) {
				result.append(", max failure rate ").append(maxFailureRate).append('%');
			}
			if (maxP95 != null) {
				result.append(", max p95 ").append(maxP95);
			}
			return result.toString();
		}
	}

	/**
	 * A file to run in each iteration, and what it is.
	 *
	 * @param path	a file path or a classpath resource
	 * @param file	true for a file
	 * @param root	what kind of SAIL document it is
	 */
	private record Entry(@Nonnull String path, boolean file, @Nonnull SAILRoot root) {
		// a value
	}

	/** The load settings given by this class's methods. */
	private final SailSettings.Code load = SailSettings.code();
	/** The settings the engines run with: the test's, if given. */
	private SailSettings.Code settings = load;
	private final List<String[]> viewRoles = new ArrayList<>();
	private final Map<UserAgentType, Supplier<BrowserConfiguration>> browsers = new EnumMap<>(UserAgentType.class);
	private boolean allowBracket;
	private LongSupplier clock = System::nanoTime;
	private IntFunction<Scheduler> schedulers = ExecutorScheduler::new;
	private SailSettings.Environment environment;
	private Path reportDirectory;
	/** False for tests whose fake users run on the test's own thread, which must keep its persistence. */
	private boolean userPersistence = true;

	private SailLoad() {
		// see the factories
	}

	/**
	 * Starts a load run with no load settings of its own: the settings give them (e.g. {@code -Dsail.load.users=5}).
	 *
	 * @return the run, to configure
	 */
	public static @Nonnull SailLoad create() {
		return new SailLoad();
	}

	/**
	 * Starts a load run of a number of virtual users.
	 *
	 * @param users	how many, above 0
	 * @return the run, to configure
	 */
	public static @Nonnull SailLoad users(int users) {
		SailLoad result = new SailLoad();
		result.load.set(SailSettings.LOAD_USERS, String.valueOf(users));
		return result;
	}

	/**
	 * Sets how long the virtual users take to start, spread evenly.
	 *
	 * @param rampUp	zero or more; shorter than the duration
	 * @return this run
	 */
	public @Nonnull SailLoad rampUp(@Nonnull Duration rampUp) {
		load.set(SailSettings.LOAD_RAMP_UP, rampUp.toString());
		return this;
	}

	/**
	 * Sets how long new iterations start for, from the run's start (ramp-up included).
	 *
	 * @param duration	above zero
	 * @return this run
	 */
	public @Nonnull SailLoad forDuration(@Nonnull Duration duration) {
		load.set(SailSettings.LOAD_DURATION, duration.toString());
		return this;
	}

	/**
	 * Sets how many iterations each virtual user runs (within the duration, if one is set too).
	 *
	 * @param iterations	above 0
	 * @return this run
	 */
	public @Nonnull SailLoad iterations(int iterations) {
		load.set(SailSettings.LOAD_ITERATIONS, String.valueOf(iterations));
		return this;
	}

	/**
	 * Sets the longest the run may take, iterations in flight at its end included.
	 *
	 * @param timeLimit	above zero; longer than the duration
	 * @return this run
	 */
	public @Nonnull SailLoad timeLimit(@Nonnull Duration timeLimit) {
		load.set(SailSettings.LOAD_TIME_LIMIT, timeLimit.toString());
		return this;
	}

	/**
	 * Lets some iterations fail without failing the test.
	 *
	 * @param percent	the percentage of iterations that may fail, 0 to 100
	 * @return this run
	 */
	public @Nonnull SailLoad maxFailureRate(double percent) {
		load.set(SailSettings.LOAD_MAX_FAILURE_RATE, String.valueOf(percent));
		return this;
	}

	/**
	 * Fails the test if any interaction's 95th percentile duration is above a limit.
	 *
	 * @param p95	the limit, above zero
	 * @return this run
	 */
	public @Nonnull SailLoad maxP95(@Nonnull Duration p95) {
		load.set(SailSettings.LOAD_MAX_P95, p95.toString());
		return this;
	}

	/**
	 * Returns the code-level settings the virtual users run with: this run's own, unless
	 * {@link #settings(SailSettings.Code)} gave the test's.
	 *
	 * @return the settings builder
	 */
	public @Nonnull SailSettings.Code settings() {
		return settings;
	}

	/**
	 * Runs the virtual users with a test's code-level settings, such as a test base's {@code settings()}. The load
	 * settings given to this run's own methods beat the test's.
	 *
	 * @param code	the test's settings; shared, so they must not change while the run lasts
	 * @return this run
	 */
	public @Nonnull SailLoad settings(@Nonnull SailSettings.Code code) {
		settings = code;
		return this;
	}

	/**
	 * Builds the views for a user that the scripts sign in as with the user's roles (see {@link SailViewRoles}), in
	 * every virtual user.
	 *
	 * @param userName	the user name as a script signs in with it; may contain {@code {run}}
	 * @param roleNames	the user's roles
	 * @return this run
	 * @throws IllegalArgumentException	if there are no role names, or one is blank
	 */
	public @Nonnull SailLoad viewRoles(@Nonnull String userName, @Nonnull String... roleNames) {
		new SailViewRoles().register(userName, roleNames); // checks them now
		String[] registration = new String[roleNames.length + 1];
		registration[0] = userName;
		System.arraycopy(roleNames, 0, registration, 1, roleNames.length);
		viewRoles.add(registration);
		return this;
	}

	/**
	 * Sets the browser for the automations of a user agent type, in every virtual user (see
	 * {@link SailWorkflow#browser(UserAgentType, Supplier)}).
	 *
	 * @param userAgentType	the automations' user agent type
	 * @param configuration	makes the browser's configuration; called on the users' threads, so it must be thread-safe
	 * @return this run
	 */
	public @Nonnull SailLoad browser(@Nonnull UserAgentType userAgentType, @Nonnull Supplier<BrowserConfiguration> configuration) {
		browsers.put(userAgentType, configuration);
		return this;
	}

	/**
	 * Lets a suite with a bracket (set-ups or a restore) run, which each virtual user runs at the same time as the others:
	 * only for a bracket that is safe to run concurrently.
	 *
	 * @return this run
	 */
	public @Nonnull SailLoad allowSuiteBracket() {
		allowBracket = true;
		return this;
	}

	/** Uses another clock, in nanoseconds, for tests. */
	@Nonnull SailLoad clock(@Nonnull LongSupplier nanos) {
		clock = nanos;
		return this;
	}

	/** Uses another scheduler, made for the number of users, for tests. */
	@Nonnull SailLoad scheduler(@Nonnull IntFunction<Scheduler> factory) {
		schedulers = factory;
		return this;
	}

	/** Resolves the settings from the given sources instead of this JVM's, for tests. */
	@Nonnull SailLoad environment(@Nonnull SailSettings.Environment sources) {
		environment = sources;
		return this;
	}

	/** Gives the users' threads no persistence of their own, for tests whose fake users run on the test's thread. */
	@Nonnull SailLoad withoutUserPersistence() {
		userPersistence = false;
		return this;
	}

	/** Writes the report into the given directory instead of under the working directory's {@code target/sail/load}, for tests. */
	@Nonnull SailLoad reportDirectory(@Nonnull Path directory) {
		reportDirectory = directory;
		return this;
	}

	/**
	 * Runs SAIL files in each iteration of each virtual user, in order. Each is a {@code <suite>}, a {@code <workflow>} or
	 * an {@code <automation>}; a file path that exists, else a classpath resource. The entries of an iteration are
	 * independent: each runs, and the iteration fails if any failed.
	 *
	 * @param resourcesOrFiles	the files, e.g. {@code sail/kitchenSink/kitchensink.suite.xml}
	 * @throws IllegalArgumentException	before any browser starts, if a file is missing or invalid, a suite has a
	 * 			bracket that is not allowed, or a setting is invalid
	 * @throws AssertionError	after the summary, with each reason the run failed
	 */
	public void run(@Nonnull String... resourcesOrFiles) {
		if (resourcesOrFiles.length == 0) {
			throw new IllegalArgumentException("SAIL load: name at least one SAIL file to run");
		}
		List<Entry> entries = new ArrayList<>(resourcesOrFiles.length);
		for (String path : resourcesOrFiles) {
			entries.add(entry(path));
		}
		verdict(execute(String.join(", ", resourcesOrFiles), user -> runEntries(user, entries)));
	}

	/**
	 * Runs custom Java in each iteration of each virtual user.
	 *
	 * @param task	the iteration, e.g. {@code user -> user.workflow().runResources("sail/x.xml")}
	 * @throws IllegalArgumentException	before any user starts, if a setting is invalid
	 * @throws AssertionError	after the summary, with each reason the run failed
	 */
	public void run(@Nonnull VirtualUserTask task) {
		verdict(execute("task", task));
	}

	private static void verdict(@Nonnull SailLoadReport report) {
		List<Executable> failures = new ArrayList<>();
		for (Violation violation : report.violations()) {
			failures.add(() -> {
				throw new AssertionError(violation.message(), violation.cause());
			});
		}
		assertAll(report.headline(), failures);
	}

	/**
	 * Works out what a path is, and checks that a suite has no bracket unless it is allowed.
	 *
	 * @throws IllegalArgumentException	if it is missing, or a suite's bracket is not allowed
	 */
	private @Nonnull Entry entry(@Nonnull String path) {
		boolean file = Files.isRegularFile(Path.of(path));
		Suite suite = null;
		SAILRoot root;
		if (file) {
			root = XMLMetaData.sailRootOfFile(path);
			if (root == SAILRoot.SUITE) {
				suite = XMLMetaData.unmarshalSAILSuiteFile(path);
			}
		}
		else {
			String xml = resource(path);
			root = XMLMetaData.sailRootOfString(xml);
			if (root == SAILRoot.SUITE) {
				suite = XMLMetaData.unmarshalSAILSuiteString(xml, path);
			}
		}
		if ((suite != null) && (! allowBracket) && ((! suite.getSetUps().isEmpty()) || (suite.getRestore() != null))) {
			throw new IllegalArgumentException("SAIL load: the suite " + path + " has a bracket (set-ups or a restore, such as a " +
												"backup and restore), which every virtual user would run at the same time as " +
												"the others. Load workflows or suites without a bracket, or call " +
												"allowSuiteBracket() if its bracket is safe to run concurrently.");
		}
		return new Entry(path, file, root);
	}

	private static @Nonnull String resource(@Nonnull String path) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(path)) {
			if (stream == null) {
				throw new IllegalArgumentException("SAIL load: " + path + " is neither a file nor a resource on the test classpath");
			}
			return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
		}
		catch (IOException e) {
			throw new IllegalArgumentException("SAIL load: could not read the resource " + path, e);
		}
	}

	/**
	 * Runs an iteration's entries, each independent of the others.
	 *
	 * @throws AssertionError	or a RuntimeException, what the one failed entry threw; a {@link MultipleFailuresError}
	 * 			with each failure if several failed
	 */
	private static void runEntries(@Nonnull VirtualUser user, @Nonnull List<Entry> entries) {
		List<Throwable> failures = new ArrayList<>();
		SailWorkflow engine = user.workflow();
		for (Entry entry : entries) {
			try {
				if (entry.root() == SAILRoot.SUITE) {
					(entry.file() ? SailSuite.file(entry.path(), engine) : SailSuite.resource(entry.path(), engine)).run();
				}
				else if (entry.file()) {
					engine.runFiles(entry.path());
				}
				else {
					engine.runResources(entry.path());
				}
			}
			catch (AssertionError | RuntimeException e) { // a failed script; the next entry is independent
				failures.add(e);
			}
		}
		if (failures.size() == 1) {
			Throwable only = failures.get(0);
			if (only instanceof RuntimeException e) {
				throw e;
			}
			throw (AssertionError) only;
		}
		if (! failures.isEmpty()) {
			throw new MultipleFailuresError("SAIL load: " + failures.size() + " of " + entries.size() + " entries failed", failures);
		}
	}

	/**
	 * Runs the virtual users and reports what they did.
	 *
	 * <p>Side effects: starts threads, and on them browsers; prints the summary; writes the report; prefixes each
	 * user thread's trace while the run lasts.
	 *
	 * @param name	what runs, for the summary
	 * @param task	an iteration
	 * @return the report
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	@SuppressWarnings("resource") // the prefixing stream wraps standard output, which must stay open: it is put back, not closed
	@Nonnull SailLoadReport execute(@Nonnull String name, @Nonnull VirtualUserTask task) {
		SailSettings.Environment sources = (environment == null) ? SailSettings.Environment.current() : environment;
		List<SailSettings.Layer> layers = (settings == load) ? List.of(load.layer()) : List.of(load.layer(), settings.layer());
		Plan plan = Plan.of(SailSettings.resolve(sources, layers)); // an invalid setting fails here, before any user starts
		// the test thread's user, for each user thread's persistence, as the test bases set it up
		User testUser = (userPersistence && AbstractPersistence.isPresent()) ? AbstractPersistence.get().getUser() : null;
		Recorder recorder = new Recorder();
		AtomicBoolean stopping = new AtomicBoolean();
		List<String> problems = new ArrayList<>();
		PrintStream out = System.out;
		out.println("SAIL load " + name + ": " + plan);
		long start = clock.getAsLong();
		Scheduler scheduler = schedulers.apply(plan.users());
		System.setOut(new UserPrefixedStream(out));
		try {
			for (int user = 1; user <= plan.users(); user++) {
				int number = user;
				scheduler.schedule(() -> runUser(number, plan, start, task, recorder, stopping, testUser), plan.delay(number));
			}
			Duration timeLimit = plan.timeLimit();
			if (! scheduler.awaitEnd(timeLimit)) {
				stopping.set(true);
				problems.add("the time limit of " + timeLimit + " (" + SailSettings.LOAD_TIME_LIMIT + ") passed: the users still running were interrupted");
				if (! scheduler.stop(plan.stopLimit())) {
					problems.add("some users did not stop within " + plan.stopLimit() + " of being interrupted: their browsers may still be open");
				}
			}
		}
		catch (@SuppressWarnings("unused") InterruptedException e) {
			Thread.currentThread().interrupt();
			stopping.set(true);
			problems.add("the run was interrupted");
		}
		finally {
			System.setOut(out);
		}
		problems.addAll(recorder.problems());
		SailLoadReport result = new SailLoadReport(name, plan, recorder.samples(), recorder.iterations(), clock.getAsLong() - start, problems);
		out.println(result.describe());
		Path folder = ((reportDirectory == null) ? sources.workingDirectory().resolve(REPORT_DIR) : reportDirectory)
						.resolve(FOLDER_TIME.format(LocalDateTime.now()) + '-' + name.replaceAll("[^A-Za-z0-9._-]+", "_"));
		try {
			result.write(folder);
			out.println("SAIL load report: " + folder.resolve("index.html"));
		}
		catch (IOException e) {
			out.println("SAIL load: could not write the report in " + folder + ": " + e);
		}
		return result;
	}

	/**
	 * Runs one virtual user's iterations on its own thread, with a persistence for the test's user.
	 */
	private void runUser(int number,
							@Nonnull Plan plan,
							long start,
							@Nonnull VirtualUserTask task,
							@Nonnull Recorder recorder,
							@Nonnull AtomicBoolean stopping,
							@Nullable User testUser) {
		TRACE_USER.set(userName(number));
		AbstractPersistence persistence = null;
		try {
			if (testUser != null) {
				persistence = AbstractPersistence.get();
				persistence.setUser(testUser);
				persistence.begin();
			}
			VirtualUser user = new VirtualUser(number, engine(), clock, recorder);
			Duration duration = plan.duration();
			long end = (duration == null) ? Long.MAX_VALUE : start + duration.toNanos();
			for (int iteration = 1;
					(! stopping.get()) && (! Thread.currentThread().isInterrupted()) &&
						((plan.iterations() == 0) || (iteration <= plan.iterations())) &&
						((duration == null) || (clock.getAsLong() - end < 0L));
					iteration++) {
				runIteration(user, iteration, task, recorder);
			}
		}
		catch (RuntimeException e) { // its persistence or engine could not be made: the summary says why it ran no more
			recorder.problem(userName(number) + " stopped: " + e);
		}
		finally {
			if (persistence != null) {
				persistence.rollback();
				persistence.commit(true);
			}
			TRACE_USER.remove();
		}
	}

	private void runIteration(@Nonnull VirtualUser user, int iteration, @Nonnull VirtualUserTask task, @Nonnull Recorder recorder) {
		int number = user.number();
		user.iteration = iteration;
		SailLoadProbe.set(new SailLoadProbe(String.format("user%02d-it%03d", Integer.valueOf(number), Integer.valueOf(iteration)),
											clock,
											(name, startNanos, endNanos, failed, failure) ->
												recorder.sample(new Sample(name, number, iteration, startNanos, endNanos, failed, failure))));
		long started = clock.getAsLong();
		Throwable failure = null;
		try {
			task.run(user);
		}
		catch (InterruptedException e) {
			Thread.currentThread().interrupt(); // stopped at the time limit
			failure = e;
		}
		catch (Exception | AssertionError e) { // the iteration failed; the next one is independent
			failure = e;
		}
		finally {
			SailLoadProbe.set(null);
		}
		try {
			// a suite can leave its last browser open for the engine's next run: every iteration ends with none open
			user.workflow().closeBrowser();
		}
		catch (RuntimeException e) {
			if (failure == null) {
				failure = e;
			}
		}
		long ended = clock.getAsLong();
		recorder.iteration(new Iteration(number, iteration, started, ended, failure));
		System.out.println(String.format(Locale.ROOT, "SAIL load: iteration %d %s in %.1f s", Integer.valueOf(iteration),
											(failure == null) ? "passed" : "failed", Double.valueOf((ended - started) / 1e9)));
	}

	/** Makes a virtual user's engine, with the test's settings and this run's view roles and browsers. */
	private @Nonnull SailWorkflow engine() {
		SailViewRoles roles = new SailViewRoles();
		for (String[] registration : viewRoles) {
			roles.register(registration[0], Arrays.copyOfRange(registration, 1, registration.length));
		}
		SailWorkflow result = new SailWorkflow(roles).settings(settings);
		for (Map.Entry<UserAgentType, Supplier<BrowserConfiguration>> browser : browsers.entrySet()) {
			result.browser(browser.getKey(), browser.getValue());
		}
		return result;
	}

	private static @Nonnull String userName(int number) {
		return String.format("user %02d", Integer.valueOf(number));
	}

	/**
	 * Standard output while a load run lasts: each line printed on a virtual user's thread starts with the user, so that
	 * the users' interleaved traces can be told apart.
	 */
	private static final class UserPrefixedStream extends PrintStream {
		UserPrefixedStream(@Nonnull OutputStream out) {
			super(out, true, StandardCharsets.UTF_8);
		}

		@Override
		public void println(@Nullable String line) {
			String user = TRACE_USER.get();
			super.println((user == null) ? line : '[' + user + "] " + line);
		}

		@Override
		public void println(@Nullable Object line) {
			println(String.valueOf(line));
		}
	}
}

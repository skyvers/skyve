package util.sail;

import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.Duration;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Suite.Evidence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.BrowserConfiguration.Browsers;

/**
 * The settings of a SAIL run, resolved from every place a setting can come from, each value with its source.
 * <p>
 * <b>Precedence</b>, highest first: a {@code -D} system property, an environment variable, the test's code (see
 * {@link #code()}), the user's properties file, a {@link BrowserConfiguration}'s own values, the suite, the workflow,
 * the automation, and the default. The test's code beats the properties file so that a developer's quick change in a
 * test takes effect even with a personal file; only an explicit {@code -D} or environment variable for this run beats
 * it. A browser configuration's own values (an IT's boilerplate {@code .browser(chrome)}) come below the file, so that
 * the file can choose the browser. Of the script levels, the innermost explicit value wins: an automation's own
 * {@code testStrategy} beats its workflow's, which beats its suite's, as the outer ones are defaults for the
 * automations that state none.
 * <ul>
 * <li><b>Environment variables</b> are {@code SAIL_} and the upper snake case of the key after {@code sail.}:
 * 		{@code SAIL_BASE_URL}, {@code SAIL_TIMEOUT_SECONDS}, {@code SAIL_SUITE_ONLY}, {@code SAIL_VAR_PASSWORD}
 * 		(see {@link #environmentName(String)}).</li>
 * <li><b>The properties file</b> is the one {@value #PROPERTIES} names (from {@code -D}, the environment or code),
 * 		else {@code ~/.skyve/sail.properties} if it exists. It holds the same {@code sail.*} keys; a key it does not
 * 		know is refused, as it is most likely a typing error.</li>
 * <li><b>The script levels</b> set only what they own: an automation or workflow its {@code testStrategy}, a suite
 * 		its {@code testStrategy} and {@code evidence}. The base URL is never in a script.</li>
 * </ul>
 * A blank value counts as not set, so a higher level cannot clear a value that a lower level sets: to turn a
 * directory off, remove it where it is set. Resolution validates every effective value and fails before any browser
 * starts, with an {@link IllegalArgumentException} naming each invalid key, its value, its source and the valid
 * values. A {@code sail.*} system property or {@code SAIL_*} environment variable that is not a setting is ignored,
 * and listed in the run's trace (except the names that other SAIL code reads: {@code sail.recording},
 * {@code sail.personas}, {@code sail.personaPassword} and {@code sail.workflowData}).
 * <p>
 * A resolved {@code SailSettings} is an immutable snapshot of the sources as they were when it was resolved.
 */
public final class SailSettings {
	/** The prefix of every SAIL setting's key. */
	public static final String PREFIX = "sail.";
	/** The Skyve base URL (ending in {@code /}) that SAIL tests run against; no default. */
	public static final String BASE_URL = "sail.baseUrl";
	/** {@code false} shows the browser window; SAIL browsers are headless by default. */
	public static final String HEADLESS = "sail.headless";
	/** The browser: {@code chrome} (the default), {@code firefox}, {@code safari} or {@code edge}. */
	public static final String BROWSER = "sail.browser";
	/** The timeout in seconds for every wait on the browser (default {@value #DEFAULT_TIMEOUT_SECONDS}). */
	public static final String TIMEOUT_SECONDS = "sail.timeoutSeconds";
	/** Overrides the scripts' test strategy: {@code Assert}, {@code Verify} or {@code None}, in any case. */
	public static final String TEST_STRATEGY = "sail.testStrategy";
	/** A directory to save a screenshot and the page source of each failure into. */
	public static final String FAILURE_DIR = "sail.failureDir";
	/** A directory for step snapshots and their film strip. */
	public static final String SNAPSHOT_DIR = "sail.snapshotDir";
	/** {@code true} also saves each step's page source with its snapshot. */
	public static final String SNAPSHOT_SOURCE = "sail.snapshotSource";
	/** A directory for the run's video. */
	public static final String VIDEO_DIR = "sail.videoDir";
	/**
	 * The evidence a run keeps when no directory is given for it ({@link Evidence}): {@code none} (only the directories
	 * that are set), {@code failures}, {@code snapshots} (and failures), {@code video} (and failures) or {@code all}. An
	 * explicitly set directory always turns its kind on; a request adds the kinds it names, in
	 * {@link #DEFAULT_FAILURE_DIR} and {@link #DEFAULT_MEDIA_DIR} under the working directory.
	 */
	public static final String EVIDENCE = "sail.evidence";
	/** Which runs keep their snapshots and video; see {@link KeepMedia}. */
	public static final String KEEP_MEDIA = "sail.keepMedia";
	/** The suite files to run, by prefix, comma separated, e.g. {@code 03,04}. */
	public static final String SUITE_ONLY = "sail.suite.only";
	/** The scripts and suites the generic runner runs: classpath resources or file paths, comma separated. */
	public static final String RUN = "sail.run";
	/**
	 * The folder the device profiles of {@code StartDevice} go in, each at
	 * {@code <folder>/<run token>-<pid>/<profile>} (default {@link #DEFAULT_PROFILES_DIR} under the working directory).
	 */
	public static final String PROFILES = "sail.profiles";
	/** Which runs keep their device profiles when they end; see {@link KeepProfiles}. */
	public static final String PROFILES_KEEP = "sail.profiles.keep";
	/**
	 * How many hours old a run's device profiles must be before a later run deletes them, whichever way the run ended
	 * (default {@value #DEFAULT_PROFILES_KEEP_HOURS}); see {@link SailProfiles}.
	 */
	public static final String PROFILES_KEEP_HOURS = "sail.profiles.keepHours";
	/** The prefix of a variable's key: {@code sail.var.password} is the value of {@code {var:password}}. */
	public static final String VAR_PREFIX = "sail.var.";
	/** The path of the user's properties file. */
	public static final String PROPERTIES = "sail.properties";
	/** The number of virtual users of a load run ({@link SailLoad}); set, it makes {@code SailRunIT} a load run. */
	public static final String LOAD_USERS = "sail.load.users";
	/** How long a load run takes to start all its virtual users, evenly spread (default none: all at once). */
	public static final String LOAD_RAMP_UP = "sail.load.rampUp";
	/** How long a load run starts new iterations for, from its start, ramp-up included. */
	public static final String LOAD_DURATION = "sail.load.duration";
	/** How many iterations each virtual user of a load run runs (default 1 when no duration is set). */
	public static final String LOAD_ITERATIONS = "sail.load.iterations";
	/**
	 * The longest a load run may take, iterations in flight at its end included (default: its duration and 5 minutes;
	 * none when only iterations are set, as each step's own timeout bounds them).
	 */
	public static final String LOAD_TIME_LIMIT = "sail.load.timeLimit";
	/** The percentage of a load run's iterations that may fail before the run fails (default 0: any failure fails). */
	public static final String LOAD_MAX_FAILURE_RATE = "sail.load.maxFailureRate";
	/** The longest that any interaction's 95th percentile duration may be in a load run (default no limit). */
	public static final String LOAD_MAX_P95 = "sail.load.maxP95";

	/** The timeout for every wait on the browser when none is set. */
	public static final long DEFAULT_TIMEOUT_SECONDS = 30L;
	/** Where failure screenshots go when an evidence request asks for them and no directory is given. */
	public static final Path DEFAULT_FAILURE_DIR = Path.of("target", "sail", "failures");
	/** Where snapshots and video go when an evidence request asks for them and no directory is given. */
	public static final Path DEFAULT_MEDIA_DIR = Path.of("target", "sail", "media");
	/** Where device profiles go when no folder is given. */
	public static final Path DEFAULT_PROFILES_DIR = Path.of("target", "sail-profiles");
	/** How many hours old a run's device profiles must be before a later run deletes them, when none is set. */
	public static final int DEFAULT_PROFILES_KEEP_HOURS = 12;

	/** The settings a properties file or {@link Code#set(String, String)} may hold, besides variables, in display order. */
	private static final List<String> KEYS = List.of(BASE_URL, HEADLESS, BROWSER, TIMEOUT_SECONDS, TEST_STRATEGY,
														FAILURE_DIR, SNAPSHOT_DIR, SNAPSHOT_SOURCE, VIDEO_DIR, EVIDENCE,
														KEEP_MEDIA, SUITE_ONLY, RUN, PROFILES, PROFILES_KEEP, PROFILES_KEEP_HOURS,
														PROPERTIES, LOAD_USERS, LOAD_RAMP_UP, LOAD_DURATION, LOAD_ITERATIONS, LOAD_TIME_LIMIT, LOAD_MAX_FAILURE_RATE,
														LOAD_MAX_P95);
	/** The {@code sail.*} system properties that other SAIL code reads, so that they are not listed as ignored. */
	private static final Set<String> OTHER_SAIL_PROPERTIES = Set.of("sail.recording", "sail.personas", "sail.personaPassword", "sail.workflowData");
	private static final String MASK = "********";
	/** A duration as a whole number and a unit, e.g. {@code 30s}. */
	private static final Pattern DURATION = Pattern.compile("(\\d+)\\s*(ms|s|m|h)", Pattern.CASE_INSENSITIVE);
	/** The prefix of a SAIL environment variable. */
	private static final String ENVIRONMENT_PREFIX = "SAIL_";
	/** The prefix of a variable's environment variable. */
	private static final String ENVIRONMENT_VAR_PREFIX = "SAIL_VAR_";

	/** Which automation runs keep their snapshot and video folders. */
	public enum KeepMedia {
		/** Every run keeps them. */
		ALL,
		/** A run that passed deletes them when the unit it belongs to (a test's script, a workflow) passed. */
		FAILED
	}

	/** Which runs keep their device profile folders when they end ({@link SailProfiles}). */
	public enum KeepProfiles {
		/** A run that failed keeps its folder, and prints where it is; a run that passed deletes it. */
		FAILED,
		/** Every run keeps its folder. */
		ALL,
		/** Every run deletes its folder. */
		NONE
	}

	/** Where a setting can come from, highest precedence first. */
	public enum Level {
		/** A {@code -D} system property. */
		SYSTEM_PROPERTY,
		/** An environment variable. */
		ENVIRONMENT,
		/** The test's code: {@link SailSettings#code()}, as a test base's {@code settings()}. */
		CODE,
		/** The user's properties file. */
		FILE,
		/** A {@link BrowserConfiguration}'s own values. */
		CONFIGURATION,
		/** An automation's own attribute. */
		AUTOMATION,
		/** A workflow's default. */
		WORKFLOW,
		/** A suite's default or request. */
		SUITE,
		/** The setting's default. */
		DEFAULT;

		/**
		 * Indicates if this level is part of the run (a command, CI job or the test's code), rather than a script.
		 *
		 * @return true for {@code -D}, the environment, code, the properties file and a browser configuration
		 */
		public boolean isRun() {
			return ordinal() <= CONFIGURATION.ordinal();
		}
	}

	/**
	 * Where a resolved value came from.
	 *
	 * @param level	how high its source ranks
	 * @param where	the exact place, for messages: {@code -Dsail.testStrategy}, {@code SAIL_TEST_STRATEGY}, {@code code},
	 * 				the properties file's path, or the script
	 */
	public record Source(@Nonnull Level level, @Nonnull String where) {
		@Override
		public @Nonnull String toString() {
			return where;
		}
	}

	/**
	 * A resolved setting: its key, its text as given, and where it came from.
	 *
	 * @param key	e.g. {@code sail.testStrategy} or {@code sail.var.password}
	 * @param value	the text as given (trimmed, but a variable's as given)
	 * @param source	where it came from
	 */
	public record Setting(@Nonnull String key, @Nonnull String value, @Nonnull Source source) {
		/**
		 * Describes the setting as {@code testStrategy=Verify (-Dsail.testStrategy)}, with a variable's value hidden.
		 */
		@Override
		public @Nonnull String toString() {
			return key.substring(PREFIX.length()) + '=' + (key.startsWith(VAR_PREFIX) ? MASK : value) + " (" + source + ')';
		}
	}

	/**
	 * The sources outside the code and the scripts: system properties, environment variables, the user's home (for
	 * {@code ~/.skyve/sail.properties}) and the working directory (for relative directories). Given explicitly, so that
	 * tests resolve settings without the JVM's own.
	 *
	 * @param systemProperties	the {@code sail.*} system properties
	 * @param environment	the {@code SAIL_*} environment variables
	 * @param userHome	the user's home directory, or null to read no user properties file
	 * @param workingDirectory	the directory relative directories resolve against
	 */
	record Environment(@Nonnull Map<String, String> systemProperties,
						@Nonnull Map<String, String> environment,
						@Nullable Path userHome,
						@Nonnull Path workingDirectory) {
		/**
		 * Returns this JVM's sources as they are now: copies of its {@code sail.*} system properties and its
		 * {@code SAIL_*} environment variables.
		 *
		 * @return the sources
		 */
		static @Nonnull Environment current() {
			Properties properties = System.getProperties();
			Map<String, String> sail = new TreeMap<>();
			for (String name : properties.stringPropertyNames()) {
				if (name.startsWith(PREFIX)) {
					sail.put(name, properties.getProperty(name));
				}
			}
			Map<String, String> env = new TreeMap<>();
			for (Map.Entry<String, String> variable : System.getenv().entrySet()) {
				if (variable.getKey().startsWith(ENVIRONMENT_PREFIX)) {
					env.put(variable.getKey(), variable.getValue());
				}
			}
			return new Environment(sail, env, Path.of(System.getProperty("user.home")), Path.of("").toAbsolutePath());
		}

		/**
		 * Returns sources with nothing in them, for a configuration that must resolve only its own values.
		 *
		 * @return no system properties, environment variables or properties file; this JVM's working directory
		 */
		static @Nonnull Environment none() {
			return new Environment(Map.of(), Map.of(), null, Path.of("").toAbsolutePath());
		}

		/**
		 * Describes the sources with every variable's value hidden.
		 */
		@Override
		public @Nonnull String toString() {
			return "Environment[systemProperties=" + masked(systemProperties, VAR_PREFIX) +
					", environment=" + masked(environment, ENVIRONMENT_VAR_PREFIX) +
					", userHome=" + userHome + ", workingDirectory=" + workingDirectory + ']';
		}

		private static @Nonnull Map<String, String> masked(@Nonnull Map<String, String> values, @Nonnull String variablePrefix) {
			Map<String, String> result = new TreeMap<>();
			for (Map.Entry<String, String> entry : values.entrySet()) {
				String key = entry.getKey();
				result.put(key, key.startsWith(variablePrefix) ? MASK : entry.getValue());
			}
			return result;
		}
	}

	/**
	 * The values one source gives, at one level. Code layers come from {@link Code}; script layers from
	 * {@link SailSettings#automation}, {@link SailSettings#workflow} and {@link SailSettings#suite}, which allow only
	 * what each level owns.
	 */
	static final class Layer {
		private final Source source;
		private final Map<String, String> values;
		/** The source of each key, when each has its own (a system property or environment variable); or null. */
		private final Map<String, String> wheres;

		private Layer(@Nonnull Level level, @Nonnull String where, @Nonnull Map<String, String> values) {
			this(level, where, values, null);
		}

		private Layer(@Nonnull Level level, @Nonnull String where, @Nonnull Map<String, String> values, @Nullable Map<String, String> wheres) {
			this.source = new Source(level, where);
			this.values = values;
			this.wheres = wheres;
		}

		@Nonnull Level level() {
			return source.level();
		}

		/** Returns the value this layer gives a key, or null; a blank value counts as not given. */
		@Nullable String get(@Nonnull String key) {
			String result = values.get(key);
			return ((result == null) || result.isBlank()) ? null : result;
		}

		/** Returns where this layer's value of a key comes from, e.g. {@code -Dsail.testStrategy} for a system property. */
		@Nonnull Source sourceOf(@Nonnull String key) {
			String where = (wheres == null) ? null : wheres.get(key);
			return (where == null) ? source : new Source(source.level(), where);
		}
	}

	/**
	 * The code level: a fluent builder of the settings a test gives in code, e.g.
	 * {@code SailSettings.code().headless(false).timeoutSeconds(60).videoDir(Path.of("/tmp/v")).variable("password", "x")}.
	 * The test bases, {@link SailWorkflow} and {@link SailSuite} each have one, from {@code settings()}. A value set here
	 * beats the properties file, a browser configuration's own values and the scripts, but not a {@code -D} system
	 * property or an environment variable. Values are checked when the settings are resolved.
	 * <p>
	 * Not thread-safe: a test sets it on its own thread, before the run that uses it.
	 */
	public static final class Code {
		private final Map<String, String> values = new LinkedHashMap<>();

		Code() {
			// see SailSettings.code()
		}

		/**
		 * Sets a setting by key, as text, as a system property would; null clears it.
		 *
		 * @param key	a key of {@link SailSettings}, or {@value SailSettings#VAR_PREFIX}{@code name}
		 * @param value	the text, or null to clear the key
		 * @return this builder
		 * @throws IllegalArgumentException	if the key is not a SAIL setting
		 */
		public @Nonnull Code set(@Nonnull String key, @Nullable String value) {
			if (! isSettingKey(key)) {
				throw new IllegalArgumentException(key + " is not a SAIL setting: use one of " + KEYS + " or " + VAR_PREFIX + "<name>");
			}
			if (value == null) {
				values.remove(key);
			}
			else {
				values.put(key, value);
			}
			return this;
		}

		/**
		 * Sets the Skyve base URL to run against.
		 *
		 * @param baseUrl	an http or https URL, ending in {@code /}
		 * @return this builder
		 */
		public @Nonnull Code baseUrl(@Nonnull String baseUrl) {
			return set(BASE_URL, baseUrl);
		}

		/**
		 * Sets whether the browser is headless.
		 *
		 * @param headless	false to show the browser window
		 * @return this builder
		 */
		public @Nonnull Code headless(boolean headless) {
			return set(HEADLESS, String.valueOf(headless));
		}

		/**
		 * Sets the browser, for every device of the run.
		 *
		 * @param browser	Chrome, Firefox, Safari or Edge
		 * @return this builder
		 */
		public @Nonnull Code browser(@Nonnull Browsers browser) {
			return set(BROWSER, browser.name());
		}

		/**
		 * Sets the timeout for every wait on the browser.
		 *
		 * @param seconds	the timeout in seconds, above 0
		 * @return this builder
		 */
		public @Nonnull Code timeoutSeconds(long seconds) {
			return set(TIMEOUT_SECONDS, String.valueOf(seconds));
		}

		/**
		 * Overrides the scripts' test strategy, as {@code -Dsail.testStrategy} does.
		 *
		 * @param testStrategy	Assert, Verify or None, for every automation that is not locked
		 * @return this builder
		 */
		public @Nonnull Code testStrategy(@Nonnull TestStrategy testStrategy) {
			return set(TEST_STRATEGY, testStrategy.name());
		}

		/**
		 * Sets the directory for failure screenshots and page sources.
		 *
		 * @param directory	absolute, or relative to the working directory
		 * @return this builder
		 */
		public @Nonnull Code failureDir(@Nonnull Path directory) {
			return set(FAILURE_DIR, directory.toString());
		}

		/**
		 * Sets the directory for step snapshots and their film strip.
		 *
		 * @param directory	absolute, or relative to the working directory
		 * @return this builder
		 */
		public @Nonnull Code snapshotDir(@Nonnull Path directory) {
			return set(SNAPSHOT_DIR, directory.toString());
		}

		/**
		 * Sets whether each step's page source is saved with its snapshot.
		 *
		 * @param snapshotSource	true to save the page sources
		 * @return this builder
		 */
		public @Nonnull Code snapshotSource(boolean snapshotSource) {
			return set(SNAPSHOT_SOURCE, String.valueOf(snapshotSource));
		}

		/**
		 * Sets the directory for the run's video.
		 *
		 * @param directory	absolute, or relative to the working directory
		 * @return this builder
		 */
		public @Nonnull Code videoDir(@Nonnull Path directory) {
			return set(VIDEO_DIR, directory.toString());
		}

		/**
		 * Requests evidence in the default directories; see {@link SailSettings#EVIDENCE}.
		 *
		 * @param evidence	what to keep
		 * @return this builder
		 */
		public @Nonnull Code evidence(@Nonnull Evidence evidence) {
			return set(EVIDENCE, evidence.name());
		}

		/**
		 * Sets which runs keep their snapshots and video.
		 *
		 * @param keepMedia	{@link KeepMedia#FAILED} to delete a passed run's
		 * @return this builder
		 */
		public @Nonnull Code keepMedia(@Nonnull KeepMedia keepMedia) {
			return set(KEEP_MEDIA, keepMedia.name().toLowerCase());
		}

		/**
		 * Chooses the suite members to run, by prefix of their file (e.g. {@code pf/}) or its name (e.g. {@code 03}).
		 *
		 * @param prefixes	e.g. {@code "pf/", "03"}
		 * @return this builder
		 */
		public @Nonnull Code suiteOnly(@Nonnull String... prefixes) {
			return set(SUITE_ONLY, String.join(",", prefixes));
		}

		/**
		 * Names the scripts and suites the generic runner runs.
		 *
		 * @param scripts	classpath resources or file paths
		 * @return this builder
		 */
		public @Nonnull Code run(@Nonnull String... scripts) {
			return set(RUN, String.join(",", scripts));
		}

		/**
		 * Sets the folder the device profiles go in.
		 *
		 * @param directory	absolute, or relative to the working directory
		 * @return this builder
		 */
		public @Nonnull Code profiles(@Nonnull Path directory) {
			return set(PROFILES, directory.toString());
		}

		/**
		 * Sets which runs keep their device profiles when they end.
		 *
		 * @param keep	{@link KeepProfiles#ALL} to keep a passed run's too, e.g. to open a profile afterwards
		 * @return this builder
		 */
		public @Nonnull Code profilesKeep(@Nonnull KeepProfiles keep) {
			return set(PROFILES_KEEP, keep.name().toLowerCase());
		}

		/**
		 * Sets how many hours old a run's device profiles must be before a later run deletes them.
		 *
		 * @param hours	above 0
		 * @return this builder
		 */
		public @Nonnull Code profilesKeepHours(int hours) {
			return set(PROFILES_KEEP_HOURS, String.valueOf(hours));
		}

		/**
		 * Defines a variable that every automation starts with, for {@code {var:name}}, e.g. a password.
		 *
		 * @param name	the name after {@code var:}
		 * @param value	the value, used as given
		 * @return this builder
		 */
		public @Nonnull Code variable(@Nonnull String name, @Nonnull String value) {
			return set(VAR_PREFIX + name, value);
		}

		/**
		 * Names the properties file to read instead of {@code ~/.skyve/sail.properties}.
		 *
		 * @param file	absolute, or relative to the working directory
		 * @return this builder
		 */
		public @Nonnull Code properties(@Nonnull Path file) {
			return set(PROPERTIES, file.toString());
		}

		/** Returns this builder's values as the test's code layer; the layer shares the builder's values. */
		@Nonnull Layer layer() {
			return new Layer(Level.CODE, "code", values);
		}

		/** Returns this builder's values as a layer of a level, e.g. a browser configuration's; it shares the values. */
		@Nonnull Layer layer(@Nonnull Level level, @Nonnull String where) {
			return new Layer(level, where, values);
		}

		/**
		 * Resolves the settings with this builder as the code level, from this JVM's system properties, environment and
		 * the properties file.
		 *
		 * @return the settings
		 * @throws IllegalArgumentException	if a setting is invalid
		 */
		public @Nonnull SailSettings resolve() {
			return SailSettings.resolve(layer());
		}
	}

	private final Environment environment;
	private final List<Layer> layers;
	/** The settings that do not come from their defaults, by key, in key order. */
	private final Map<String, Setting> settings = new LinkedHashMap<>();
	/** The variables, by key. */
	private final Map<String, Setting> variables = new TreeMap<>();
	/** The {@code sail.*} and {@code SAIL_*} names that are not settings, in order. */
	private final List<String> ignored = new ArrayList<>();

	// the parsed values, set once by parse()
	private String url;
	private boolean headlessBrowser = true;
	private Browsers browserType = Browsers.chrome;
	private Duration waitTimeout = Duration.ofSeconds(DEFAULT_TIMEOUT_SECONDS);
	private TestStrategy strategy;
	private Path failureDirectory;
	private Path snapshotDirectory;
	private boolean withPageSources;
	private Path videoDirectory;
	private Evidence evidenceRequest = Evidence.none;
	private KeepMedia mediaKept = KeepMedia.ALL;
	private List<String> onlyPrefixes = List.of();
	private List<String> runScripts = List.of();
	private Path profilesDirectory;
	private KeepProfiles profilesKept = KeepProfiles.FAILED;
	private int profilesHours = DEFAULT_PROFILES_KEEP_HOURS;
	private int virtualUsers;
	private Duration rampUpTime = Duration.ZERO;
	private Duration loadTime;
	private int iterationCount;
	private Duration loadLimit;
	private double failureRate;
	private Duration p95Limit;

	/** Why the test strategy is locked to the scripts' own (e.g. {@code set-up}), or null if a run override applies. */
	private final String lock;
	/** The run override that a lock kept from applying, or null. */
	private Setting ignoredOverride;

	private SailSettings(@Nonnull Environment environment, @Nonnull List<Layer> layers, @Nullable String lock) {
		this.environment = environment;
		this.layers = layers;
		this.lock = lock;
	}

	/**
	 * Starts a code-level builder.
	 *
	 * @return a builder with nothing set
	 */
	public static @Nonnull Code code() {
		return new Code();
	}

	/**
	 * Makes an automation's layer: its own test strategy.
	 *
	 * @param name	the automation, for messages, e.g. its script; or null if it is not known
	 * @param testStrategy	the automation's {@code testStrategy}, or null if it states none
	 * @return the layer
	 */
	static @Nonnull Layer automation(@Nullable String name, @Nullable TestStrategy testStrategy) {
		return scriptLayer(Level.AUTOMATION, (name == null) ? "automation" : "automation " + name, testStrategy, null);
	}

	/**
	 * Makes a workflow's layer: its default test strategy for the automations that state none.
	 *
	 * @param name	the workflow, for messages, e.g. its file
	 * @param testStrategy	the workflow's {@code testStrategy}, or null
	 * @return the layer
	 */
	static @Nonnull Layer workflow(@Nonnull String name, @Nullable TestStrategy testStrategy) {
		return scriptLayer(Level.WORKFLOW, "workflow " + name, testStrategy, null);
	}

	/**
	 * Makes a suite's layer: its default test strategy and its evidence request.
	 *
	 * @param name	the suite, for messages, e.g. its file
	 * @param testStrategy	the suite's {@code testStrategy}, or null
	 * @param evidence	the suite's {@code evidence}, or null
	 * @return the layer
	 */
	static @Nonnull Layer suite(@Nonnull String name, @Nullable TestStrategy testStrategy, @Nullable Evidence evidence) {
		return scriptLayer(Level.SUITE, "suite " + name, testStrategy, evidence);
	}

	private static @Nonnull Layer scriptLayer(@Nonnull Level level,
												@Nonnull String where,
												@Nullable TestStrategy testStrategy,
												@Nullable Evidence evidence) {
		Map<String, String> values = new LinkedHashMap<>();
		if (testStrategy != null) {
			values.put(TEST_STRATEGY, testStrategy.name());
		}
		if (evidence != null) {
			values.put(EVIDENCE, evidence.name());
		}
		return new Layer(level, where, values);
	}

	/**
	 * Resolves the settings from this JVM's system properties, environment and properties file, and the given layers.
	 *
	 * @param layers	the code and script layers, in any order; layers of one level take precedence in the order given
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid, or a named properties file is missing or unreadable
	 */
	static @Nonnull SailSettings resolve(@Nonnull Layer... layers) {
		return resolve(Environment.current(), Arrays.asList(layers));
	}

	/**
	 * Resolves the settings from the given sources.
	 *
	 * <p>Side effects: reads the properties file, if there is one.
	 *
	 * @param environment	the system properties, environment variables, user home and working directory
	 * @param layers	the code and script layers, in any order; layers of one level take precedence in the order given
	 * @return the settings
	 * @throws IllegalArgumentException	listing every invalid setting with its source and the valid values, or if a named
	 * 			properties file is missing or unreadable, or holds a key that is not a SAIL setting
	 */
	static @Nonnull SailSettings resolve(@Nonnull Environment environment, @Nonnull List<Layer> layers) {
		return resolve(environment, layers, null);
	}

	private static @Nonnull SailSettings resolve(@Nonnull Environment environment, @Nonnull List<Layer> layers, @Nullable String lock) {
		List<Layer> given = new ArrayList<>(layers);
		SailSettings result = new SailSettings(environment, given, lock);
		Layer system = result.systemLayer();
		// the levels above the file name it; the environment's variables are bound once the file's keys are known too
		List<Layer> above = new ArrayList<>(given.size() + 2);
		above.add(system);
		above.add(result.environmentLayer(given, null, false));
		above.addAll(given);
		above.sort(Comparator.comparing(Layer::level)); // stable: a level's layers keep their order
		Layer file = result.fileLayer(first(above, PROPERTIES, false));
		List<Layer> ordered = new ArrayList<>(given.size() + 3);
		ordered.add(system);
		ordered.add(result.environmentLayer(given, file, true));
		ordered.addAll(given);
		if (file != null) {
			ordered.add(file);
		}
		ordered.sort(Comparator.comparing(Layer::level));

		for (String key : KEYS) {
			Setting setting = first(ordered, key, false);
			if ((lock != null) && (setting != null) && TEST_STRATEGY.equals(key) && setting.source().level().isRun()) {
				// locked: the scripts' own strategy applies, and the run's override is noted
				result.ignoredOverride = setting;
				setting = first(ordered, key, true);
			}
			if (setting != null) {
				result.settings.put(key, setting);
			}
		}
		for (Layer layer : ordered) {
			for (String key : layer.values.keySet()) {
				// a value as given (a password may start or end with a space), but a blank one counts as not set
				String value = layer.get(key);
				if (key.startsWith(VAR_PREFIX) && (value != null) && (! result.variables.containsKey(key))) {
					result.variables.put(key, new Setting(key, value, layer.sourceOf(key)));
				}
			}
		}
		result.parse();
		return result;
	}

	/**
	 * Returns a key's value, trimmed, from the highest of the ordered layers that gives it one.
	 *
	 * @param scriptsOnly	true to look at the script levels only (for a locked test strategy)
	 * @return the setting, or null if no layer gives the key a value
	 */
	private static @Nullable Setting first(@Nonnull List<Layer> ordered, @Nonnull String key, boolean scriptsOnly) {
		for (Layer layer : ordered) {
			String value = layer.get(key);
			if ((value != null) && ((! scriptsOnly) || (! layer.level().isRun()))) {
				return new Setting(key, value.trim(), layer.sourceOf(key));
			}
		}
		return null;
	}

	/**
	 * Returns these settings with more layers, resolved again from the same environment and layers. A code layer
	 * shares its builder's values, so they are its values as they are now.
	 *
	 * @param more	the layers to add, e.g. an automation's
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	@Nonnull SailSettings with(@Nonnull Layer... more) {
		List<Layer> all = new ArrayList<>(layers.size() + more.length);
		all.addAll(layers);
		all.addAll(Arrays.asList(more));
		return resolve(environment, all, lock);
	}

	/**
	 * Returns these settings with the test strategy locked to the scripts' own: a run-level override ({@code -D}, the
	 * environment, code or the properties file) does not apply, and is kept as {@link #ignoredOverride()}. For a suite's
	 * locked members (its set-ups, its restore, and members with {@code lockTestStrategy="true"}). Later
	 * {@link #with(Layer...)} calls keep the lock.
	 *
	 * @param reason	why, for messages, e.g. {@code set-up}
	 * @return the settings
	 */
	@Nonnull SailSettings lockTestStrategy(@Nonnull String reason) {
		return resolve(environment, layers, reason);
	}

	/**
	 * Returns the run's test strategy override that a lock kept from applying.
	 *
	 * @return the override with its source, or null if there was none or the settings are not locked
	 */
	@Nullable Setting ignoredOverride() {
		return ignoredOverride;
	}

	/** Makes the system properties' layer, noting the {@code sail.*} properties that are not settings. */
	private @Nonnull Layer systemLayer() {
		Map<String, String> values = new LinkedHashMap<>();
		Map<String, String> wheres = new LinkedHashMap<>();
		for (Map.Entry<String, String> property : environment.systemProperties().entrySet()) {
			String key = property.getKey();
			if (isSettingKey(key)) {
				values.put(key, property.getValue());
				wheres.put(key, "-D" + key);
			}
			else if (! OTHER_SAIL_PROPERTIES.contains(key)) {
				ignored.add("-D" + key);
			}
		}
		return new Layer(Level.SYSTEM_PROPERTY, "-D", values, wheres);
	}

	/**
	 * Makes the environment variables' layer.
	 *
	 * @param given	the code and script layers, whose variables bind the variables' environment variables
	 * @param file	the properties file's layer, whose variables do too; or null
	 * @param noteIgnored	true to note the {@code SAIL_*} variables that are not settings
	 */
	private @Nonnull Layer environmentLayer(@Nonnull List<Layer> given, @Nullable Layer file, boolean noteIgnored) {
		Map<String, String> env = environment.environment();
		Map<String, String> values = new LinkedHashMap<>();
		Map<String, String> names = new LinkedHashMap<>();
		Map<String, String> keysByName = new TreeMap<>();
		for (String key : KEYS) {
			keysByName.put(environmentName(key), key);
		}
		List<Layer> others = new ArrayList<>(given);
		if (file != null) {
			others.add(file);
		}
		for (Layer layer : others) {
			addVariableKeys(keysByName, layer.values.keySet());
		}
		addVariableKeys(keysByName, environment.systemProperties().keySet());
		Set<String> otherNames = new TreeSet<>();
		for (String other : OTHER_SAIL_PROPERTIES) {
			otherNames.add(environmentName(other));
		}
		for (Map.Entry<String, String> variable : env.entrySet()) {
			String name = variable.getKey();
			String key = keysByName.get(name);
			if ((key == null) && name.startsWith(ENVIRONMENT_VAR_PREFIX) && (name.length() > ENVIRONMENT_VAR_PREFIX.length())) {
				// a variable no other source names: its name is the lower camel case of the environment variable's
				key = VAR_PREFIX + camelCase(name.substring(ENVIRONMENT_VAR_PREFIX.length()));
			}
			if (key != null) {
				values.put(key, variable.getValue());
				names.put(key, name);
			}
			else if (noteIgnored && name.startsWith(ENVIRONMENT_PREFIX) && (! otherNames.contains(name))) {
				ignored.add(name);
			}
		}
		return new Layer(Level.ENVIRONMENT, "environment", values, names);
	}

	private static void addVariableKeys(@Nonnull Map<String, String> keysByEnvironmentName, @Nonnull Iterable<String> keys) {
		for (String key : keys) {
			if (key.startsWith(VAR_PREFIX)) {
				keysByEnvironmentName.put(environmentName(key), key);
			}
		}
	}

	/**
	 * Returns the environment variable for a key: {@code SAIL_} and the upper snake case of the key after
	 * {@code sail.}, e.g. {@code SAIL_BASE_URL} for {@code sail.baseUrl}, {@code SAIL_SUITE_ONLY} for
	 * {@code sail.suite.only} and {@code SAIL_VAR_PASSWORD} for {@code sail.var.password}. An environment variable
	 * {@code SAIL_VAR_*} that names no variable known from another source is the variable of its lower camel case name:
	 * {@code SAIL_VAR_ADMIN_PASSWORD} is {@code adminPassword}.
	 *
	 * @param key	a {@code sail.*} key
	 * @return the environment variable's name
	 */
	static @Nonnull String environmentName(@Nonnull String key) {
		String name = key.startsWith(PREFIX) ? key.substring(PREFIX.length()) : key;
		StringBuilder result = new StringBuilder(name.length() + 12).append(ENVIRONMENT_PREFIX);
		char previous = '.';
		for (int i = 0, l = name.length(); i < l; i++) {
			char c = name.charAt(i);
			if (c == '.') {
				result.append('_');
			}
			else {
				if (Character.isUpperCase(c) && (Character.isLowerCase(previous) || Character.isDigit(previous))) {
					result.append('_');
				}
				result.append(Character.toUpperCase(c));
			}
			previous = c;
		}
		return result.toString();
	}

	/** Makes {@code ADMIN_PASSWORD} into {@code adminPassword}. */
	private static @Nonnull String camelCase(@Nonnull String snake) {
		StringBuilder result = new StringBuilder(snake.length());
		boolean upper = false;
		for (int i = 0, l = snake.length(); i < l; i++) {
			char c = snake.charAt(i);
			if (c == '_') {
				upper = ! result.isEmpty();
			}
			else {
				result.append(upper ? Character.toUpperCase(c) : Character.toLowerCase(c));
				upper = false;
			}
		}
		return result.toString();
	}

	/**
	 * Returns the properties file's layer, or null when there is no file: the file a run level names, else
	 * {@code ~/.skyve/sail.properties} if it exists (and the environment has a user home).
	 *
	 * <p>Side effects: reads the file.
	 *
	 * @param named	the file a level above names, or null
	 * @throws IllegalArgumentException	if a named file is missing or unreadable, or holds a key that is not a setting
	 */
	private @Nullable Layer fileLayer(@Nullable Setting named) {
		Path file;
		if (named == null) {
			Path home = environment.userHome();
			if (home == null) {
				return null;
			}
			file = home.resolve(".skyve").resolve("sail.properties");
			if (! Files.isRegularFile(file)) {
				return null;
			}
		}
		else {
			try {
				file = path(named.value());
			}
			catch (IllegalArgumentException e) {
				throw new IllegalArgumentException("Invalid SAIL setting: " + describeProblem(named, "use a file path (" + e.getMessage() + ')'), e);
			}
			if (! Files.isRegularFile(file)) {
				throw new IllegalArgumentException("Invalid SAIL setting: " + describeProblem(named, "there is no such file"));
			}
		}
		Properties properties = new Properties();
		try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
			properties.load(reader);
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Could not read the SAIL properties file " + file, e);
		}
		Map<String, String> values = new LinkedHashMap<>();
		List<String> unknown = new ArrayList<>();
		for (String key : new TreeSet<>(properties.stringPropertyNames())) {
			if ((! PROPERTIES.equals(key)) && isSettingKey(key)) {
				values.put(key, properties.getProperty(key));
			}
			else {
				unknown.add(key);
			}
		}
		if (! unknown.isEmpty()) {
			List<String> valid = new ArrayList<>(KEYS);
			valid.remove(PROPERTIES);
			throw new IllegalArgumentException(String.format("The SAIL properties file %s has keys that are not SAIL settings: %s; use %s or %s<name>",
																file, unknown, valid, VAR_PREFIX));
		}
		return new Layer(Level.FILE, file.toString(), values);
	}

	/**
	 * Indicates if a key is a SAIL setting or variable.
	 *
	 * @param key	any key
	 * @return true for a key of {@link SailSettings}, or {@value #VAR_PREFIX} and a name
	 */
	static boolean isSettingKey(@Nonnull String key) {
		return KEYS.contains(key) || (key.startsWith(VAR_PREFIX) && (key.length() > VAR_PREFIX.length()));
	}

	private static @Nonnull String describeProblem(@Nonnull Setting setting, @Nonnull String problem) {
		return String.format("%s=%s (%s): %s", setting.key(), setting.value(), setting.source(), problem);
	}

	/** Parses and checks every effective value, failing with all the invalid ones. */
	private void parse() {
		List<String> problems = new ArrayList<>();
		for (Setting setting : settings.values()) {
			try {
				parse(setting);
			}
			catch (IllegalArgumentException e) {
				problems.add(describeProblem(setting, e.getMessage()));
			}
		}
		if (! problems.isEmpty()) {
			throw new IllegalArgumentException("Invalid SAIL setting" + ((problems.size() == 1) ? ": " : "s:\n  ") + String.join("\n  ", problems));
		}
	}

	private void parse(@Nonnull Setting setting) {
		String value = setting.value();
		switch (setting.key()) {
			case BASE_URL -> url = url(value);
			case HEADLESS -> headlessBrowser = bool(value);
			case BROWSER -> browserType = constant(value, Browsers.values());
			case TIMEOUT_SECONDS -> waitTimeout = Duration.ofSeconds(seconds(value));
			case TEST_STRATEGY -> strategy = constant(value, TestStrategy.values());
			case FAILURE_DIR -> failureDirectory = directory(value);
			case SNAPSHOT_DIR -> snapshotDirectory = directory(value);
			case SNAPSHOT_SOURCE -> withPageSources = bool(value);
			case VIDEO_DIR -> videoDirectory = directory(value);
			case EVIDENCE -> evidenceRequest = constant(value, Evidence.values());
			case KEEP_MEDIA -> mediaKept = constant(value, KeepMedia.values());
			case SUITE_ONLY -> onlyPrefixes = list(value);
			case RUN -> runScripts = list(value);
			case PROFILES -> profilesDirectory = directory(value);
			case PROFILES_KEEP -> profilesKept = constant(value, KeepProfiles.values());
			case PROFILES_KEEP_HOURS -> profilesHours = count(value);
			case LOAD_USERS -> virtualUsers = count(value);
			case LOAD_RAMP_UP -> rampUpTime = duration(value, true);
			case LOAD_DURATION -> loadTime = duration(value, false);
			case LOAD_ITERATIONS -> iterationCount = count(value);
			case LOAD_TIME_LIMIT -> loadLimit = duration(value, false);
			case LOAD_MAX_FAILURE_RATE -> failureRate = percent(value);
			case LOAD_MAX_P95 -> p95Limit = duration(value, false);
			default -> {
				// PROPERTIES: read when the file was found
			}
		}
	}

	private static @Nonnull String url(@Nonnull String value) {
		try {
			URI uri = new URI(value);
			String scheme = uri.getScheme();
			if (("http".equalsIgnoreCase(scheme) || "https".equalsIgnoreCase(scheme)) && (uri.getHost() != null)) {
				return value;
			}
		}
		catch (@SuppressWarnings("unused") URISyntaxException e) {
			// not a URL: say what is valid below
		}
		throw new IllegalArgumentException("use an http or https URL ending in /, such as http://localhost:8080/skyve/");
	}

	private static boolean bool(@Nonnull String value) {
		if ("true".equalsIgnoreCase(value)) {
			return true;
		}
		if ("false".equalsIgnoreCase(value)) {
			return false;
		}
		throw new IllegalArgumentException("use true or false");
	}

	private static long seconds(@Nonnull String value) {
		try {
			long result = Long.parseLong(value);
			if (result > 0L) {
				return result;
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			// not a number: say what is valid below
		}
		throw new IllegalArgumentException("use a whole number of seconds above 0");
	}

	private static int count(@Nonnull String value) {
		try {
			int result = Integer.parseInt(value);
			if (result > 0) {
				return result;
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			// not a number: say what is valid below
		}
		throw new IllegalArgumentException("use a whole number above 0");
	}

	/**
	 * Parses a duration: a whole number and a unit ({@code 500ms}, {@code 30s}, {@code 5m}, {@code 1h}) or ISO-8601
	 * ({@code PT1M30S}).
	 *
	 * @param value	the setting's text
	 * @param zero	true if zero is valid
	 * @return the duration
	 * @throws IllegalArgumentException	saying what is valid, if it is not a duration, is negative, or is zero when zero
	 * 			is not valid
	 */
	private static @Nonnull Duration duration(@Nonnull String value, boolean zero) {
		Duration result = null;
		Matcher matcher = DURATION.matcher(value);
		try {
			if (matcher.matches()) {
				long amount = Long.parseLong(matcher.group(1));
				result = switch (matcher.group(2).toLowerCase()) {
					case "ms" -> Duration.ofMillis(amount);
					case "s" -> Duration.ofSeconds(amount);
					case "m" -> Duration.ofMinutes(amount);
					default -> Duration.ofHours(amount);
				};
			}
			else {
				result = Duration.parse(value);
			}
		}
		catch (@SuppressWarnings("unused") ArithmeticException | DateTimeParseException | NumberFormatException e) {
			// not a duration: say what is valid below
		}
		if ((result == null) || result.isNegative() || ((! zero) && result.isZero())) {
			throw new IllegalArgumentException("use a duration " + (zero ? "of 0 or more" : "above 0") + " such as 30s, 5m, 1h, 500ms or PT1M30S");
		}
		return result;
	}

	private static double percent(@Nonnull String value) {
		try {
			double result = Double.parseDouble(value);
			if ((result >= 0.0) && (result <= 100.0)) {
				return result;
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			// not a number: say what is valid below
		}
		throw new IllegalArgumentException("use a percentage from 0 to 100, such as 2.5");
	}

	/** Returns the constant named by a value in any case; the valid names are listed as the settings' values are written. */
	private static @Nonnull <E extends Enum<E>> E constant(@Nonnull String value, @Nonnull E[] constants) {
		List<String> names = new ArrayList<>(constants.length);
		for (E constant : constants) {
			String name = constant.name();
			if (name.equalsIgnoreCase(value)) {
				return constant;
			}
			names.add((constant instanceof TestStrategy) ? name : name.toLowerCase());
		}
		throw new IllegalArgumentException("use one of " + String.join(", ", names) + " (in any case)");
	}

	private @Nonnull Path directory(@Nonnull String value) {
		try {
			return path(value);
		}
		catch (IllegalArgumentException e) {
			throw new IllegalArgumentException("use a directory path (" + e.getMessage() + ')', e);
		}
	}

	/**
	 * Resolves a path against the working directory, normalised.
	 *
	 * @throws IllegalArgumentException	with the reason, if it is not a path
	 */
	private @Nonnull Path path(@Nonnull String value) {
		try {
			return environment.workingDirectory().resolve(value).normalize();
		}
		catch (InvalidPathException e) {
			throw new IllegalArgumentException(e.getReason(), e);
		}
	}

	private static @Nonnull List<String> list(@Nonnull String value) {
		List<String> result = new ArrayList<>();
		for (String item : value.split(",")) {
			String trimmed = item.trim();
			if (! trimmed.isEmpty()) {
				result.add(trimmed);
			}
		}
		if (result.isEmpty()) {
			throw new IllegalArgumentException("use a comma-separated list");
		}
		// the settings are an immutable snapshot
		return Collections.unmodifiableList(result);
	}

	/**
	 * Returns the Skyve base URL to run against.
	 *
	 * @return the URL, or null if none is set (there is no default)
	 */
	public @Nullable String baseUrl() {
		return url;
	}

	/**
	 * Returns the Skyve base URL to run against, which must be set.
	 *
	 * @return the URL
	 * @throws IllegalStateException	if none is set, so a run never silently targets an unintended server
	 */
	public @Nonnull String requireBaseUrl() {
		String result = url;
		if (result == null) {
			throw new IllegalStateException("No SAIL base URL: set -D" + BASE_URL + "=http://<host>:<port>/<context>/ (or " +
												environmentName(BASE_URL) + ", or baseUrl in code or the SAIL properties file) " +
												"to the disposable server to test against");
		}
		return result;
	}

	/**
	 * Indicates if the browser is headless.
	 *
	 * @return true unless set to false
	 */
	public boolean headless() {
		return headlessBrowser;
	}

	/**
	 * Returns the browser.
	 *
	 * @return Chrome unless set
	 */
	public @Nonnull Browsers browser() {
		return browserType;
	}

	/**
	 * Returns the timeout for every wait on the browser. It is a limit, not a delay.
	 *
	 * @return {@value #DEFAULT_TIMEOUT_SECONDS} seconds unless set
	 */
	public @Nonnull Duration timeout() {
		return waitTimeout;
	}

	/**
	 * Returns the effective test strategy: a run-level override, else the innermost script level's.
	 *
	 * @return the strategy, or null if nothing sets one (the executor then asserts)
	 */
	public @Nullable TestStrategy testStrategy() {
		return strategy;
	}

	/**
	 * Returns the directory for failure screenshots and page sources: the one set, else the default one when the
	 * evidence request asks for failures.
	 *
	 * @return the absolute directory, or null for none
	 */
	public @Nullable Path failureDir() {
		if (failureDirectory != null) {
			return failureDirectory;
		}
		return keepsFailures(evidenceRequest) ? environment.workingDirectory().resolve(DEFAULT_FAILURE_DIR) : null;
	}

	/**
	 * Returns the directory for step snapshots: the one set, else the default one when the evidence request asks for
	 * snapshots.
	 *
	 * @return the absolute directory, or null for none
	 */
	public @Nullable Path snapshotDir() {
		if (snapshotDirectory != null) {
			return snapshotDirectory;
		}
		return keepsSnapshots(evidenceRequest) ? environment.workingDirectory().resolve(DEFAULT_MEDIA_DIR) : null;
	}

	/**
	 * Indicates if each step's page source is saved with its snapshot.
	 *
	 * @return false unless set to true
	 */
	public boolean snapshotSource() {
		return withPageSources;
	}

	/**
	 * Returns the directory for the run's video: the one set, else the default one when the evidence request asks for
	 * video.
	 *
	 * @return the absolute directory, or null for none
	 */
	public @Nullable Path videoDir() {
		if (videoDirectory != null) {
			return videoDirectory;
		}
		return keepsVideo(evidenceRequest) ? environment.workingDirectory().resolve(DEFAULT_MEDIA_DIR) : null;
	}

	/**
	 * Returns the evidence request.
	 *
	 * @return {@code none} unless set
	 */
	public @Nonnull Evidence evidence() {
		return evidenceRequest;
	}

	/**
	 * Returns which runs keep their snapshots and video.
	 *
	 * @return {@link KeepMedia#ALL} unless set
	 */
	public @Nonnull KeepMedia keepMedia() {
		return mediaKept;
	}

	/**
	 * Returns the prefixes of the suite members to run.
	 *
	 * @return the prefixes (unmodifiable), or an empty list for all
	 */
	public @Nonnull List<String> suiteOnly() {
		return onlyPrefixes;
	}

	/**
	 * Returns the scripts and suites the generic runner runs.
	 *
	 * @return the classpath resources or file paths (unmodifiable), or an empty list for none
	 */
	public @Nonnull List<String> run() {
		return runScripts;
	}

	/**
	 * Returns the folder the device profiles go in.
	 *
	 * @return the absolute folder: the one set, else {@link #DEFAULT_PROFILES_DIR} under the working directory
	 */
	public @Nonnull Path profiles() {
		Path result = profilesDirectory;
		return (result == null) ? environment.workingDirectory().resolve(DEFAULT_PROFILES_DIR) : result;
	}

	/**
	 * Returns which runs keep their device profiles when they end.
	 *
	 * @return {@link KeepProfiles#FAILED} unless set
	 */
	public @Nonnull KeepProfiles profilesKeep() {
		return profilesKept;
	}

	/**
	 * Returns how many hours old a run's device profiles must be before a later run deletes them.
	 *
	 * @return above 0: {@value #DEFAULT_PROFILES_KEEP_HOURS} unless set
	 */
	public int profilesKeepHours() {
		return profilesHours;
	}

	/**
	 * Returns the number of virtual users of a load run.
	 *
	 * @return above 0, or 0 if none is set (not a load run)
	 */
	public int loadUsers() {
		return virtualUsers;
	}

	/**
	 * Returns how long a load run takes to start all its virtual users.
	 *
	 * @return zero unless set
	 */
	public @Nonnull Duration loadRampUp() {
		return rampUpTime;
	}

	/**
	 * Returns how long a load run starts new iterations for.
	 *
	 * @return the duration, or null if none is set
	 */
	public @Nullable Duration loadDuration() {
		return loadTime;
	}

	/**
	 * Returns how many iterations each virtual user of a load run runs.
	 *
	 * @return above 0, or 0 if none is set
	 */
	public int loadIterations() {
		return iterationCount;
	}

	/**
	 * Returns the longest a load run may take.
	 *
	 * @return the limit, or null if none is set
	 */
	public @Nullable Duration loadTimeLimit() {
		return loadLimit;
	}

	/**
	 * Returns the percentage of a load run's iterations that may fail before the run fails.
	 *
	 * @return 0 (any failure fails the run) unless set
	 */
	public double loadMaxFailureRate() {
		return failureRate;
	}

	/**
	 * Returns the longest that any interaction's 95th percentile duration may be in a load run.
	 *
	 * @return the limit, or null for none
	 */
	public @Nullable Duration loadMaxP95() {
		return p95Limit;
	}

	/**
	 * Returns the variables that every automation starts with.
	 *
	 * @return the values by variable name (without {@value #VAR_PREFIX}), sorted by name; a new map
	 */
	public @Nonnull Map<String, String> variables() {
		Map<String, String> result = new TreeMap<>();
		for (Setting variable : variables.values()) {
			result.put(variable.key().substring(VAR_PREFIX.length()), variable.value());
		}
		return result;
	}

	/**
	 * Returns a setting as resolved, with its source.
	 *
	 * @param key	e.g. {@value #TEST_STRATEGY} or {@value #VAR_PREFIX}{@code password}
	 * @return the setting, or null if it has its default
	 */
	public @Nullable Setting setting(@Nonnull String key) {
		return key.startsWith(VAR_PREFIX) ? variables.get(key) : settings.get(key);
	}

	/**
	 * Returns a setting if it comes from the run ({@code -D}, the environment, code, the properties file or a browser
	 * configuration), which overrides what the scripts say: for example a test strategy override.
	 *
	 * @param key	the setting's key
	 * @return the setting, or null if a script level or the default gives it
	 */
	@Nullable Setting runOverride(@Nonnull String key) {
		Setting result = setting(key);
		return ((result != null) && result.source().level().isRun()) ? result : null;
	}

	/**
	 * Returns the {@code sail.*} system properties ({@code -Dsail.x}) and {@code SAIL_*} environment variables that are
	 * not settings, and so are ignored; without the names other SAIL code reads.
	 *
	 * @return the names, system properties first (unmodifiable)
	 */
	@Nonnull List<String> ignored() {
		return Collections.unmodifiableList(ignored);
	}

	/**
	 * Describes the settings that do not come from their defaults, each with its source, for the trace and the media
	 * pages: e.g. {@code testStrategy=Verify (-Dsail.testStrategy)}. The settings come in the order of the keys, then
	 * the directories an evidence request makes (with the request as their source), then the variables, by name, with
	 * their values hidden, then a test strategy override that a lock kept from applying.
	 *
	 * @return one line per setting; empty if every setting has its default
	 */
	@Nonnull List<String> describe() {
		List<String> result = new ArrayList<>(settings.size() + variables.size() + 3);
		for (Setting setting : settings.values()) {
			result.add(setting.toString());
		}
		Setting request = settings.get(EVIDENCE);
		if (request != null) {
			String from = " (" + EVIDENCE.substring(PREFIX.length()) + ", " + request.source() + ')';
			if (failureDirectory == null) {
				describeDirectory(result, FAILURE_DIR, failureDir(), from);
			}
			if (snapshotDirectory == null) {
				describeDirectory(result, SNAPSHOT_DIR, snapshotDir(), from);
			}
			if (videoDirectory == null) {
				describeDirectory(result, VIDEO_DIR, videoDir(), from);
			}
		}
		for (Setting variable : variables.values()) {
			result.add(variable.toString());
		}
		Setting kept = ignoredOverride;
		if (kept != null) {
			result.add("testStrategy override " + kept.value() + " from " + kept.source() + " not applied: locked (" + lock + ')');
		}
		return result;
	}

	/** Indicates if an evidence request keeps failure screenshots: every request but {@code none}. */
	static boolean keepsFailures(@Nonnull Evidence request) {
		return request != Evidence.none;
	}

	/** Indicates if an evidence request keeps step snapshots: {@code snapshots} and {@code all}. */
	static boolean keepsSnapshots(@Nonnull Evidence request) {
		return (request == Evidence.snapshots) || (request == Evidence.all);
	}

	/** Indicates if an evidence request keeps a video: {@code video} and {@code all}. */
	static boolean keepsVideo(@Nonnull Evidence request) {
		return (request == Evidence.video) || (request == Evidence.all);
	}

	private static void describeDirectory(@Nonnull List<String> lines, @Nonnull String key, @Nullable Path directory, @Nonnull String from) {
		if (directory != null) {
			lines.add(key.substring(PREFIX.length()) + '=' + directory + from);
		}
	}
}

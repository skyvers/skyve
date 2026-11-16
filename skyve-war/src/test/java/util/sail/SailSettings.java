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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.metadata.sail.language.Automation.TestStrategy;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.BrowserConfiguration.Browsers;

/**
 * The settings of a SAIL run, resolved from every place a setting can come from, each value with its source.
 * <p>
 * <b>Precedence</b>, highest first: a {@code -D} system property, an environment variable, code (see {@link #code()}),
 * the user's properties file, the suite, the workflow, the automation, and the default. Code beats the properties
 * file so that a developer's quick change in a test takes effect even with a personal file; only an explicit
 * {@code -D} or environment variable for this run beats code. Of the script levels, the innermost explicit value
 * wins: an automation's own {@code testStrategy} beats its workflow's, which beats its suite's, as the outer ones
 * are defaults for the automations that state none.
 * <ul>
 * <li><b>Environment variables</b> are {@code SAIL_} and the upper snake case of the key after {@code sail.}:
 * 		{@code SAIL_BASE_URL}, {@code SAIL_TIMEOUT_SECONDS}, {@code SAIL_SUITE_ONLY}, {@code SAIL_VAR_PASSWORD}
 * 		(see {@link #environmentName(String)}).</li>
 * <li><b>The properties file</b> is the one {@value #PROPERTIES} names (from {@code -D}, the environment or code),
 * 		else {@code ~/.skyve/sail.properties} if it exists. It holds the same {@code sail.*} keys; a key it does not
 * 		know is refused, as it is most likely a typing error.</li>
 * <li><b>The script levels</b> set only what they own: an automation or workflow its {@code testStrategy}, a suite
 * 		its {@code testStrategy} and {@code evidence} (see {@link #automation}, {@link #workflow}, {@link #suite}).
 * 		The base URL is never in a script.</li>
 * </ul>
 * Resolution validates every effective value and fails before any browser starts, with an
 * {@link IllegalArgumentException} naming each invalid key, its value, its source and the valid values. A
 * {@code sail.*} system property or environment variable that is not a setting is ignored (other code has its own,
 * such as the server's {@code sail.recording}).
 * <p>
 * A resolved {@code SailSettings} is an immutable snapshot of the sources as they were when it was resolved.
 * {@link #describe()} lists the settings that do not come from their defaults, with their sources, and hides the
 * values of {@value #VAR_PREFIX}{@code *} variables.
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
	/** The evidence a run keeps when no directory is given for it; see {@link Evidence}. */
	public static final String EVIDENCE = "sail.evidence";
	/** Which runs keep their snapshots and video; see {@link KeepMedia}. */
	public static final String KEEP_MEDIA = "sail.keepMedia";
	/** The suite files to run, by prefix, comma separated, e.g. {@code 03,04}. */
	public static final String SUITE_ONLY = "sail.suite.only";
	/** The scripts and suites the generic runner runs: classpath resources or file paths, comma separated. */
	public static final String RUN = "sail.run";
	/** The prefix of a variable's key: {@code sail.var.password} is the value of {@code {var:password}}. */
	public static final String VAR_PREFIX = "sail.var.";
	/** The path of the user's properties file. */
	public static final String PROPERTIES = "sail.properties";

	/** The timeout for every wait on the browser when none is set. */
	public static final long DEFAULT_TIMEOUT_SECONDS = 30L;
	/** Where failure screenshots go when an evidence request asks for them and no directory is given. */
	public static final Path DEFAULT_FAILURE_DIR = Path.of("target", "sail", "failures");
	/** Where snapshots and video go when an evidence request asks for them and no directory is given. */
	public static final Path DEFAULT_MEDIA_DIR = Path.of("target", "sail", "media");

	/** The settings a properties file or {@link Code#set(String, String)} may hold, besides variables, in display order. */
	private static final List<String> KEYS = List.of(BASE_URL, HEADLESS, BROWSER, TIMEOUT_SECONDS, TEST_STRATEGY,
														FAILURE_DIR, SNAPSHOT_DIR, SNAPSHOT_SOURCE, VIDEO_DIR, EVIDENCE,
														KEEP_MEDIA, SUITE_ONLY, RUN, PROPERTIES);
	private static final String MASK = "********";
	/** The prefix of a variable's environment variable. */
	private static final String ENVIRONMENT_VAR_PREFIX = "SAIL_VAR_";

	/**
	 * The evidence a run keeps without naming directories for it. An explicitly set directory
	 * ({@value #FAILURE_DIR}, {@value #SNAPSHOT_DIR}, {@value #VIDEO_DIR}) always turns its kind on; a request adds the
	 * kinds it names, in {@link SailSettings#DEFAULT_FAILURE_DIR} and {@link SailSettings#DEFAULT_MEDIA_DIR} under the
	 * working directory.
	 */
	public enum Evidence {
		/** Nothing beyond the directories that are set. */
		NONE,
		/** A screenshot and the page source of each failure. */
		FAILURES,
		/** The failures, and a snapshot of every step with a film strip. */
		SNAPSHOTS,
		/** The failures, and a video of the run. */
		VIDEO,
		/** The failures, the snapshots and the video. */
		ALL;

		/**
		 * Indicates if this request keeps failure screenshots.
		 *
		 * @return true for every request but {@link #NONE}
		 */
		public boolean failures() {
			return this != NONE;
		}

		/**
		 * Indicates if this request keeps step snapshots.
		 *
		 * @return true for {@link #SNAPSHOTS} and {@link #ALL}
		 */
		public boolean snapshots() {
			return (this == SNAPSHOTS) || (this == ALL);
		}

		/**
		 * Indicates if this request keeps a video.
		 *
		 * @return true for {@link #VIDEO} and {@link #ALL}
		 */
		public boolean video() {
			return (this == VIDEO) || (this == ALL);
		}
	}

	/** Which automation runs keep their snapshot and video folders. */
	public enum KeepMedia {
		/** Every run keeps them. */
		ALL,
		/** A run that passed deletes them when it ends. */
		FAILED
	}

	/** Where a setting can come from, highest precedence first. */
	public enum Level {
		/** A {@code -D} system property. */
		SYSTEM_PROPERTY,
		/** An environment variable. */
		ENVIRONMENT,
		/** Code: {@link SailSettings#code()}, or a {@link BrowserConfiguration}'s own values. */
		CODE,
		/** The user's properties file. */
		FILE,
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
		 * @return true for {@code -D}, the environment, code and the properties file
		 */
		public boolean isRun() {
			return ordinal() <= FILE.ordinal();
		}
	}

	/**
	 * Where a resolved value came from.
	 *
	 * @param level	the level
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
	 * @param key	the key, e.g. {@code sail.testStrategy} or {@code sail.var.password}
	 * @param value	the value as given
	 * @param source	where it came from
	 */
	public record Setting(@Nonnull String key, @Nonnull String value, @Nonnull Source source) {
		/**
		 * Returns the value to show: the value, or a mask for a variable.
		 *
		 * @return the value or {@value SailSettings#MASK}
		 */
		public @Nonnull String shownValue() {
			return key.startsWith(VAR_PREFIX) ? MASK : value;
		}

		/**
		 * Describes the setting as {@code testStrategy=Verify (-Dsail.testStrategy)}, with a variable's value hidden.
		 */
		@Override
		public @Nonnull String toString() {
			return key.substring(PREFIX.length()) + '=' + shownValue() + " (" + source + ')';
		}
	}

	/**
	 * The sources outside the code and the scripts: system properties, environment variables, the user's home (for
	 * {@code ~/.skyve/sail.properties}) and the working directory (for the default evidence directories).
	 * Given explicitly, so that tests can resolve settings without changing the JVM's own.
	 *
	 * @param systemProperties	the system properties
	 * @param environment	the environment variables
	 * @param userHome	the user's home directory
	 * @param workingDirectory	the directory relative directories resolve against
	 */
	public record Environment(@Nonnull Map<String, String> systemProperties,
								@Nonnull Map<String, String> environment,
								@Nonnull Path userHome,
								@Nonnull Path workingDirectory) {
		/**
		 * Returns this JVM's sources as they are now: a copy of its {@code sail.*} system properties, and its
		 * environment.
		 *
		 * @return the sources
		 */
		public static @Nonnull Environment current() {
			Properties properties = System.getProperties();
			Map<String, String> sail = new TreeMap<>();
			for (String name : properties.stringPropertyNames()) {
				if (name.startsWith(PREFIX)) {
					sail.put(name, properties.getProperty(name));
				}
			}
			return new Environment(sail, System.getenv(), Path.of(System.getProperty("user.home")), Path.of("").toAbsolutePath());
		}
	}

	/**
	 * The values one source gives, at one level. Code layers come from {@link Code}; script layers from
	 * {@link SailSettings#automation}, {@link SailSettings#workflow} and {@link SailSettings#suite}, which allow only
	 * what each level owns.
	 */
	public static final class Layer {
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

		/**
		 * Returns this layer's level.
		 *
		 * @return the level
		 */
		public @Nonnull Level level() {
			return source.level();
		}

		/**
		 * Returns a value of this layer.
		 *
		 * @param key	the key
		 * @return the value, or null if this layer does not set it
		 */
		public @Nullable String get(@Nonnull String key) {
			return values.get(key);
		}

		/**
		 * Returns where a value of this layer comes from.
		 *
		 * @param key	the key
		 * @return the source, e.g. {@code -Dsail.testStrategy} for a system property
		 */
		@Nonnull Source sourceOf(@Nonnull String key) {
			String where = (wheres == null) ? null : wheres.get(key);
			return (where == null) ? source : new Source(source.level(), where);
		}
	}

	/**
	 * The code level: a fluent builder of the settings a test gives in code, e.g.
	 * {@code SailSettings.code().headless(false).timeoutSeconds(60).videoDir(Path.of("/tmp/v")).variable("password", "x")}.
	 * The test bases, {@link SailWorkflow} and {@link SailSuite} each have one, from {@code settings()}. A value set here
	 * beats the properties file and the scripts, but not a {@code -D} system property or an environment variable.
	 * Values are checked when the settings are resolved.
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
		 * @param value	the value, or null
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
		 * @param baseUrl	the URL, ending in {@code /}
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
		 * @param browser	the browser
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
		 * @param testStrategy	the strategy
		 * @return this builder
		 */
		public @Nonnull Code testStrategy(@Nonnull TestStrategy testStrategy) {
			return set(TEST_STRATEGY, testStrategy.name());
		}

		/**
		 * Sets the directory for failure screenshots and page sources.
		 *
		 * @param directory	the directory
		 * @return this builder
		 */
		public @Nonnull Code failureDir(@Nonnull Path directory) {
			return set(FAILURE_DIR, directory.toString());
		}

		/**
		 * Sets the directory for step snapshots and their film strip.
		 *
		 * @param directory	the directory
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
		 * @param directory	the directory
		 * @return this builder
		 */
		public @Nonnull Code videoDir(@Nonnull Path directory) {
			return set(VIDEO_DIR, directory.toString());
		}

		/**
		 * Requests evidence in the default directories; see {@link Evidence}.
		 *
		 * @param evidence	the request
		 * @return this builder
		 */
		public @Nonnull Code evidence(@Nonnull Evidence evidence) {
			return set(EVIDENCE, evidence.name().toLowerCase());
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
		 * Chooses the suite files to run, by prefix.
		 *
		 * @param prefixes	the prefixes, e.g. {@code "03", "04"}
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
		 * Defines a variable that every automation starts with, for {@code {var:name}}, e.g. a password.
		 *
		 * @param name	the variable's name
		 * @param value	the value
		 * @return this builder
		 */
		public @Nonnull Code variable(@Nonnull String name, @Nonnull String value) {
			return set(VAR_PREFIX + name, value);
		}

		/**
		 * Names the properties file to read instead of {@code ~/.skyve/sail.properties}.
		 *
		 * @param file	the file
		 * @return this builder
		 */
		public @Nonnull Code properties(@Nonnull Path file) {
			return set(PROPERTIES, file.toString());
		}

		/**
		 * Returns this builder's values as a layer with the source {@code code}.
		 *
		 * @return the layer, which shares this builder's values
		 */
		public @Nonnull Layer layer() {
			return layer("code");
		}

		/**
		 * Returns this builder's values as a code layer with the given source.
		 *
		 * @param where	the source, for messages
		 * @return the layer, which shares this builder's values
		 */
		@Nonnull Layer layer(@Nonnull String where) {
			return new Layer(Level.CODE, where, values);
		}

		/**
		 * Resolves the settings with this builder as the code level.
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
	/** The settings that do not come from their defaults, by key, in display order. */
	private final Map<String, Setting> settings = new LinkedHashMap<>();
	/** The variables, by name. */
	private final Map<String, Setting> variables = new TreeMap<>();
	private Path propertiesFile;

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
	private Evidence evidenceRequest = Evidence.NONE;
	private KeepMedia mediaKept = KeepMedia.ALL;
	private List<String> onlyPrefixes = List.of();
	private List<String> runScripts = List.of();

	private SailSettings(@Nonnull Environment environment, @Nonnull List<Layer> layers) {
		this.environment = environment;
		this.layers = layers;
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
	 * @return the browser; Chrome by default
	 */
	public @Nonnull Browsers browser() {
		return browserType;
	}

	/**
	 * Returns the timeout for every wait on the browser. It is a limit, not a delay.
	 *
	 * @return the timeout; {@value #DEFAULT_TIMEOUT_SECONDS} seconds by default
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
		return evidenceRequest.failures() ? environment.workingDirectory().resolve(DEFAULT_FAILURE_DIR) : null;
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
		return evidenceRequest.snapshots() ? environment.workingDirectory().resolve(DEFAULT_MEDIA_DIR) : null;
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
		return evidenceRequest.video() ? environment.workingDirectory().resolve(DEFAULT_MEDIA_DIR) : null;
	}

	/**
	 * Returns the evidence request.
	 *
	 * @return the request; {@link Evidence#NONE} by default
	 */
	public @Nonnull Evidence evidence() {
		return evidenceRequest;
	}

	/**
	 * Returns which runs keep their snapshots and video.
	 *
	 * @return {@link KeepMedia#ALL} by default
	 */
	public @Nonnull KeepMedia keepMedia() {
		return mediaKept;
	}

	/**
	 * Returns the prefixes of the suite files to run.
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
	 * Returns the properties file that was read.
	 *
	 * @return the file, or null if there was none
	 */
	public @Nullable Path propertiesFile() {
		return propertiesFile;
	}

	/**
	 * Returns a setting as resolved, with its source.
	 *
	 * @param key	the key, e.g. {@value #TEST_STRATEGY} or {@value #VAR_PREFIX}{@code password}
	 * @return the setting, or null if it has its default
	 */
	public @Nullable Setting setting(@Nonnull String key) {
		return key.startsWith(VAR_PREFIX) ? variables.get(key) : settings.get(key);
	}

	/**
	 * Returns a setting if it comes from the run ({@code -D}, the environment, code or the properties file), which
	 * overrides what the scripts say: for example a test strategy override.
	 *
	 * @param key	the key
	 * @return the setting, or null if a script level or the default gives it
	 */
	public @Nullable Setting runOverride(@Nonnull String key) {
		Setting result = setting(key);
		return ((result != null) && result.source().level().isRun()) ? result : null;
	}

	/**
	 * Describes the settings that do not come from their defaults, each with its source, for the trace and the media
	 * pages: e.g. {@code testStrategy=Verify (-Dsail.testStrategy)}. A variable's value is hidden. A directory that an
	 * evidence request makes is listed too, with the request as its source.
	 *
	 * @return one line per setting, in key order; empty if every setting has its default
	 */
	public @Nonnull List<String> describe() {
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
		return result;
	}

	private static void describeDirectory(@Nonnull List<String> lines, @Nonnull String key, @Nullable Path directory, @Nonnull String from) {
		if (directory != null) {
			lines.add(key.substring(PREFIX.length()) + '=' + directory + from);
		}
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
	public static @Nonnull Layer automation(@Nullable String name, @Nullable TestStrategy testStrategy) {
		return scriptLayer(Level.AUTOMATION, (name == null) ? "automation" : "automation " + name, testStrategy, null);
	}

	/**
	 * Makes a workflow's layer: its default test strategy for the automations that state none.
	 *
	 * @param name	the workflow, for messages
	 * @param testStrategy	the workflow's {@code testStrategy}, or null
	 * @return the layer
	 */
	public static @Nonnull Layer workflow(@Nonnull String name, @Nullable TestStrategy testStrategy) {
		return scriptLayer(Level.WORKFLOW, "workflow " + name, testStrategy, null);
	}

	/**
	 * Makes a suite's layer: its default test strategy and its evidence request.
	 *
	 * @param name	the suite, for messages
	 * @param testStrategy	the suite's {@code testStrategy}, or null
	 * @param evidence	the suite's {@code evidence}, or null
	 * @return the layer
	 */
	public static @Nonnull Layer suite(@Nonnull String name, @Nullable TestStrategy testStrategy, @Nullable Evidence evidence) {
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
			values.put(EVIDENCE, evidence.name().toLowerCase());
		}
		return new Layer(level, where, values);
	}

	/**
	 * Resolves the settings from this JVM's system properties and environment, and the given layers.
	 *
	 * @param layers	the code and script layers, in any order; layers of one level take precedence in the order given
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid, or a named properties file is missing or unreadable
	 */
	public static @Nonnull SailSettings resolve(@Nonnull Layer... layers) {
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
	public static @Nonnull SailSettings resolve(@Nonnull Environment environment, @Nonnull List<Layer> layers) {
		List<Layer> given = new ArrayList<>(layers);
		SailSettings result = new SailSettings(environment, given);
		List<Layer> ordered = new ArrayList<>(given.size() + 3);
		ordered.add(systemLayer(environment));
		ordered.add(environmentLayer(environment, given));
		ordered.addAll(given);
		// the file is found from the run levels above it
		Layer file = result.fileLayer(ordered);
		if (file != null) {
			ordered.add(file);
		}
		ordered.sort(Comparator.comparing(Layer::level)); // stable: a level's layers keep their order

		for (String key : KEYS) {
			for (Layer layer : ordered) {
				String value = layer.get(key);
				if ((value != null) && (! value.isBlank())) {
					result.settings.put(key, new Setting(key, value.trim(), layer.sourceOf(key)));
					break;
				}
			}
		}
		for (Layer layer : ordered) {
			for (Map.Entry<String, String> entry : layer.values.entrySet()) {
				String key = entry.getKey();
				if (key.startsWith(VAR_PREFIX) && (! result.variables.containsKey(key))) {
					// a value as given: a password may start or end with a space
					result.variables.put(key, new Setting(key, entry.getValue(), layer.sourceOf(key)));
				}
			}
		}
		result.parse();
		return result;
	}

	/**
	 * Returns these settings with more layers, resolved again from the same environment and layers. A code layer
	 * shares its builder's values, so they are its values as they are now.
	 *
	 * @param more	the layers to add, e.g. an automation's
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull SailSettings with(@Nonnull Layer... more) {
		List<Layer> all = new ArrayList<>(layers.size() + more.length);
		all.addAll(layers);
		all.addAll(Arrays.asList(more));
		return resolve(environment, all);
	}

	private static @Nonnull Layer systemLayer(@Nonnull Environment environment) {
		Map<String, String> values = new LinkedHashMap<>();
		Map<String, String> wheres = new LinkedHashMap<>();
		for (Map.Entry<String, String> property : environment.systemProperties().entrySet()) {
			String key = property.getKey();
			if (isSettingKey(key)) {
				values.put(key, property.getValue());
				wheres.put(key, "-D" + key);
			}
		}
		return new Layer(Level.SYSTEM_PROPERTY, "-D", values, wheres);
	}

	private static @Nonnull Layer environmentLayer(@Nonnull Environment environment, @Nonnull List<Layer> given) {
		Map<String, String> env = environment.environment();
		Map<String, String> values = new LinkedHashMap<>();
		Map<String, String> names = new LinkedHashMap<>();
		for (String key : KEYS) {
			String name = environmentName(key);
			String value = env.get(name);
			if (value != null) {
				values.put(key, value);
				names.put(key, name);
			}
		}
		Map<String, String> knownVariables = knownVariables(environment, given);
		for (Map.Entry<String, String> variable : env.entrySet()) {
			String name = variable.getKey();
			if (name.startsWith(ENVIRONMENT_VAR_PREFIX) && (name.length() > ENVIRONMENT_VAR_PREFIX.length())) {
				// a variable's name is known from another source, or made from the environment variable's
				String key = knownVariables.get(name);
				if (key == null) {
					key = VAR_PREFIX + camelCase(name.substring(ENVIRONMENT_VAR_PREFIX.length()));
				}
				values.put(key, variable.getValue());
				names.put(key, name);
			}
		}
		return new Layer(Level.ENVIRONMENT, "environment", values, names);
	}

	/** Returns the variable keys that the other sources name, by their environment variable's name. */
	private static @Nonnull Map<String, String> knownVariables(@Nonnull Environment environment, @Nonnull List<Layer> given) {
		Map<String, String> result = new TreeMap<>();
		for (Layer layer : given) {
			addVariableKeys(result, layer.values.keySet());
		}
		addVariableKeys(result, environment.systemProperties().keySet());
		return result;
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
	 * @param key	the key
	 * @return the environment variable's name
	 */
	public static @Nonnull String environmentName(@Nonnull String key) {
		String name = key.startsWith(PREFIX) ? key.substring(PREFIX.length()) : key;
		StringBuilder result = new StringBuilder(name.length() + 12).append("SAIL_");
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
	 * {@code ~/.skyve/sail.properties} if it exists.
	 *
	 * <p>Side effects: reads the file, and sets {@link #propertiesFile}.
	 *
	 * @param runLayers	the layers above the file, which may name it
	 * @throws IllegalArgumentException	if a named file is missing or unreadable, or holds a key that is not a setting
	 */
	private @Nullable Layer fileLayer(@Nonnull List<Layer> runLayers) {
		Setting named = null;
		List<Layer> above = new ArrayList<>(runLayers);
		above.sort(Comparator.comparing(Layer::level));
		for (Layer layer : above) {
			String value = layer.get(PROPERTIES);
			if ((value != null) && (! value.isBlank())) {
				named = new Setting(PROPERTIES, value.trim(), layer.sourceOf(PROPERTIES));
				break;
			}
		}
		Path file;
		if (named == null) {
			file = environment.userHome().resolve(".skyve").resolve("sail.properties");
			if (! Files.isRegularFile(file)) {
				return null;
			}
		}
		else {
			file = path(named.value(), "use a file path", named);
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
			throw new IllegalArgumentException(String.format("The SAIL properties file %s has keys that are not SAIL settings: %s; use %s or %s<name>",
																file, unknown, KEYS.subList(0, KEYS.size() - 1), VAR_PREFIX));
		}
		propertiesFile = file;
		return new Layer(Level.FILE, file.toString(), values);
	}

	/**
	 * Indicates if a key is a SAIL setting or variable.
	 *
	 * @param key	the key
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
			case FAILURE_DIR -> failureDirectory = path(value, "use a directory path", setting);
			case SNAPSHOT_DIR -> snapshotDirectory = path(value, "use a directory path", setting);
			case SNAPSHOT_SOURCE -> withPageSources = bool(value);
			case VIDEO_DIR -> videoDirectory = path(value, "use a directory path", setting);
			case EVIDENCE -> evidenceRequest = constant(value, Evidence.values());
			case KEEP_MEDIA -> mediaKept = constant(value, KeepMedia.values());
			case SUITE_ONLY -> onlyPrefixes = list(value);
			case RUN -> runScripts = list(value);
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

	/** Resolves a path against the working directory. */
	private @Nonnull Path path(@Nonnull String value, @Nonnull String valid, @Nonnull Setting setting) {
		try {
			return environment.workingDirectory().resolve(value);
		}
		catch (InvalidPathException e) {
			throw new IllegalArgumentException("Invalid SAIL setting: " + describeProblem(setting, valid + " (" + e.getReason() + ')'), e);
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
}

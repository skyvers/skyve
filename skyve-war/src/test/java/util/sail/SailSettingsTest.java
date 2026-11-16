package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.metadata.sail.language.Automation.TestStrategy;

import util.sail.BrowserConfiguration.Browsers;
import util.sail.SailSettings.Environment;
import util.sail.SailSettings.Evidence;
import util.sail.SailSettings.KeepMedia;
import util.sail.SailSettings.Layer;
import util.sail.SailSettings.Level;
import util.sail.SailSettings.Setting;

/**
 * Tests the resolution of SAIL settings: the precedence of every level, environment variable names, the properties
 * file, validation, masking and the code level. The system properties, environment and home directory are given to
 * each resolution, so no test changes the JVM's own.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailSettingsTest {
	@TempDir
	Path directory;

	private final Map<String, String> system = new HashMap<>();
	private final Map<String, String> env = new HashMap<>();

	private SailSettings resolve(Layer... layers) {
		return SailSettings.resolve(new Environment(system, env, directory.resolve("home"), directory), List.of(layers));
	}

	private Path userFile(String content) throws IOException {
		Path file = directory.resolve("home").resolve(".skyve").resolve("sail.properties");
		Files.createDirectories(file.getParent());
		return Files.writeString(file, content);
	}

	/** Returns where a setting came from, or "default". */
	private static String source(SailSettings settings, String key) {
		Setting setting = settings.setting(key);
		return (setting == null) ? "default" : setting.source().toString();
	}

	/** Returns the level a setting came from. */
	private static Level level(SailSettings settings, String key) {
		Setting setting = settings.setting(key);
		return (setting == null) ? Level.DEFAULT : setting.source().level();
	}

	/** Returns the level a run override came from, or null if there is none. */
	private static Level overrideLevel(SailSettings settings, String key) {
		Setting setting = settings.runOverride(key);
		return (setting == null) ? null : setting.source().level();
	}

	@Test
	void theDefaultsApplyWhenNothingIsSet() {
		SailSettings settings = resolve();
		assertAll(() -> assertNull(settings.baseUrl()),
					() -> assertTrue(settings.headless()),
					() -> assertEquals(Browsers.chrome, settings.browser()),
					() -> assertEquals(Duration.ofSeconds(30), settings.timeout()),
					() -> assertNull(settings.testStrategy()),
					() -> assertNull(settings.failureDir()),
					() -> assertNull(settings.snapshotDir()),
					() -> assertFalse(settings.snapshotSource()),
					() -> assertNull(settings.videoDir()),
					() -> assertEquals(Evidence.NONE, settings.evidence()),
					() -> assertEquals(KeepMedia.ALL, settings.keepMedia()),
					() -> assertEquals(List.of(), settings.suiteOnly()),
					() -> assertEquals(List.of(), settings.run()),
					() -> assertEquals(Map.of(), settings.variables()),
					() -> assertNull(settings.propertiesFile()),
					() -> assertEquals(List.of(), settings.describe()));
	}

	/**
	 * Each run level is set, then the highest is taken away in turn: -D, then the environment, then code, then the
	 * properties file, then the default.
	 */
	@Test
	void theRunLevelsTakePrecedenceInOrder() throws IOException {
		userFile("sail.timeoutSeconds=4\n");
		SailSettings.Code code = SailSettings.code().timeoutSeconds(3);
		env.put("SAIL_TIMEOUT_SECONDS", "2");
		system.put(SailSettings.TIMEOUT_SECONDS, "1");
		List<String> sources = new ArrayList<>();
		List<Long> seconds = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			SailSettings settings = resolve(code.layer());
			sources.add(source(settings, SailSettings.TIMEOUT_SECONDS));
			seconds.add(Long.valueOf(settings.timeout().toSeconds()));
			switch (i) {
				case 0 -> system.clear();
				case 1 -> env.clear();
				case 2 -> code.set(SailSettings.TIMEOUT_SECONDS, null);
				case 3 -> Files.delete(directory.resolve("home/.skyve/sail.properties"));
				default -> {
					// the default is left
				}
			}
		}
		String file = directory.resolve("home/.skyve/sail.properties").toString();
		assertAll(() -> assertEquals(List.of("-Dsail.timeoutSeconds", "SAIL_TIMEOUT_SECONDS", "code", file, "default"), sources),
					() -> assertEquals(List.of(Long.valueOf(1), Long.valueOf(2), Long.valueOf(3), Long.valueOf(4), Long.valueOf(30)), seconds));
	}

	/**
	 * Of the script levels, the automation's own strategy beats its workflow's default, which beats its suite's; the
	 * properties file (a run level) beats them all.
	 */
	@Test
	void theInnermostScriptLevelWinsAndTheRunBeatsTheScripts() throws IOException {
		Layer suite = SailSettings.suite("s.xml", TestStrategy.None, Evidence.VIDEO);
		Layer workflow = SailSettings.workflow("w.xml", TestStrategy.Verify);
		Layer automation = SailSettings.automation("a.xml", TestStrategy.Assert);
		SailSettings all = resolve(suite, workflow, automation);
		SailSettings noAutomation = resolve(suite, workflow, SailSettings.automation("a.xml", null));
		SailSettings suiteOnly = resolve(suite);
		userFile("sail.testStrategy=verify\nsail.evidence=snapshots\n");
		SailSettings file = resolve(suite, workflow, automation);
		assertAll(() -> assertEquals(TestStrategy.Assert, all.testStrategy()),
					() -> assertEquals("automation a.xml", source(all, SailSettings.TEST_STRATEGY)),
					() -> assertEquals(Level.AUTOMATION, level(all, SailSettings.TEST_STRATEGY)),
					() -> assertNull(all.runOverride(SailSettings.TEST_STRATEGY), "a script's strategy is not an override"),
					() -> assertEquals(Evidence.VIDEO, all.evidence()),
					() -> assertEquals(TestStrategy.Verify, noAutomation.testStrategy()),
					() -> assertEquals("workflow w.xml", source(noAutomation, SailSettings.TEST_STRATEGY)),
					() -> assertEquals(TestStrategy.None, suiteOnly.testStrategy()),
					() -> assertEquals("suite s.xml", source(suiteOnly, SailSettings.TEST_STRATEGY)),
					() -> assertEquals(TestStrategy.Verify, file.testStrategy()),
					() -> assertEquals(Level.FILE, overrideLevel(file, SailSettings.TEST_STRATEGY)),
					() -> assertEquals(Evidence.SNAPSHOTS, file.evidence()));
	}

	/** Code beats a browser configuration's own values, which are code too, so that a test's settings() rule its devices. */
	@Test
	void aTestsCodeBeatsABrowserConfigurationsOwnValues() {
		SailSettings.Code test = SailSettings.code().browser(Browsers.firefox);
		SailSettings.Code configuration = SailSettings.code().browser(Browsers.edge).headless(false);
		SailSettings settings = resolve(test.layer(), configuration.layer("code: BrowserConfiguration"));
		assertAll(() -> assertEquals(Browsers.firefox, settings.browser()),
					() -> assertEquals("code", source(settings, SailSettings.BROWSER)),
					() -> assertFalse(settings.headless()),
					() -> assertEquals("code: BrowserConfiguration", source(settings, SailSettings.HEADLESS)));
	}

	@Test
	void aBrowserConfigurationResolvesTheTestsSettingsBeforeItsOwn() {
		BrowserConfiguration configuration = new BrowserConfiguration().browser(Browsers.edge);
		Browsers own = configuration.getBrowser();
		configuration.settings(SailSettings.code().browser(Browsers.safari));
		assertAll(() -> assertEquals(Browsers.edge, own),
					() -> assertEquals(Browsers.safari, configuration.getBrowser()));
	}

	@Test
	void environmentVariablesAreSailAndTheUpperSnakeCaseOfTheKey() {
		assertAll(() -> assertEquals("SAIL_BASE_URL", SailSettings.environmentName(SailSettings.BASE_URL)),
					() -> assertEquals("SAIL_TIMEOUT_SECONDS", SailSettings.environmentName(SailSettings.TIMEOUT_SECONDS)),
					() -> assertEquals("SAIL_SUITE_ONLY", SailSettings.environmentName(SailSettings.SUITE_ONLY)),
					() -> assertEquals("SAIL_KEEP_MEDIA", SailSettings.environmentName(SailSettings.KEEP_MEDIA)),
					() -> assertEquals("SAIL_PROPERTIES", SailSettings.environmentName(SailSettings.PROPERTIES)),
					() -> assertEquals("SAIL_VAR_PASSWORD", SailSettings.environmentName(SailSettings.VAR_PREFIX + "password")),
					() -> assertEquals("SAIL_VAR_ADMIN_PASSWORD", SailSettings.environmentName(SailSettings.VAR_PREFIX + "adminPassword")));
	}

	/**
	 * An environment variable sets its key; a variable's name comes from another source that names it, or else is the
	 * lower camel case of the environment variable's.
	 */
	@Test
	void environmentVariablesSetTheirKeysAndVariables() {
		env.put("SAIL_BASE_URL", "http://localhost:8520/skyve/");
		env.put("SAIL_SUITE_ONLY", "03, 04");
		env.put("SAIL_VAR_ADMIN_PASSWORD", "camel");
		env.put("SAIL_VAR_USER_NAME", "snake");
		env.put("SAIL_UNKNOWN", "ignored");
		env.put("PATH", "/bin");
		SailSettings settings = resolve(SailSettings.code().variable("user_name", "code").layer());
		assertAll(() -> assertEquals("http://localhost:8520/skyve/", settings.baseUrl()),
					() -> assertEquals("SAIL_BASE_URL", source(settings, SailSettings.BASE_URL)),
					() -> assertEquals(List.of("03", "04"), settings.suiteOnly()),
					() -> assertEquals(Map.of("adminPassword", "camel", "user_name", "snake"), settings.variables()),
					() -> assertEquals("SAIL_VAR_USER_NAME", source(settings, SailSettings.VAR_PREFIX + "user_name")));
	}

	@Test
	void sailPropertiesThatAreNotSettingsAreIgnored() {
		system.put("sail.recording", "true");
		system.put("sail.personas", "/tmp/p.tsv");
		assertEquals(List.of(), resolve().describe());
	}

	@Test
	void theUserPropertiesFileIsReadFromTheHomeDirectory() throws IOException {
		Path file = userFile("sail.headless=false\nsail.var.password=secret\n");
		SailSettings settings = resolve();
		assertAll(() -> assertFalse(settings.headless()),
					() -> assertEquals(file, settings.propertiesFile()),
					() -> assertEquals(file.toString(), source(settings, SailSettings.HEADLESS)),
					() -> assertEquals(Map.of("password", "secret"), settings.variables()));
	}

	/** -D, the environment or code can name another file, which replaces the one in the home directory. */
	@Test
	void aNamedPropertiesFileReplacesTheUsersOne() throws IOException {
		userFile("sail.headless=false\n");
		Path named = Files.writeString(directory.resolve("ci.properties"), "sail.timeoutSeconds=90\n");
		SailSettings fromCode = resolve(SailSettings.code().properties(Path.of("ci.properties")).layer());
		env.put("SAIL_PROPERTIES", named.toString());
		SailSettings fromEnvironment = resolve();
		assertAll(() -> assertEquals(named, fromCode.propertiesFile()),
					() -> assertEquals(Duration.ofSeconds(90), fromCode.timeout()),
					() -> assertTrue(fromCode.headless(), "the user's file is not read"),
					() -> assertEquals(named, fromEnvironment.propertiesFile()),
					() -> assertEquals("SAIL_PROPERTIES", source(fromEnvironment, SailSettings.PROPERTIES)));
	}

	@Test
	void aMissingNamedPropertiesFileFails() {
		system.put(SailSettings.PROPERTIES, "no-such.properties");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, this::resolve);
		assertEquals("Invalid SAIL setting: sail.properties=no-such.properties (-Dsail.properties): there is no such file", e.getMessage());
	}

	@Test
	void aPropertiesFileWithAKeyThatIsNotASettingFails() throws IOException {
		Path file = userFile("sail.timeoutSecs=5\nsail.headless=true\nsail.properties=other\n");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, this::resolve);
		assertAll(() -> assertTrue(e.getMessage().startsWith("The SAIL properties file " + file +
																" has keys that are not SAIL settings: [sail.properties, sail.timeoutSecs]"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("sail.timeoutSeconds"), "lists the valid keys: " + e.getMessage()));
	}

	/** Every invalid value is listed, each with its key, value, source and the valid values. */
	@Test
	void invalidValuesFailWithTheirKeySourceAndValidValues() throws IOException {
		system.put(SailSettings.TIMEOUT_SECONDS, "soon");
		env.put("SAIL_HEADLESS", "yes");
		userFile("sail.browser=opera\n");
		SailSettings.Code code = SailSettings.code().set(SailSettings.TEST_STRATEGY, "Check").set(SailSettings.EVIDENCE, "some")
										.set(SailSettings.KEEP_MEDIA, "passed").baseUrl("localhost:8080").set(SailSettings.SUITE_ONLY, " , ");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> resolve(code.layer()));
		String file = directory.resolve("home/.skyve/sail.properties").toString();
		assertEquals("Invalid SAIL settings:\n" +
						"  sail.baseUrl=localhost:8080 (code): use an http or https URL ending in /, such as http://localhost:8080/skyve/\n" +
						"  sail.headless=yes (SAIL_HEADLESS): use true or false\n" +
						"  sail.browser=opera (" + file + "): use one of chrome, firefox, safari, edge (in any case)\n" +
						"  sail.timeoutSeconds=soon (-Dsail.timeoutSeconds): use a whole number of seconds above 0\n" +
						"  sail.testStrategy=Check (code): use one of Assert, Verify, None (in any case)\n" +
						"  sail.evidence=some (code): use one of none, failures, snapshots, video, all (in any case)\n" +
						"  sail.keepMedia=passed (code): use one of all, failed (in any case)\n" +
						"  sail.suite.only=, (code): use a comma-separated list",
						e.getMessage());
	}

	@Test
	void oneInvalidValueFailsOnOneLine() {
		system.put(SailSettings.TIMEOUT_SECONDS, "0");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, this::resolve);
		assertEquals("Invalid SAIL setting: sail.timeoutSeconds=0 (-Dsail.timeoutSeconds): use a whole number of seconds above 0", e.getMessage());
	}

	@Test
	void valuesIgnoreCaseAndSpaces() {
		system.put(SailSettings.TEST_STRATEGY, " verify ");
		system.put(SailSettings.BROWSER, "FireFox");
		system.put(SailSettings.HEADLESS, "FALSE");
		system.put(SailSettings.EVIDENCE, "Video");
		SailSettings settings = resolve();
		assertAll(() -> assertEquals(TestStrategy.Verify, settings.testStrategy()),
					() -> assertEquals(Browsers.firefox, settings.browser()),
					() -> assertFalse(settings.headless()),
					() -> assertEquals(Evidence.VIDEO, settings.evidence()));
	}

	@Test
	void codeCanSetEveryOption() {
		SailSettings settings = resolve(SailSettings.code()
												.baseUrl("https://example.test/skyve/")
												.headless(false)
												.browser(Browsers.edge)
												.timeoutSeconds(60)
												.testStrategy(TestStrategy.None)
												.failureDir(Path.of("f"))
												.snapshotDir(Path.of("s"))
												.snapshotSource(true)
												.videoDir(Path.of("/v"))
												.evidence(Evidence.ALL)
												.keepMedia(KeepMedia.FAILED)
												.suiteOnly("03", "04")
												.run("a.xml", "b.xml")
												.variable("password", "x")
												.layer());
		assertAll(() -> assertEquals("https://example.test/skyve/", settings.baseUrl()),
					() -> assertFalse(settings.headless()),
					() -> assertEquals(Browsers.edge, settings.browser()),
					() -> assertEquals(Duration.ofSeconds(60), settings.timeout()),
					() -> assertEquals(TestStrategy.None, settings.testStrategy()),
					() -> assertEquals(Level.CODE, overrideLevel(settings, SailSettings.TEST_STRATEGY)),
					() -> assertEquals(directory.resolve("f"), settings.failureDir()),
					() -> assertEquals(directory.resolve("s"), settings.snapshotDir()),
					() -> assertTrue(settings.snapshotSource()),
					() -> assertEquals(Path.of("/v"), settings.videoDir()),
					() -> assertEquals(Evidence.ALL, settings.evidence()),
					() -> assertEquals(KeepMedia.FAILED, settings.keepMedia()),
					() -> assertEquals(List.of("03", "04"), settings.suiteOnly()),
					() -> assertEquals(List.of("a.xml", "b.xml"), settings.run()),
					() -> assertEquals(Map.of("password", "x"), settings.variables()));
	}

	@Test
	void codeRefusesAKeyThatIsNotASetting() {
		SailSettings.Code code = SailSettings.code();
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> code.set("sail.timeoutSecs", "5"));
		assertTrue(e.getMessage().startsWith("sail.timeoutSecs is not a SAIL setting: use one of [sail.baseUrl,"), e.getMessage());
	}

	/** A variable set in code is overridden by -D, and the description hides every variable's value. */
	@Test
	void variablesFollowThePrecedenceAndAreMasked() {
		system.put(SailSettings.VAR_PREFIX + "password", "from-d");
		SailSettings settings = resolve(SailSettings.code().variable("password", "from-code").variable("user", "u").layer());
		List<String> described = settings.describe();
		assertAll(() -> assertEquals(Map.of("password", "from-d", "user", "u"), settings.variables()),
					() -> assertEquals(List.of("var.password=******** (-Dsail.var.password)", "var.user=******** (code)"), described),
					() -> assertFalse(described.toString().contains("from-d"), described.toString()),
					() -> assertEquals("var.user=******** (code)", String.valueOf(settings.setting(SailSettings.VAR_PREFIX + "user"))));
	}

	/** The description lists only what does not come from a default, with each source, in key order. */
	@Test
	void theDescriptionListsTheSettingsThatAreNotDefaultsWithTheirSources() {
		system.put(SailSettings.TEST_STRATEGY, "Verify");
		env.put("SAIL_BASE_URL", "http://localhost:8520/skyve/");
		SailSettings settings = resolve(SailSettings.code().headless(false).layer(), SailSettings.automation(null, TestStrategy.Assert));
		assertEquals(List.of("baseUrl=http://localhost:8520/skyve/ (SAIL_BASE_URL)",
								"headless=false (code)",
								"testStrategy=Verify (-Dsail.testStrategy)"),
						settings.describe());
	}

	/** An evidence request turns on the kinds it names in target/sail; a directory that is set wins, and turns its kind on. */
	@Test
	void anEvidenceRequestUsesTheDefaultDirectoriesUnlessOneIsSet() {
		SailSettings all = resolve(SailSettings.suite("s.xml", null, Evidence.ALL));
		SailSettings failures = resolve(SailSettings.code().evidence(Evidence.FAILURES).videoDir(Path.of("v")).layer());
		SailSettings snapshots = resolve(SailSettings.code().evidence(Evidence.SNAPSHOTS).layer());
		Path media = directory.resolve("target/sail/media");
		assertAll(() -> assertEquals(directory.resolve("target/sail/failures"), all.failureDir()),
					() -> assertEquals(media, all.snapshotDir()),
					() -> assertEquals(media, all.videoDir()),
					() -> assertEquals(List.of("evidence=all (suite s.xml)",
												"failureDir=" + directory.resolve("target/sail/failures") + " (evidence, suite s.xml)",
												"snapshotDir=" + media + " (evidence, suite s.xml)",
												"videoDir=" + media + " (evidence, suite s.xml)"),
										all.describe()),
					() -> assertEquals(directory.resolve("target/sail/failures"), failures.failureDir()),
					() -> assertNull(failures.snapshotDir()),
					() -> assertEquals(directory.resolve("v"), failures.videoDir(), "a directory that is set turns its kind on"),
					() -> assertEquals(media, snapshots.snapshotDir()),
					() -> assertNull(snapshots.videoDir()));
	}

	@Test
	void withAddsLayersToTheSameSources() {
		system.put(SailSettings.TIMEOUT_SECONDS, "5");
		SailSettings settings = resolve().with(SailSettings.automation("a.xml", TestStrategy.Verify));
		assertAll(() -> assertEquals(TestStrategy.Verify, settings.testStrategy()),
					() -> assertEquals(Duration.ofSeconds(5), settings.timeout()));
	}

	@Test
	void aSuiteRefusesACodeOverrideOfTheTestStrategyNamingItsSource() {
		SailSettings settings = resolve(SailSettings.code().testStrategy(TestStrategy.None).layer());
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> SailSuite.refuseTestStrategyOverride("The suite", settings));
		assertEquals("The suite runs with its scripts' own test strategies: unset sail.testStrategy (it is None, from code)", e.getMessage());
	}

	@Test
	void aSuiteAcceptsAScriptsOwnTestStrategy() {
		SailSettings settings = resolve(SailSettings.automation("a.xml", TestStrategy.None));
		assertDoesNotThrow(() -> SailSuite.refuseTestStrategyOverride("The suite", settings));
	}
}

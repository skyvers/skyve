package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.step.interaction.session.Login;

/**
 * Unit tests of the SAIL harness pieces that need no browser: the test strategy override, password masking in step
 * descriptions, the suite runner's restore gating, and the cross-JVM server lock. Settings resolve from given sources,
 * never this JVM's system properties, environment or properties file.
 */
class SailHarnessTest {
	@AfterEach
	@SuppressWarnings("static-method")
	void clearProperties() {
		SailServerLock.releaseAll();
	}

	@Test
	@SuppressWarnings("static-method")
	void theTestStrategyOverrideIgnoresCaseAndRejectsUnknownNames() {
		assertNull(InterpretedWebDriverExecutor.testStrategyOverride(SailSettings.Environment.none()));
		assertEquals(TestStrategy.Verify, InterpretedWebDriverExecutor.testStrategyOverride(sources(Map.of(SailSettings.TEST_STRATEGY, " verify "))));
		SailSettings.Environment check = sources(Map.of(SailSettings.TEST_STRATEGY, "Check"));
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> InterpretedWebDriverExecutor.testStrategyOverride(check));
		assertTrue(e.getMessage().contains("Assert") && e.getMessage().contains("Verify") && e.getMessage().contains("None"), e.getMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	void stepDescriptionsMaskPasswords() {
		Login login = new Login();
		login.setUser("admin");
		login.setPassword("secret");
		String description = InterpretedWebDriverExecutor.describe(login);
		assertAll(() -> assertFalse(description.contains("secret"), description),
					() -> assertTrue(description.contains("user=\"admin\""), description));
	}

	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theSuiteRefusesTheTestStrategyOverride() {
		List<String> ran = new ArrayList<>();
		SailSuite suite = new SailSuite("s/").setUp("00-pre.xml").files("01.xml").restore("99.xml", "00-pre.xml")
										.environment(sources(Map.of(SailSettings.TEST_STRATEGY, "None")));
		assertThrows(IllegalStateException.class, () -> suite.run(ran::add));
		assertTrue(ran.isEmpty(), ran.toString());
	}

	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theRestoreRunsAfterALaterFailureButNotAfterAFailedPrecondition() {
		List<String> ran = new ArrayList<>();
		SailSuite suite = new SailSuite("s/").setUp("00-pre.xml", "00-backup.xml").files("01.xml", "02.xml").restore("99.xml", "00-pre.xml")
										.environment(SailSettings.Environment.none());
		assertThrows(AssertionError.class, () -> suite.run(file -> {
			ran.add(file);
			if (file.endsWith("01.xml")) {
				throw new IllegalArgumentException("not a valid script");
			}
		}));
		assertEquals(List.of("s/00-pre.xml", "s/00-backup.xml", "s/01.xml", "s/99.xml"), ran);

		ran.clear();
		assertThrows(AssertionError.class, () -> suite.run(file -> {
			ran.add(file);
			if (file.endsWith("00-pre.xml")) {
				throw new AssertionError("a stray backup");
			}
		}));
		assertEquals(List.of("s/00-pre.xml"), ran, "the restore is held back");
	}

	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void onlyRunsTheChosenFilesBetweenTheSetUpAndTheRestore() {
		List<String> ran = new ArrayList<>();
		new SailSuite("s/").setUp("00-pre.xml").files("01.xml", "02.xml").restore("99.xml", "00-pre.xml")
							.environment(sources(Map.of(SailSettings.SUITE_ONLY, "02"))).run(ran::add);
		assertEquals(List.of("s/00-pre.xml", "s/02.xml", "s/99.xml"), ran);
	}

	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theSuitesSettingsInCodeChooseTheFiles() {
		List<String> ran = new ArrayList<>();
		SailSettings.Code code = SailSettings.code().suiteOnly("01");
		new SailSuite("s/").setUp("00-pre.xml").files("01.xml", "02.xml").restore("99.xml", "00-pre.xml").settings(code)
							.environment(SailSettings.Environment.none()).run(ran::add);
		assertEquals(List.of("s/00-pre.xml", "s/01.xml", "s/99.xml"), ran);
	}

	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theSuiteRefusesATestStrategyOverrideInCode() {
		List<String> ran = new ArrayList<>();
		SailSuite suite = new SailSuite("s/").setUp("00-pre.xml").files("01.xml").environment(SailSettings.Environment.none());
		suite.settings().testStrategy(TestStrategy.Verify);
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> suite.run(ran::add));
		assertAll(() -> assertTrue(e.getMessage().contains("unset sail.testStrategy (it is Verify, from code)"), e.getMessage()),
					() -> assertTrue(ran.isEmpty(), ran.toString()));
	}

	@Test
	@SuppressWarnings("static-method")
	void theServerLockHoldsAcrossJvms(@TempDir Path directory) throws IOException, InterruptedException {
		String baseUrl = "http://localhost:1/sail-lock-test-" + ProcessHandle.current().pid() + "/";
		SailServerLock.acquire(baseUrl);
		SailServerLock.acquire(baseUrl); // again in this JVM: no wait
		Path probe = directory.resolve("Probe.java");
		Files.writeString(probe, """
				import java.nio.channels.FileChannel;
				import java.nio.file.Path;
				import java.nio.file.StandardOpenOption;
				public class Probe {
					public static void main(String[] args) throws Exception {
						try (FileChannel c = FileChannel.open(Path.of(args[0]), StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
							System.exit((c.tryLock() == null) ? 3 : 0);
						}
					}
				}
				""");
		// the lock's key: the base URL without its trailing slash, with each run of other characters as _
		String key = baseUrl.substring(0, baseUrl.length() - 1).toLowerCase().replaceAll("[^a-z0-9]+", "_");
		Path lockFile = Path.of(System.getProperty("java.io.tmpdir"), "skyve-sail-" + key + ".lock");
		String java = Path.of(System.getProperty("java.home"), "bin", "java").toString();
		assertEquals(3, run(java, probe.toString(), lockFile.toString()), "another JVM cannot take the lock while this one holds it");
		SailServerLock.releaseAll();
		assertEquals(0, run(java, probe.toString(), lockFile.toString()), "another JVM can take it once released");
	}

	/**
	 * The next script's settings restart the browser only for a browser-level change (browser, headless mode, server,
	 * user agent, window size); a new timeout applies to the running browser.
	 */
	@Test
	@SuppressWarnings("static-method")
	void onlyABrowserLevelChangeRestartsTheBrowser() {
		BrowserConfiguration configuration = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/");
		PrimeFacesSelenide selenide = new PrimeFacesSelenide();
		selenide.settings = configuration.resolve(SailSettings.Environment.none());
		org.openqa.selenium.WebDriver driver = mock(org.openqa.selenium.WebDriver.class, RETURNS_DEEP_STUBS);
		selenide.driver = driver;
		SailSettings longer = configuration.settings(SailSettings.code().timeoutSeconds(90)).resolve(SailSettings.Environment.none());
		String sameBrowser = selenide.browserChange(longer, configuration);
		selenide.useSettings(longer, configuration); // no change: no browser is started or stopped
		SailSettings headed = configuration.settings(SailSettings.code().headless(false)).resolve(SailSettings.Environment.none());
		String change = selenide.browserChange(headed, configuration.userAgentString("UA").resolution(10, 20));
		assertAll(() -> assertNull(sameBrowser),
					() -> assertEquals(java.time.Duration.ofSeconds(90), selenide.timeout()),
					() -> verify(driver.manage().timeouts()).pageLoadTimeout(java.time.Duration.ofSeconds(90)),
					() -> assertEquals("headless true -> false, user agent null -> UA, resolution null -> (10, 20)", change));
	}

	/** The older builder holds the restore back when its requires names a file that is not in the suite, as it did. */
	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theOlderBuildersRestoreRequiringAnUnknownFileIsHeldBack() {
		List<String> ran = new ArrayList<>();
		new SailSuite("s/").setUp("00-pre.xml").files("01.xml").restore("99.xml", "00-typo.xml")
							.environment(SailSettings.Environment.none()).run(ran::add);
		assertEquals(List.of("s/00-pre.xml", "s/01.xml"), ran);
	}

	/** The older builder runs its set-ups and restore when sail.suite.only matches no file, as it did. */
	@Test
	@SuppressWarnings({"static-method", "deprecation"}) // the older SailSuite builder, kept working on the new engine
	void theOlderBuilderRunsItsBracketWhenOnlyMatchesNoFile() {
		List<String> ran = new ArrayList<>();
		new SailSuite("s/").setUp("00-pre.xml").files("01.xml").restore("99.xml", "00-pre.xml")
							.environment(sources(Map.of(SailSettings.SUITE_ONLY, "07"))).run(ran::add);
		assertEquals(List.of("s/00-pre.xml", "s/99.xml"), ran);
	}

	/** Sources with only the given system properties: no environment variables or properties file. */
	private static SailSettings.Environment sources(Map<String, String> systemProperties) {
		return new SailSettings.Environment(systemProperties, Map.of(), null, Path.of("").toAbsolutePath());
	}

	private static int run(String... command) throws IOException, InterruptedException {
		Process process = new ProcessBuilder(command).inheritIO().start();
		if (! process.waitFor(60, TimeUnit.SECONDS)) {
			process.destroyForcibly();
			throw new IllegalStateException("The probe JVM did not finish");
		}
		return process.exitValue();
	}
}

package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.step.interaction.session.Login;

/**
 * Unit tests of the SAIL harness pieces that need no browser: the test strategy override, password masking in step
 * descriptions, the suite runner's restore gating, and the cross-JVM server lock.
 */
class SailHarnessTest {
	@AfterEach
	@SuppressWarnings("static-method")
	void clearProperties() {
		System.clearProperty(InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY);
		System.clearProperty(SailSuite.ONLY_PROPERTY);
		SailServerLock.releaseAll();
	}

	@Test
	@SuppressWarnings("static-method")
	void theTestStrategyOverrideIgnoresCaseAndRejectsUnknownNames() {
		assertNull(InterpretedWebDriverExecutor.testStrategyOverride());
		System.setProperty(InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY, " verify ");
		assertEquals(TestStrategy.Verify, InterpretedWebDriverExecutor.testStrategyOverride());
		System.setProperty(InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY, "Check");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, InterpretedWebDriverExecutor::testStrategyOverride);
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
	@SuppressWarnings("static-method")
	void theSuiteRefusesTheTestStrategyOverride() {
		System.setProperty(InterpretedWebDriverExecutor.TEST_STRATEGY_PROPERTY, "None");
		List<String> ran = new ArrayList<>();
		SailSuite suite = new SailSuite("s/").setUp("00-pre.xml").files("01.xml").restore("99.xml", "00-pre.xml");
		assertThrows(IllegalStateException.class, () -> suite.run(ran::add));
		assertTrue(ran.isEmpty(), ran.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void theRestoreRunsAfterALaterFailureButNotAfterAFailedPrecondition() {
		List<String> ran = new ArrayList<>();
		SailSuite suite = new SailSuite("s/").setUp("00-pre.xml", "00-backup.xml").files("01.xml", "02.xml").restore("99.xml", "00-pre.xml");
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
	@SuppressWarnings("static-method")
	void onlyRunsTheChosenFilesBetweenTheSetUpAndTheRestore() {
		System.setProperty(SailSuite.ONLY_PROPERTY, "02");
		List<String> ran = new ArrayList<>();
		new SailSuite("s/").setUp("00-pre.xml").files("01.xml", "02.xml").restore("99.xml", "00-pre.xml").run(ran::add);
		assertEquals(List.of("s/00-pre.xml", "s/02.xml", "s/99.xml"), ran);
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

	private static int run(String... command) throws IOException, InterruptedException {
		Process process = new ProcessBuilder(command).inheritIO().start();
		if (! process.waitFor(60, TimeUnit.SECONDS)) {
			process.destroyForcibly();
			throw new IllegalStateException("The probe JVM did not finish");
		}
		return process.exitValue();
	}
}

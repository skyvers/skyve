package util.sail;

import static com.codeborne.selenide.Selenide.$;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Nonnull;
import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Tests that a two-factor sign in with Remember me ticked is remembered, in a real Chrome on a profile of its own: after
 * the browser restarts (its session cookies go, its profile's cookies stay) it is signed in with no code asked, while a
 * two-factor sign in without Remember me is signed out.
 * <p>
 * Needs a disposable server ({@code -Dsail.baseUrl}) that has email two-factor on for the customer that {@code admin}
 * signs in to ({@code account.tfaCustomers} in its {@code skyve.json}, and the customer's Configuration with Two Factor
 * Type email), and sends its mail bogusly ({@code smtp.testBogusSend}), which logs the code email's body in the
 * server's log. The test reads the code from that log: {@code -Dsail.serverLog} (or {@code SAIL_SERVER_LOG}) names the
 * file, and without it the test is skipped.
 */
@Tag("sail")
class TwoFactorRememberSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SIGNED_OUT = "<navigateList /> landed on the login page: the browser is signed out " +
												"(session expired, or remember-me didn't sign it in)";
	private static final Pattern CODE = Pattern.compile("Your verification code is: (\\d{6})");
	private static final String NAVIGATE = "<navigateList module=\"admin\" document=\"User\" />";

	TwoFactorRememberSailIT() {
		super(newDevice());
	}

	private static BrowserConfiguration newDevice() {
		try {
			return new BrowserConfiguration().browser(Browsers.chrome).profile(Files.createTempDirectory("sail-2fa-remember-profile"));
		}
		catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@AfterEach
	void deleteProfile() throws IOException {
		BrowserConfiguration device = selenium.getConfiguration();
		selenium.stopBrowser();
		Path profile = (device == null) ? null : device.getProfile();
		if (profile != null) {
			try (Stream<Path> files = Files.walk(profile)) {
				files.sorted(Comparator.reverseOrder()).forEach(file -> file.toFile().delete());
			}
		}
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"TwoFactor\"><method>" + steps + "</method></interaction></automation>";
	}

	/** Stops the browser and starts it again on its profile, as a user closing and reopening it does. */
	private void restartBrowser() {
		BrowserConfiguration device = Objects.requireNonNull(selenium.getConfiguration(), "the browser has not started");
		selenium.stopBrowser();
		selenium.startBrowser(device, device.resolve());
	}

	private static @Nonnull Path serverLog() {
		String name = System.getProperty("sail.serverLog", System.getenv("SAIL_SERVER_LOG"));
		assumeTrue(name != null, "needs -Dsail.serverLog (or SAIL_SERVER_LOG): the log that has the two-factor code emails");
		return Path.of(name);
	}

	private static int codesSent(@Nonnull Path log) {
		Matcher matcher = CODE.matcher(read(log));
		int count = 0;
		while (matcher.find()) {
			count++;
		}
		return count;
	}

	private static @Nonnull String read(@Nonnull Path log) {
		try {
			return Files.readString(log, StandardCharsets.ISO_8859_1);
		}
		catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/** Waits for the server to log another code email than the {@code before} it had logged, and returns the newest code. */
	private @Nonnull String newCode(@Nonnull Path log, int before) {
		return selenium.waitFor("the server to log the two-factor code email", d -> {
			Matcher matcher = CODE.matcher(read(log));
			String code = null;
			int count = 0;
			while (matcher.find()) {
				code = matcher.group(1);
				count++;
			}
			return (count > before) ? code : null;
		});
	}

	/** Signs in as admin up to the code page, with Remember me as asked, and returns the code that was emailed. */
	private @Nonnull String signInToCodePage(@Nonnull Path log, boolean remember) {
		int before = codesSent(log);
		selenium.login(null, "admin", "admin", Boolean.valueOf(remember));
		assertTrue($("#tfaCode1").exists(), "the password step asks for the emailed code");
		return newCode(log, before);
	}

	private void submitCode(@Nonnull String code) {
		for (int i = 0; i < code.length(); i++) {
			$("#tfaCode" + (i + 1)).setValue(String.valueOf(code.charAt(i)));
		}
		$("input[type=\"submit\"]").click();
		// the page is replaced by home (signed in), or by the code page again (a bad code)
		selenium.waitFor("the code to be checked", d -> (! $("#tfaCode1").exists() || $(".ui.error.message").exists()) ? Boolean.TRUE : null);
		selenium.waitFor(d -> "complete".equals(((org.openqa.selenium.JavascriptExecutor) d).executeScript("return document.readyState")) ? Boolean.TRUE : null);
	}

	/** Remember me, ticked at the password step, is still ticked on the code page. */
	private static boolean rememberTickedOnCodePage() {
		return $("#remember").is(com.codeborne.selenide.Condition.attribute("checked"));
	}

	/** A restarted browser is signed in by its Remember me cookie, and is not asked for a code. */
	@Test
	void aRememberedTwoFactorSignInIsSignedInAfterARestartWithoutACode() {
		Path log = serverLog();
		String code = signInToCodePage(log, true);
		assertTrue(rememberTickedOnCodePage(), "the code page keeps Remember me ticked");
		submitCode(code);
		assertFalse(selenium.isLoginPage(), "the right code signs in");

		restartBrowser();
		int before = codesSent(log);
		sailString(automation(NAVIGATE + "<testSuccess />"));
		assertAll(() -> assertFalse(selenium.isLoginPage(), "the restarted browser is signed in"),
					() -> assertFalse($("#tfaCode1").exists(), "and is not asked for a code"),
					() -> assertEquals(before, codesSent(log), "and no code was emailed"));
	}

	/** A mistyped code does not lose the user's Remember me choice. */
	@Test
	void aMistypedCodeKeepsRememberMeSoTheRightCodeIsRemembered() {
		Path log = serverLog();
		String code = signInToCodePage(log, true);
		String wrong = code.equals("000000") ? "111111" : "000000";
		submitCode(wrong);
		assertTrue($("#tfaCode1").exists(), "a wrong code stays on the code page");
		assertTrue(rememberTickedOnCodePage(), "with Remember me still ticked");
		submitCode(code);
		assertFalse(selenium.isLoginPage(), "the right code signs in");

		restartBrowser();
		sailString(automation(NAVIGATE + "<testSuccess />"));
		assertFalse(selenium.isLoginPage(), "the restarted browser is signed in");
	}

	/** Without Remember me a restarted browser is signed out, as it is for a sign in without a second factor. */
	@Test
	void aTwoFactorSignInWithoutRememberMeIsSignedOutAfterARestart() {
		Path log = serverLog();
		String code = signInToCodePage(log, false);
		assertFalse(rememberTickedOnCodePage(), "the code page shows Remember me unticked");
		submitCode(code);
		assertFalse(selenium.isLoginPage(), "the right code signs in");

		restartBrowser();
		String navigateList = automation(NAVIGATE);
		AssertionError navigate = assertThrows(AssertionError.class, () -> sailString(navigateList));
		assertNotNull(navigate.getMessage());
		assertTrue(navigate.getMessage().contains(SIGNED_OUT), navigate.getMessage());
	}
}

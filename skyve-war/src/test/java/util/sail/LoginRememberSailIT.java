package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Tests {@code <login remember="..."/>} and the harness's signed out detection against a Skyve server, in a real Chrome
 * on a profile of its own: a restart keeps the profile's cookies, including the Remember me cookie, and loses the
 * session's. Needs a disposable server ({@code -Dsail.baseUrl}), like the other SAIL ITs.
 */
@Tag("sail")
class LoginRememberSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SIGNED_OUT = "<navigateList /> landed on the login page: the browser is signed out " +
												"(session expired, or remember-me didn't sign it in)";

	LoginRememberSailIT() {
		super(newDevice());
	}

	private static BrowserConfiguration newDevice() {
		try {
			return new BrowserConfiguration().browser(Browsers.chrome).profile(Files.createTempDirectory("sail-remember-profile"));
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

	/** Stops the browser and starts it again on its profile, as a user closing and reopening it does. */
	private void restartBrowser() {
		BrowserConfiguration device = Objects.requireNonNull(selenium.getConfiguration(), "the browser has not started");
		selenium.stopBrowser();
		selenium.startBrowser(device, device.resolve());
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Remember\"><method>" + steps + "</method></interaction></automation>";
	}

	/** With {@code remember="true"} a restarted browser is signed in again by its Remember me cookie. */
	@Test
	void aRememberedBrowserIsSignedInAfterARestart() {
		sailString(automation("<login user=\"admin\" password=\"admin\" remember=\"true\" /><testSuccess />"));
		restartBrowser();
		sailString(automation("<navigateList module=\"admin\" document=\"User\" /><testSuccess />"));
		assertFalse(selenium.isLoginPage(), "the restarted browser is signed in");
	}

	/** With {@code remember="false"} a restarted browser is signed out, and the next navigate says so. */
	@Test
	void aBrowserThatIsNotRememberedIsSignedOutAfterARestart() {
		sailString(automation("<login user=\"admin\" password=\"admin\" remember=\"false\" /><testSuccess />"));
		restartBrowser();
		String navigateList = automation("<navigateList module=\"admin\" document=\"User\" />");
		String testSuccess = automation("<navigateLink href=\"login\" /><testSuccess />");
		AssertionError navigate = assertThrows(AssertionError.class, () -> sailString(navigateList));
		AssertionError success = assertThrows(AssertionError.class, () -> sailString(testSuccess));
		assertAll(() -> assertTrue(navigate.getMessage().contains(SIGNED_OUT), navigate.getMessage()),
					() -> assertTrue(success.getMessage().contains("expected success, but the browser is on the login page (signed out)"),
										success.getMessage()));
	}
}

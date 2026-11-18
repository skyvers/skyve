package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Tests that signing out ends Remember me in that browser only, in two real Chrome browsers (profiles) signed in as the
 * same user with Remember me: a phone and a PC. Signing out on the PC leaves the phone remembered, so after a restart it
 * is still signed in, while the PC is signed out. Needs a disposable server ({@code -Dsail.baseUrl}), like the other
 * SAIL ITs.
 */
@Tag("sail")
class RememberMePerDeviceSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SIGNED_OUT = "<navigateList /> landed on the login page: the browser is signed out " +
												"(session expired, or remember-me didn't sign it in)";
	private static final String LOGIN = "<login user=\"admin\" password=\"admin\" remember=\"true\" /><testSuccess />";
	private static final String NAVIGATE = "<navigateList module=\"admin\" document=\"User\" />";

	private final List<Path> profiles = new ArrayList<>();
	private BrowserConfiguration device;

	RememberMePerDeviceSailIT() {
		super(new BrowserConfiguration().browser(Browsers.chrome).profile(newProfile("sail-remember-phone")));
	}

	private static Path newProfile(String name) {
		try {
			return Files.createTempDirectory(name);
		}
		catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@AfterEach
	void deleteProfiles() throws IOException {
		selenium.stopBrowser();
		for (Path profile : profiles) {
			try (Stream<Path> files = Files.walk(profile)) {
				files.sorted(Comparator.reverseOrder()).forEach(file -> file.toFile().delete());
			}
		}
	}

	/**
	 * Makes the next script run on the device with the given profile. A change of profile is a browser-level change, so
	 * the harness stops the browser (its session cookies go, as when a user closes it) and starts it on the profile.
	 */
	private void useDevice(Path profile) {
		device.profile(profile);
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Remember\"><method>" + steps + "</method></interaction></automation>";
	}

	@Test
	void signingOutOnThePcLeavesThePhoneRemembered() {
		device = Objects.requireNonNull(selenium.getConfiguration(), "the browser has not started");
		Path phone = Objects.requireNonNull(device.getProfile());
		Path pc = newProfile("sail-remember-pc");
		profiles.add(phone);
		profiles.add(pc);

		sailString(automation(LOGIN)); // the phone signs in with Remember me
		useDevice(pc);
		sailString(automation(LOGIN)); // so does the PC, then the PC signs out
		sailString(automation("<logout />"));

		// The phone restarts: still signed in by its own Remember me cookie
		useDevice(phone);
		sailString(automation(NAVIGATE + "<testSuccess />"));
		boolean phoneOnLoginPage = selenium.isLoginPage();

		// The PC restarts: it signed out, so it is signed out
		useDevice(pc);
		String pcNavigateSteps = automation(NAVIGATE);
		AssertionError pcNavigate = assertThrows(AssertionError.class, () -> sailString(pcNavigateSteps));

		assertAll(() -> assertFalse(phoneOnLoginPage, "the phone, which did not sign out, is still signed in"),
					() -> assertTrue(pcNavigate.getMessage().contains(SIGNED_OUT), pcNavigate.getMessage()));
	}
}

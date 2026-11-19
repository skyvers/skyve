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
 * Tests "Sign out of all other sessions" on the user's account page, in two real Chrome browsers (profiles) signed in as
 * the same user with Remember me: a phone and a PC. Pressed on the PC, it signs the phone out (after a restart the phone
 * is signed out, its Remember me gone) and leaves the PC signed in, both at once and after a restart (this browser's
 * Remember me stays). Needs a disposable server ({@code -Dsail.baseUrl}), like the other SAIL ITs.
 */
@Tag("sail")
class SignOutOtherSessionsSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SIGNED_OUT = "<navigateList /> landed on the login page: the browser is signed out " +
												"(session expired, or remember-me didn't sign it in)";
	private static final String LOGIN = "<login user=\"admin\" password=\"admin\" remember=\"true\" /><testSuccess />";
	private static final String NAVIGATE = "<navigateList module=\"admin\" document=\"User\" />";

	private final List<Path> profiles = new ArrayList<>();
	private BrowserConfiguration device;

	SignOutOtherSessionsSailIT() {
		super(new BrowserConfiguration().browser(Browsers.chrome).profile(newProfile("sail-signout-phone")));
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
				"<interaction name=\"SignOutOther\"><method>" + steps + "</method></interaction></automation>";
	}

	@Test
	void signingOutOfAllOtherSessionsOnThePcSignsOutThePhoneOnly() {
		device = Objects.requireNonNull(selenium.getConfiguration(), "the browser has not started");
		Path phone = Objects.requireNonNull(device.getProfile());
		Path pc = newProfile("sail-signout-pc");
		profiles.add(phone);
		profiles.add(pc);

		sailString(automation(LOGIN)); // the phone signs in with Remember me
		useDevice(pc);
		sailString(automation(LOGIN)); // so does the PC

		// On the PC: My Current Sessions, Sign out of all other sessions, confirmed
		sailString(automation("<navigateEdit module=\"admin\" document=\"UserAccount\" />" +
								"<tabSelect path=\"My Current Sessions\" />" +
								"<action name=\"SignOutOtherSessions\" confirm=\"true\" />" +
								"<testMessage contains=\"Signed out of\" type=\"info\" />"));
		// The PC's own session carries on
		sailString(automation(NAVIGATE + "<testSuccess />"));

		// The phone restarts: its Remember me is gone, so it is signed out
		useDevice(phone);
		String phoneNavigateSteps = automation(NAVIGATE);
		AssertionError phoneNavigate = assertThrows(AssertionError.class, () -> sailString(phoneNavigateSteps));

		// The PC restarts: still signed in by its own Remember me cookie
		useDevice(pc);
		sailString(automation(NAVIGATE + "<testSuccess />"));
		boolean pcOnLoginPage = selenium.isLoginPage();

		assertAll(() -> assertFalse(pcOnLoginPage, "the PC, which signed the others out, is still signed in"),
					() -> assertTrue(phoneNavigate.getMessage().contains(SIGNED_OUT), phoneNavigate.getMessage()));
	}

	/**
	 * The administrator's "Sign out everywhere" on the User view, here on the administrator's own user (the one user a
	 * disposable server has): it signs the phone out but not this browser, which stays on its own session and Remember me.
	 * (Another user's tokens and sessions are all removed: see {@code SignOutEverywhereH2Test}.)
	 */
	@Test
	void theAdministratorsSignOutEverywhereOnTheirOwnUserKeepsThisBrowser() {
		device = Objects.requireNonNull(selenium.getConfiguration(), "the browser has not started");
		Path phone = Objects.requireNonNull(device.getProfile());
		Path pc = newProfile("sail-signout-admin-pc");
		profiles.add(phone);
		profiles.add(pc);

		sailString(automation(LOGIN));
		useDevice(pc);
		sailString(automation(LOGIN));
		sailString(automation(NAVIGATE +
								"<listGridZoom row=\"0\" />" +
								"<action name=\"SignOutEverywhere\" confirm=\"true\" />" +
								"<testMessage contains=\"Signed out everywhere else\" type=\"info\" />"));

		useDevice(phone);
		String phoneNavigateSteps = automation(NAVIGATE);
		AssertionError phoneNavigate = assertThrows(AssertionError.class, () -> sailString(phoneNavigateSteps));

		useDevice(pc);
		sailString(automation(NAVIGATE + "<testSuccess />"));
		boolean pcOnLoginPage = selenium.isLoginPage();

		assertAll(() -> assertFalse(pcOnLoginPage, "this browser stays signed in"),
					() -> assertTrue(phoneNavigate.getMessage().contains(SIGNED_OUT), phoneNavigate.getMessage()));
	}
}

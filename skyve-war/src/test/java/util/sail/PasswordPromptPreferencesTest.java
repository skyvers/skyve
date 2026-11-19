package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
import org.openqa.selenium.edge.EdgeOptions;

/**
 * Tests that the options SAIL starts Chrome and Edge with turn the password manager's prompts off, whatever the
 * configuration (plain, a profile, a mobile device), without a browser.
 */
class PasswordPromptPreferencesTest {
	private static final Map<String, Object> OFF = Map.of("profile.password_manager_leak_detection", Boolean.FALSE,
															"credentials_enable_service", Boolean.FALSE,
															"profile.password_manager_enabled", Boolean.FALSE);

	@SuppressWarnings("unchecked")
	private static Map<String, Object> prefs(ChromiumOptions<?> options) {
		return (Map<String, Object>) CrossBrowserSelenium.chromiumPreferences(options);
	}

	@Test
	@SuppressWarnings("static-method")
	void plainChromeHasThePrefs() {
		assertEquals(OFF, prefs(CrossBrowserSelenium.chromeOptions(new BrowserConfiguration(), false)));
	}

	@Test
	@SuppressWarnings("static-method")
	void headlessChromeWithAProfileAndExtraArgumentsHasThePrefsAndTheArguments() {
		Path profile = Path.of("target", "password-prompt-test", "p").toAbsolutePath();
		ChromeOptions options = CrossBrowserSelenium.chromeOptions(new BrowserConfiguration().profile(profile).arguments("--x"), true);
		@SuppressWarnings("unchecked")
		Map<String, Object> chromeOptions = (Map<String, Object>) options.asMap().get(ChromeOptions.CAPABILITY);
		assertAll(() -> assertEquals(OFF, prefs(options)),
					() -> assertTrue(chromeOptions.get("args").toString().contains("--user-data-dir=" + profile), "profile kept"),
					() -> assertTrue(chromeOptions.get("args").toString().contains("--x"), "arguments kept"));
	}

	/** A mobile device is a user agent and a window size, so it goes through the same options. */
	@Test
	@SuppressWarnings("static-method")
	void aMobileDeviceHasThePrefs() {
		BrowserConfiguration phone = new BrowserConfiguration().userAgentString(Devices.iphone.userAgentString)
																.resolution(375, 667);
		assertEquals(OFF, prefs(CrossBrowserSelenium.chromeOptions(phone, true)));
	}

	@Test
	@SuppressWarnings("static-method")
	void edgeHasThePrefs() {
		EdgeOptions options = CrossBrowserSelenium.edgeOptions(new BrowserConfiguration(), true);
		assertEquals(OFF, prefs(options));
	}

	@Test
	@SuppressWarnings("static-method")
	void thePreferencesAreANewMapEachTime() {
		Map<String, Object> first = CrossBrowserSelenium.passwordPromptPreferences();
		first.clear();
		assertEquals(OFF, CrossBrowserSelenium.passwordPromptPreferences());
	}

	@Test
	@SuppressWarnings("static-method")
	void optionsWithNoPreferencesReadAsNone() {
		assertNull(CrossBrowserSelenium.chromiumPreferences(new ChromeOptions()));
	}

	/** Preferences already on the options stay, and win over ours for the same name. */
	@Test
	@SuppressWarnings("static-method")
	void preferencesAlreadySetAreKeptAndWin() {
		ChromeOptions options = new ChromeOptions();
		options.setExperimentalOption("prefs", Map.of("download.default_directory", "/tmp/x", "credentials_enable_service", Boolean.TRUE));
		CrossBrowserSelenium.turnOffPasswordPrompts(options);
		Map<String, Object> prefs = prefs(options);
		assertAll(() -> assertEquals("/tmp/x", prefs.get("download.default_directory"), "kept"),
					() -> assertEquals(Boolean.TRUE, prefs.get("credentials_enable_service"), "the caller's choice wins"),
					() -> assertEquals(Boolean.FALSE, prefs.get("profile.password_manager_leak_detection"), "ours added"));
	}
}

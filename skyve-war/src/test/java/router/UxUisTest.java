package router;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S5960" }) // Assertions are intentionally test-only.
class UxUisTest {

	@Test
	void primeFacesProfilesUseEditorialConfiguration() {
		assertAll(
				() -> assertEditorial(UxUis.PHONE, "phone"),
				() -> assertEditorial(UxUis.TABLET, "tablet"),
				() -> assertEditorial(UxUis.EXTERNAL, "external"),
				() -> assertEditorial(UxUis.STARTUP, "startup"));
	}

	@Test
	void desktopUsesSmartClientConfiguration() {
		assertAll(
				() -> assertEquals("desktop", UxUis.DESKTOP.getName()),
				() -> assertEquals("Tahoe", UxUis.DESKTOP.getScSkin()),
				() -> assertNull(UxUis.DESKTOP.getPfTemplateName()),
				() -> assertEquals("casablanca", UxUis.DESKTOP.getPfThemeName()),
				() -> assertEquals("smartclient", UxUis.DESKTOP.getPfThemeColour()));
	}

	private static void assertEditorial(org.skyve.metadata.router.UxUi uxui, String name) {
		assertAll(
				() -> assertEquals(name, uxui.getName()),
				() -> assertEquals("editorial", uxui.getPfTemplateName()),
				() -> assertEquals("skyve", uxui.getPfThemeName()),
				() -> assertEquals("blue", uxui.getPfThemeColour()));
	}
}

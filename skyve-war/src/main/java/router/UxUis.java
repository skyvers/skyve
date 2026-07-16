package router;

import org.skyve.metadata.router.UxUi;

/**
 * Defines named UX/UI profiles used by {@link DefaultUxUiSelector}.
 * 
 * Only change theme attributes here.
 * Declare new UxUis in the DefaultUxUiSelector.
 */
public class UxUis {
	/**
	 * The PF Theme to use
	 */
	private static final String EDITORIAL_THEME = "editorial";
	
	/**
	 * Colour palette for the editorial template: "blue", "indigo", "emerald", "custom" or
	 * "dark". Light palettes follow the browser/operating system colour scheme preference;
	 * "dark" forces dark mode.
	 * See docs/editorial-custom-colour.md.
	 */
	private static final String THEME_COLOUR = "blue";
	// "dark" forces dark mode.
	private static final String SKYVE_THEME = "dark".equals(THEME_COLOUR) ? "skyve-dark" : "skyve";

	/**
	 * PrimeFaces profile for phone devices.
	 */
	public static final UxUi PHONE = UxUi.newPrimeFaces("phone", EDITORIAL_THEME, SKYVE_THEME, THEME_COLOUR);
	
	/**
	 * PrimeFaces profile for tablet devices.
	 */
	public static final UxUi TABLET = UxUi.newPrimeFaces("tablet", EDITORIAL_THEME, SKYVE_THEME, THEME_COLOUR);
	
	/**
	 * SmartClient profile for desktop browsers.
	 */
	public static final UxUi DESKTOP = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Tahoe", "casablanca");
	
	/**
	 * PrimeFaces profile for unauthenticated/public pages.
	 */
	public static final UxUi EXTERNAL = UxUi.newPrimeFaces("external", EDITORIAL_THEME, SKYVE_THEME, THEME_COLOUR);
	
	/**
	 * PrimeFaces profile for startup/setup wizard pages.
	 */
	public static final UxUi STARTUP = UxUi.newPrimeFaces("startup", EDITORIAL_THEME, SKYVE_THEME, THEME_COLOUR);

	/**
	 * Prevent instantiation
	 */
	private UxUis() {
		// nothing to see here
	}
}

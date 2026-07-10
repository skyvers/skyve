package router;

import org.skyve.metadata.router.UxUi;

/**
 * Defines named UX/UI profiles used by {@link DefaultUxUiSelector}.
 * 
 * Only change theme attributes here.
 * Declare new UxUis in the DefaultUxUiSelector.
 */
public class UxUis {
	private static final String EDITORIAL_THEME = "editorial";
	private static final String SKYVE_THEME = "skyve";
	/**
	 * Colour palette for the editorial template: "blue" (default), "indigo", "emerald" or "dark".
	 * Each palette pairs a primefaces-skyve-&lt;colour&gt; alias theme (widget internals) with
	 * editorial/assets/css/themes/&lt;colour&gt;.css (layout palette).
	 */
	private static final String THEME_COLOUR = "dark";

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
}

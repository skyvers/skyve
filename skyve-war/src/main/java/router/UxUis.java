package router;

import org.skyve.metadata.router.UxUi;

/**
 * Defines named UX/UI profiles used by {@link DefaultUxUiSelector}.
 * 
 * Only change theme attributes here.
 * Declare new UxUis in the DefaultUxUiSelector.
 */
public class UxUis {
	// Templates
	private static final String EDITORIAL_TEMPLATE = "editorial";
	private static final String ECUADOR_TEMPLATE = "ecuador";
	private static final String ULTIMA_TEMPLATE = "ultima";
	private static final String DIAMOND_TEMPLATE = "diamond";

	/**
	 * Base colours supported by each PrimeFaces template.
	 * <ul>
	 * <li>Editorial: {@code blue}, {@code indigo}, {@code emerald}, {@code custom}, {@code dark}</li>
	 * <li>Ecuador: {@code amber}, {@code blue}, {@code bluegrey}, {@code cyan}, {@code darkblue},
	 * {@code deeporange}, {@code deeppurple}, {@code green}, {@code grey}, {@code indigo}, {@code lime},
	 * {@code mojito}, {@code pink}, {@code purple}, {@code yellow}</li>
	 * <li>Ultima: {@code amber}, {@code blue}, {@code bluegrey}, {@code brown}, {@code cyan},
	 * {@code deeporange}, {@code deeppurple}, {@code green}, {@code indigo}, {@code lightblue},
	 * {@code lightgreen}, {@code lime}, {@code orange}, {@code pink}, {@code purple}, {@code teal},
	 * {@code yellow}</li>
	 * <li>Diamond: {@code blue}, {@code cyan}, {@code deeppurple}, {@code green}, {@code indigo},
	 * {@code lightgreen}, {@code orange}, {@code pink}, {@code purple}, {@code teal}. Append the
	 * {@code light}, {@code dim}, or {@code dark} scheme with a hyphen, for example
	 * {@code indigo-light}.</li>
	 * </ul>
	 * Editorial light palettes follow the browser/operating system colour scheme preference;
	 * {@code dark} forces dark mode. See {@code docs/editorial-custom-colour.md}.
	 * For instructions, see <a href="https://github.com/skyvers/skyve/blob/master/primefaces-themes.txt">primefaces-themes.txt</a>.
	 */
	private static final String THEME_COLOUR = "blue";

	// "dark" forces dark mode
	private static final String SKYVE_THEME = "dark".equals(THEME_COLOUR) ? "skyve-dark" : "skyve";
	@SuppressWarnings("unused")
	private static final String ECUADOR_THEME = ECUADOR_TEMPLATE + '-' + THEME_COLOUR;
	@SuppressWarnings("unused")
	private static final String ULTIMA_THEME = ULTIMA_TEMPLATE + '-' + THEME_COLOUR;
	@SuppressWarnings("unused")
	private static final String DIAMOND_THEME = DIAMOND_TEMPLATE + '-' + THEME_COLOUR;

	/**
	 * PrimeFaces profile for phone devices.
	 */
	public static final UxUi PHONE = UxUi.newPrimeFaces("phone", EDITORIAL_TEMPLATE, SKYVE_THEME, THEME_COLOUR);

	/**
	 * PrimeFaces profile for tablet devices.
	 */
	public static final UxUi TABLET = UxUi.newPrimeFaces("tablet", EDITORIAL_TEMPLATE, SKYVE_THEME, THEME_COLOUR);

	/**
	 * SmartClient profile for desktop browsers.
	 */
	public static final UxUi DESKTOP = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Tahoe", "casablanca", "smartclient");

	/**
	 * PrimeFaces profile for unauthenticated/public pages.
	 */
	public static final UxUi EXTERNAL = UxUi.newPrimeFaces("external", EDITORIAL_TEMPLATE, SKYVE_THEME, THEME_COLOUR);

	/**
	 * PrimeFaces profile for startup/setup wizard pages.
	 */
	public static final UxUi STARTUP = UxUi.newPrimeFaces("startup", EDITORIAL_TEMPLATE, SKYVE_THEME, THEME_COLOUR);

	/**
	 * Prevent instantiation
	 */
	private UxUis() {
		// nothing to see here
	}
}

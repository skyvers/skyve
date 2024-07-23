package router;

import org.skyve.metadata.router.UxUi;

// Only change theme attributes here.
// Declare new UxUis in the DefaultUxUiSelector.
public class UxUis {
	public static final UxUi PHONE = UxUi.newPrimeFaces("phone", "editorial", "saga");
	public static final UxUi TABLET = UxUi.newPrimeFaces("tablet", "editorial", "saga");
	public static final UxUi DESKTOP = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Tahoe", "casablanca");
	public static final UxUi EXTERNAL = UxUi.newPrimeFaces("external", "editorial", "saga");
	public static final UxUi STARTUP = UxUi.newPrimeFaces("startup", "editorial", "saga");
}

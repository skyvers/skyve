package util.sail;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration.Browsers;

/**
 * The same views and actions as {@link SignOutOtherSessionsSailIT}, in SmartClient (desktop): the confirmation, the
 * message and that this browser's own session carries on. Which devices are signed out is server side and the same
 * for both renderers, so it is checked once, in {@link SignOutOtherSessionsSailIT}, which restarts browsers on
 * profiles (a restart lands on PrimeFaces, so it cannot be checked in SmartClient). Needs a disposable server
 * ({@code -Dsail.baseUrl}), like the other SAIL ITs.
 */
@Tag("sail")
class SignOutOtherSessionsSmartClientSailIT extends AbstractSmartClientInterpretedSail {
	private static final String LOGIN = "<login user=\"admin\" password=\"admin\" remember=\"true\" /><testSuccess /><switchMode />";
	private static final String STILL_SIGNED_IN = "<navigateList module=\"admin\" document=\"User\" /><testSuccess />";

	SignOutOtherSessionsSmartClientSailIT() {
		super(new BrowserConfiguration().browser(Browsers.chrome));
	}

	private static String automation(String steps) {
		return "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"SignOutOther\"><method>" + steps + "</method></interaction></automation>";
	}

	@Test
	void signOutOfAllOtherSessionsOnTheAccountPage() {
		String script = automation(LOGIN +
								"<navigateEdit module=\"admin\" document=\"UserAccount\" />" +
								"<tabSelect path=\"My Current Sessions\" />" +
								"<action name=\"SignOutOtherSessions\" confirm=\"true\" />" +
								"<testMessage contains=\"Signed out of\" type=\"info\" />" +
								STILL_SIGNED_IN);
		assertDoesNotThrow(() -> sailString(script));
	}

	@Test
	void signOutEverywhereOnTheUserView() {
		String script = automation(LOGIN +
								"<navigateList module=\"admin\" document=\"User\" />" +
								"<listGridZoom row=\"0\" />" +
								"<action name=\"SignOutEverywhere\" confirm=\"true\" />" +
								"<testMessage contains=\"Signed out everywhere else\" type=\"info\" />" +
								STILL_SIGNED_IN);
		assertDoesNotThrow(() -> sailString(script));
	}
}

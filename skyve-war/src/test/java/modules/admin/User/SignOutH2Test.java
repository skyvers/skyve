package modules.admin.User;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.cache.StateUtil;
import org.skyve.persistence.Persistence;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpSession;

import jakarta.servlet.http.Cookie;
import util.AbstractH2Test;

/**
 * Sign out of every device, and of every device but this one, against ADM_UserToken and the session cache.
 */
@SuppressWarnings("static-method")
class SignOutH2Test extends AbstractH2Test {
	private static final String CUSTOMER = "bizhub";
	private static final String USER_ID = "sign-out-user-id";
	private static final String PRINCIPAL = CUSTOMER + "/signOutUser";
	private static final String OTHER_PRINCIPAL = CUSTOMER + "/someoneElse";

	private Persistence p;
	private MockHttpServletRequest request;
	private MockHttpSession thisSession;

	@BeforeEach
	void setup() {
		p = CORE.getPersistence();
		for (String series : List.of("phone", "tablet", "pc")) {
			RememberMeTokens.add(p, CUSTOMER, PRINCIPAL, series);
		}
		RememberMeTokens.add(p, CUSTOMER, OTHER_PRINCIPAL, "pc2");

		request = new MockHttpServletRequest();
		thisSession = (MockHttpSession) request.getSession();
		StateUtil.addSession(USER_ID, thisSession);
		StateUtil.addSession(USER_ID, new MockHttpSession(null, "phoneSession"));
		StateUtil.addSession(USER_ID, new MockHttpSession(null, "tabletSession"));
		StateUtil.addSession("someone-else-id", new MockHttpSession(null, "elsewhere"));
	}

	@AfterEach
	void cleanUp() {
		StateUtil.removeSessions(USER_ID);
		StateUtil.removeSessions("someone-else-id");
	}

	private static String cookie(String series) {
		return Base64.getEncoder().encodeToString((series + ":token").getBytes(StandardCharsets.UTF_8));
	}

	@Test
	void everywhereRemovesAllTheUsersTokensAndSessionsButNoOneElses() {
		SignOut.everywhere(p, CUSTOMER, "signOutUser", USER_ID);

		assertEquals(List.of(), RememberMeTokens.series(p, PRINCIPAL));
		assertEquals(List.of("pc2"), RememberMeTokens.series(p, OTHER_PRINCIPAL));
		assertEquals(Set.of(), StateUtil.getSessions(USER_ID));
		assertEquals(Set.of("elsewhere"), StateUtil.getSessions("someone-else-id"));
	}

	@Test
	void otherDevicesKeepsThisBrowsersSeriesAndSession() {
		request.setCookies(new Cookie("remember", cookie("pc")));

		int signedOut = SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request);

		assertEquals(List.of("pc"), RememberMeTokens.series(p, PRINCIPAL));
		assertEquals(Set.of(thisSession.getId()), StateUtil.getSessions(USER_ID));
		assertEquals(List.of("pc2"), RememberMeTokens.series(p, OTHER_PRINCIPAL));
		assertEquals(Set.of("elsewhere"), StateUtil.getSessions("someone-else-id"));
		assertEquals(2, signedOut);
	}

	/** The real shape: the cookie holds the URL-encoded Base64 series, the token store the decoded one. */
	@Test
	void otherDevicesMatchesTheSeriesAsSpringWritesItInTheCookie() {
		RememberMeTokens.add(p, CUSTOMER, PRINCIPAL, "0PjFbQ0J05e+YG2CIMTo7A==");
		request.setCookies(new Cookie("remember", cookie("0PjFbQ0J05e%2BYG2CIMTo7A%3D%3D")));

		SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request);

		assertEquals(List.of("0PjFbQ0J05e+YG2CIMTo7A=="), RememberMeTokens.series(p, PRINCIPAL));
	}

	@Test
	void otherDevicesWithoutACookieRemovesAllTheTokensButKeepsTheSession() {
		SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request);

		assertEquals(List.of(), RememberMeTokens.series(p, PRINCIPAL));
		assertEquals(Set.of(thisSession.getId()), StateUtil.getSessions(USER_ID));
	}

	@Test
	void otherDevicesWithAnUnknownSeriesCookieRemovesAllTheTokens() {
		request.setCookies(new Cookie("remember", cookie("someSeriesOfNoOne")));

		SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request);

		assertEquals(List.of(), RememberMeTokens.series(p, PRINCIPAL));
	}

	@Test
	void otherDevicesCountsTheLargerOfSessionsAndTokens() {
		StateUtil.removeSessions(USER_ID);
		StateUtil.addSession(USER_ID, thisSession);
		request.setCookies(new Cookie("remember", cookie("pc")));

		// two other tokens, no other sessions
		assertEquals(2, SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request));
		// nothing is left to sign out
		assertEquals(0, SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, request));
	}

	@Test
	void otherDevicesWithoutASessionEndsAllTheSessions() {
		MockHttpServletRequest noSession = new MockHttpServletRequest();

		int signedOut = SignOut.otherDevices(p, CUSTOMER, "signOutUser", USER_ID, noSession);

		assertEquals(Set.of(), StateUtil.getSessions(USER_ID));
		assertEquals(3, signedOut);
	}
}

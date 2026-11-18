package modules.admin.UserAccount.actions;

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
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.WebContainer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import jakarta.servlet.http.Cookie;
import modules.admin.User.RememberMeTokens;
import modules.admin.domain.UserAccount;
import util.AbstractH2Test;

/**
 * "Sign out of all other sessions" keeps this browser's Remember me series and session, and ends the rest.
 */
@SuppressWarnings("static-method")
class SignOutOtherSessionsH2Test extends AbstractH2Test {
	private Persistence p;
	private User user;
	private String principal;
	private MockHttpServletRequest request;
	private MockHttpSession thisSession;

	@BeforeEach
	void setup() {
		p = CORE.getPersistence();
		user = CORE.getUser();
		principal = user.getCustomerName() + '/' + user.getName();
		for (String series : List.of("phone", "tablet", "pc")) {
			RememberMeTokens.add(p, user.getCustomerName(), principal, series);
		}
		RememberMeTokens.add(p, user.getCustomerName(), user.getCustomerName() + "/someoneElse", "pc2");

		request = new MockHttpServletRequest();
		thisSession = (MockHttpSession) request.getSession();
		StateUtil.removeSessions(user.getId());
		StateUtil.addSession(user.getId(), thisSession);
		StateUtil.addSession(user.getId(), new MockHttpSession(null, "phoneSession"));
		StateUtil.addSession(user.getId(), new MockHttpSession(null, "tabletSession"));
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());
	}

	@AfterEach
	void cleanUp() {
		WebContainer.clear();
		StateUtil.removeSessions(user.getId());
	}

	@Test
	void keepsThisBrowsersSeriesAndSessionAndEndsTheOthers() throws Exception {
		request.setCookies(new Cookie("remember",
				Base64.getEncoder().encodeToString("pc:token".getBytes(StandardCharsets.UTF_8))));
		UserAccount account = UserAccount.newInstance();
		MockWebContext webContext = new MockWebContext();

		UserAccount result = new SignOutOtherSessions().execute(account, webContext).getBean();

		assertEquals(List.of("pc"), RememberMeTokens.series(p, principal));
		assertEquals(List.of("pc2"), RememberMeTokens.series(p, user.getCustomerName() + "/someoneElse"));
		assertEquals(Set.of(thisSession.getId()), StateUtil.getSessions(user.getId()));
		// the sessions list now shows only this one
		assertEquals(1, result.getSessions().size());
		assertEquals(thisSession.getId(), result.getSessions().get(0).getMemo1());
	}

	@Test
	void signedInWithoutRememberMeRemovesAllTokensButKeepsTheSession() throws Exception {
		new SignOutOtherSessions().execute(UserAccount.newInstance(), new MockWebContext());

		assertEquals(List.of(), RememberMeTokens.series(p, principal));
		assertEquals(Set.of(thisSession.getId()), StateUtil.getSessions(user.getId()));
	}
}

package modules.admin.User.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.AccessException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.WebContainer;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import jakarta.servlet.http.Cookie;
import modules.admin.User.RememberMeTokens;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminRole;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * The administrator's "Sign out everywhere" on a user.
 */
class SignOutEverywhereH2Test extends AbstractH2Test {
	private Persistence p;
	private DataBuilder db;
	private UserExtension target;
	private String targetPrincipal;
	private MockHttpServletRequest request;

	@BeforeEach
	void setup() {
		p = CORE.getPersistence();
		db = new DataBuilder().fixture(FixtureType.crud);
		target = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		target.setUserName("signOutTarget");
		target = p.save(target);
		targetPrincipal = target.getBizCustomer() + "/signOutTarget";
		for (String series : List.of("phone", "tablet", "pc")) {
			RememberMeTokens.add(p, target.getBizCustomer(), targetPrincipal, series);
		}
		RememberMeTokens.add(p, target.getBizCustomer(), target.getBizCustomer() + "/someoneElse", "pc2");
		StateUtil.addSession(target.getBizId(), new MockHttpSession(null, "targetSession1"));
		StateUtil.addSession(target.getBizId(), new MockHttpSession(null, "targetSession2"));

		request = new MockHttpServletRequest();
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());
	}

	@AfterEach
	void cleanUp() {
		WebContainer.clear();
		StateUtil.removeSessions(target.getBizId());
		StateUtil.removeSessions(CORE.getUser().getId());
	}

	@Test
	void anAdministratorRemovesAllTheUsersTokensAndSessions() throws Exception {
		assertTrue(CORE.getUser().isInRole(AdminRole.SECURITY_ADMINISTRATOR));

		new SignOutEverywhere().execute(target, new MockWebContext());

		assertEquals(List.of(), RememberMeTokens.series(p, targetPrincipal));
		assertEquals(List.of("pc2"), RememberMeTokens.series(p, target.getBizCustomer() + "/someoneElse"));
		assertEquals(Set.of(), StateUtil.getSessions(target.getBizId()));
	}

	/** The administrator's own request does not end its own session or Remember me. */
	@Test
	void anAdministratorTargetingThemselfKeepsThisBrowser() throws Exception {
		org.skyve.metadata.user.User admin = CORE.getUser();
		String adminPrincipal = admin.getCustomerName() + '/' + admin.getName();
		for (String series : List.of("phone", "pc")) {
			RememberMeTokens.add(p, admin.getCustomerName(), adminPrincipal, series);
		}
		MockHttpSession thisSession = (MockHttpSession) request.getSession();
		StateUtil.addSession(admin.getId(), thisSession);
		StateUtil.addSession(admin.getId(), new MockHttpSession(null, "adminPhoneSession"));
		request.setCookies(new Cookie("remember",
				Base64.getEncoder().encodeToString("pc:token".getBytes(StandardCharsets.UTF_8))));
		UserExtension self = Objects.requireNonNull(p.retrieve(User.MODULE_NAME, User.DOCUMENT_NAME, admin.getId()));

		new SignOutEverywhere().execute(self, new MockWebContext());

		assertEquals(List.of("pc"), RememberMeTokens.series(p, adminPrincipal));
		assertEquals(Set.of(thisSession.getId()), StateUtil.getSessions(admin.getId()));
	}

	@Test
	void anUnsavedUserHasNothingToSignOut() throws Exception {
		UserExtension unsaved = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		unsaved.setUserName("neverSaved");
		RememberMeTokens.add(p, unsaved.getBizCustomer(), unsaved.getBizCustomer() + "/neverSaved", "stale");

		new SignOutEverywhere().execute(unsaved, new MockWebContext());

		assertEquals(List.of("stale"), RememberMeTokens.series(p, unsaved.getBizCustomer() + "/neverSaved"));
	}

	@Test
	void aNonAdministratorCannotSignAnotherUserOut() {
		AbstractPersistence persistence = AbstractPersistence.get();
		org.skyve.metadata.user.User originalUser = persistence.getUser();
		try {
			UserExtension basic = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
			basic.setUserName("basicUserNoAdmin");
			basic.getGroups().clear();
			basic.getRoles().clear();
			UserRole role = db.build(UserRole.MODULE_NAME, UserRole.DOCUMENT_NAME);
			role.setRoleName("admin.BasicUser");
			basic.addRolesElement(role);
			basic = p.save(basic);
			org.skyve.metadata.user.User basicUser = basic.toMetaDataUser();
			assertTrue(basicUser.isInRole(AdminRole.BASIC_USER));
			persistence.setUser(basicUser);
			UserExtension victim = target;
			SignOutEverywhere action = new SignOutEverywhere();
			MockWebContext webContext = new MockWebContext();

			assertThrows(AccessException.class, () -> action.execute(victim, webContext));

			persistence.setUser(originalUser);
			assertEquals(List.of("pc", "phone", "tablet"), RememberMeTokens.series(p, targetPrincipal));
			assertEquals(Set.of("targetSession1", "targetSession2"), StateUtil.getSessions(target.getBizId()));
		} finally {
			persistence.setUser(originalUser);
		}
	}
}

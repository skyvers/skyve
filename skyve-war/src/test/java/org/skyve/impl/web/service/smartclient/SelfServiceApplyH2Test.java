package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.View;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.UserDashboard;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * An {@code AppUser} holds {@code admin.User} {@code _RU_U}. Two layers keep that from reaching their roles through
 * the web: the access check both edit servlets make (the user's own administration view is not reachable to them),
 * and the SmartClient apply, which sets only the bindings of the view being edited (the Profile view the user can
 * reach holds name, email and mobile, not roles, password, user name or inactive).
 */
@SuppressWarnings("static-method")
class SelfServiceApplyH2Test extends AbstractH2Test {
	@Test
	void anApplyOfAProfileViewOnlySetsTheBindingsOfThatView() throws Exception {
		AbstractPersistence p = AbstractPersistence.get();
		User original = p.getUser();
		UserExtension me = appUser(p, "AppUser", "apply-me");
		User user = me.toMetaDataUser();
		p.setUser(user);
		try {
			Customer c = user.getCustomer();
			Module m = c.getModule("admin");
			Document d = m.getDocument(c, "UserDashboard");
			View view = d.getView("desktop", c, "edit");
			UserDashboard bean = UserDashboard.newInstance();
			UserExtension current = bean.getCurrentUser();
			String userName = current.getUserName();
			String password = current.getPassword();
			int roles = current.getRoles().size();
			Boolean inactive = current.getInactive();
			AbstractWebContext ctx = mockWebContext();
			ctx.setCurrentBean(bean);

			ViewJSONManipulator vjm = new ViewJSONManipulator(user, m, d, view, "desktop", bean, 0, 0, true);
			vjm.visit();
			vjm.applyJSON("{\"currentUser_contact_mobile\":\"0400 111 222\","
					+ "\"currentUser_userName\":\"hijacked\","
					+ "\"currentUser_password\":\"injected\","
					+ "\"currentUser_inactive\":true,"
					+ "\"currentUser_passwordExpired\":true,"
					+ "\"currentUser_roles\":[{\"roleName\":\"admin.SecurityAdministrator\"}],"
					+ "\"currentUser_groups\":[\"x\"]}", p, ctx);

			assertEquals("0400 111 222", current.getContact().getMobile(), "a binding of the view applies");
			assertEquals(userName, current.getUserName());
			assertEquals(password, current.getPassword());
			assertEquals(inactive, current.getInactive());
			assertEquals(roles, current.getRoles().size());
			assertNotEquals(Boolean.TRUE, current.getPasswordExpired());
		} finally {
			p.setUser(original);
		}
	}

	@Test
	void anAppUserAndAViewUserCannotOpenTheUserAdministrationDocuments() {
		AbstractPersistence p = AbstractPersistence.get();
		User original = p.getUser();
		try {
			for (String roleName : List.of("AppUser", "ViewUser")) {
				User user = appUser(p, roleName, "gate-" + roleName).toMetaDataUser();
				for (String uxui : List.of("desktop", "tablet", "phone", "external")) {
					for (String document : List.of("User", "UserProxy", "UserRole", "Group")) {
						UserAccess access = UserAccess.singular("admin", document);
						assertThrows(SecurityException.class,
								() -> EXT.checkAccess(user, access, uxui),
								roleName + " opening admin." + document + " in " + uxui);
					}
				}
			}
		} finally {
			p.setUser(original);
		}
	}

	@Test
	void aBasicUserAndAViewUserMayNotUpdateAUser() {
		AbstractPersistence p = AbstractPersistence.get();
		User original = p.getUser();
		try {
			for (String roleName : List.of("BasicUser", "ViewUser")) {
				User user = appUser(p, roleName, "update-" + roleName).toMetaDataUser();
				Customer c = user.getCustomer();
				Document users = c.getModule("admin").getDocument(c, "User");
				assertFalse(user.canUpdateDocument(users), roleName);
				assertFalse(user.canCreateDocument(users), roleName);
				assertFalse(user.canDeleteDocument(users), roleName);
			}
			User appUser = appUser(p, "AppUser", "update-AppUser").toMetaDataUser();
			Customer c = appUser.getCustomer();
			assertTrue(appUser.canUpdateDocument(c.getModule("admin").getDocument(c, "User")),
					"AppUser keeps the user-scoped update on their own record, which only the two layers above contain");
		} finally {
			p.setUser(original);
		}
	}

	private static UserExtension appUser(AbstractPersistence p, String roleName, String userName) {
		DataBuilder builder = new DataBuilder().fixture(FixtureType.crud);
		UserExtension me = builder.build("admin", "User");
		me.setUserName(userName + "-" + System.nanoTime());
		me.getGroups().clear();
		me.getRoles().clear();
		UserRole role = builder.build(UserRole.MODULE_NAME, UserRole.DOCUMENT_NAME);
		role.setRoleName("admin." + roleName);
		me.addRolesElement(role);
		me = p.save(me);
		p.flush();
		return me;
	}
}

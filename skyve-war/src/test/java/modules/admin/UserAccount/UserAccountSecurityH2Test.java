package modules.admin.UserAccount;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.UserAccess;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.UserAgentType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import modules.admin.domain.UserAccount;
import modules.admin.domain.UserLoginRecord;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/** Verifies Account login-history access without the SecurityAdministrator role. */
@SuppressWarnings("static-method")
class UserAccountSecurityH2Test extends AbstractH2Test {
	@Test
	void appUserCanLoadOnlyTheirOwnLoginHistory() {
		assertOwnLoginHistory("AppUser");
	}

	@Test
	void basicUserCanLoadOnlyTheirOwnLoginHistory() {
		assertOwnLoginHistory("BasicUser");
	}

	@Test
	void viewUserCanLoadOnlyTheirOwnLoginHistory() {
		assertOwnLoginHistory("ViewUser");
	}

	private static void assertOwnLoginHistory(String roleName) {
		AbstractPersistence persistence = AbstractPersistence.get();
		org.skyve.metadata.user.User originalUser = persistence.getUser();
		boolean originalAccessControl = UtilImpl.ACCESS_CONTROL;
		try {
			DataBuilder builder = new DataBuilder().fixture(FixtureType.crud);
			UserExtension accountUser = builder.build(User.MODULE_NAME, User.DOCUMENT_NAME);
			accountUser.setUserName("account-" + roleName);
			accountUser.getGroups().clear();
			accountUser.getRoles().clear();
			UserRole role = builder.build(UserRole.MODULE_NAME, UserRole.DOCUMENT_NAME);
			role.setRoleName("admin." + roleName);
			accountUser.addRolesElement(role);
			accountUser = persistence.save(accountUser);

			UserLoginRecord ownLogin = builder.build(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			ownLogin.setBizUserId(accountUser.getBizId());
			ownLogin.setUserName(accountUser.getUserName());
			ownLogin = persistence.save(ownLogin);
			UserLoginRecord otherLogin = builder.build(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			otherLogin.setBizUserId(originalUser.getId());
			otherLogin.setUserName(originalUser.getName());
			otherLogin = persistence.save(otherLogin);
			persistence.flush();

			org.skyve.metadata.user.User user = accountUser.toMetaDataUser();
			assertNotNull(user);
			assertTrue(user.isInRole("admin", roleName));
			assertFalse(user.isInRole("admin", "SecurityAdministrator"));
			UtilImpl.ACCESS_CONTROL = true;
			ProvidedRepositoryFactory.get().resetMenus(user);
			persistence.setUser(user);
			Document loginDocument = user.getCustomer().getModule(UserLoginRecord.MODULE_NAME)
					.getDocument(user.getCustomer(), UserLoginRecord.DOCUMENT_NAME);
			assertEquals(DocumentPermissionScope.user, user.getScope(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME));
			assertFalse(user.canCreateDocument(loginDocument));
			assertFalse(user.canUpdateDocument(loginDocument));
			assertFalse(user.canDeleteDocument(loginDocument));

			MockHttpServletRequest request = new MockHttpServletRequest();
			request.getSession().setAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME,
					UserAgentType.desktop);
			WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());
			String uxui = UserAgent.getSelection(request).getUxUi().getName();
			assertTrue(user.canAccess(UserAccess.singular(UserAccount.MODULE_NAME, UserAccount.DOCUMENT_NAME), uxui));

			// Exercise the same query-access and document-read checks that failed when rendering Account.
			SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), UserAccount.MODULE_NAME,
					null, "qMyLoginHistory", null, List.of(), List.of(), true);
			List<BeanMapAdapter> rows = model.load(0, 20, Map.of(), Map.of());
			assertEquals(1, model.getRowCount());
			assertEquals(1, rows.size());
			assertEquals(ownLogin.getBizId(), rows.get(0).getBean().getBizId());

			// User scope must also isolate records without the Account query's explicit owner filter.
			List<UserLoginRecord> visible = persistence.newDocumentQuery(UserLoginRecord.MODULE_NAME,
					UserLoginRecord.DOCUMENT_NAME).beanResults();
			assertEquals(1, visible.size());
			assertEquals(ownLogin.getBizId(), visible.get(0).getBizId());
			assertFalse(user.canReadBean(otherLogin.getBizId(), otherLogin.getBizModule(), otherLogin.getBizDocument(),
					otherLogin.getBizCustomer(), otherLogin.getBizDataGroupId(), otherLogin.getBizUserId()));
		}
		finally {
			WebContainer.clear();
			persistence.setUser(originalUser);
			UtilImpl.ACCESS_CONTROL = originalAccessControl;
		}
	}
}

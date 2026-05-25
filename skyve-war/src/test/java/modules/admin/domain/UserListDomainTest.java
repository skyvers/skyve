package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserListDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserList bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserList.MODULE_NAME, UserList.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserList bean = UserList.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserList", bean.getBizDocument());
	}

	@Test
	void emailListSetAndGet() throws Exception {
		UserList bean = UserList.newInstance();
		bean.setUserInvitiationEmailList("a@example.com,b@example.com");
		assertEquals("a@example.com,b@example.com", bean.getUserInvitiationEmailList());
	}

	@Test
	void bulkCreateWithEmailSetAndGet() throws Exception {
		UserList bean = UserList.newInstance();
		bean.setBulkCreateWithEmail(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getBulkCreateWithEmail());
	}

	@Test
	void defaultModuleNameSetAndGet() throws Exception {
		UserList bean = UserList.newInstance();
		bean.setDefaultModuleName("admin");
		assertEquals("admin", bean.getDefaultModuleName());
	}

	@Test
	void userInvitationGroupsListInitialized() throws Exception {
		UserList bean = UserList.newInstance();
		assertNotNull(bean.getUserInvitationGroups());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		UserList bean = UserList.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void isEmailConfiguredReturnsBooleanValue() throws Exception {
		UserList bean = UserList.newInstance();
		// Method delegates to static config, just verify it's callable
		assertEquals(!bean.isEmailConfigured(), bean.isNotEmailConfigured());
	}

	@Test
	void addAndRemoveUserInvitationGroupsElement() throws Exception {
		UserList bean = UserList.newInstance();
		modules.admin.Group.GroupExtension grp = new modules.admin.Group.GroupExtension();
		assertTrue(bean.addUserInvitationGroupsElement(grp));
		assertEquals(1, bean.getUserInvitationGroups().size());
		assertTrue(bean.removeUserInvitationGroupsElement(grp));
		assertTrue(bean.getUserInvitationGroups().isEmpty());
	}

	@Test
	void removeUserInvitationGroupsElementNotPresent() throws Exception {
		UserList bean = UserList.newInstance();
		modules.admin.Group.GroupExtension grp = new modules.admin.Group.GroupExtension();
		assertFalse(bean.removeUserInvitationGroupsElement(grp));
	}

	@Test
	void removeUserInvitationGroupsElementByIndex() throws Exception {
		UserList bean = UserList.newInstance();
		modules.admin.Group.GroupExtension grp = new modules.admin.Group.GroupExtension();
		bean.addUserInvitationGroupsElement(grp);
		modules.admin.domain.Group removed = bean.removeUserInvitationGroupsElement(0);
		assertEquals(grp, removed);
	}

	@Test
	void addUserInvitationGroupsElementAtIndex() throws Exception {
		UserList bean = UserList.newInstance();
		modules.admin.Group.GroupExtension grp = new modules.admin.Group.GroupExtension();
		bean.addUserInvitationGroupsElement(0, grp);
		assertEquals(1, bean.getUserInvitationGroups().size());
	}

	@Test
	void getUserInvitationGroupsElementById() throws Exception {
		UserList bean = UserList.newInstance();
		modules.admin.Group.GroupExtension grp = new modules.admin.Group.GroupExtension();
		bean.addUserInvitationGroupsElement(grp);
		// getElementById returns null when no match (new instance has no persisted bizId)
		// Just verify the method is callable without exception
		bean.getUserInvitationGroupsElementById(grp.getBizId());
	}
}

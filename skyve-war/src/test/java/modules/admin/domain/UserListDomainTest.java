package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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
}

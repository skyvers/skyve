package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserRoleDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserRole bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserRole.MODULE_NAME, UserRole.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserRole bean = UserRole.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserRole", bean.getBizDocument());
	}

	@Test
	void roleNameSetAndGet() throws Exception {
		UserRole bean = UserRole.newInstance();
		bean.setRoleName("admin.BasicUser");
		assertEquals("admin.BasicUser", bean.getRoleName());
	}

	@Test
	void bizOrdinalSetAndGet() throws Exception {
		UserRole bean = UserRole.newInstance();
		bean.setBizOrdinal(Integer.valueOf(2));
		assertEquals(Integer.valueOf(2), bean.getBizOrdinal());
	}

        @Test
        void getBizKeyNotNull() throws Exception {
                UserRole bean = UserRole.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void parentSetAndGet() {
                UserRole bean = new UserRole();
                modules.admin.User.UserExtension parent = new modules.admin.User.UserExtension();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
                // Setting same value is a no-op
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
        }
}

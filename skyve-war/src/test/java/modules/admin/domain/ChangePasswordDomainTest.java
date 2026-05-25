package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class ChangePasswordDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		ChangePassword bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ChangePassword.MODULE_NAME, ChangePassword.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ChangePassword", bean.getBizDocument());
	}

	@Test
	void oldPasswordSetAndGet() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		bean.setOldPassword("oldPass123");
		assertEquals("oldPass123", bean.getOldPassword());
	}

	@Test
	void newPasswordSetAndGet() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		bean.setNewPassword("newPass456");
		assertEquals("newPass456", bean.getNewPassword());
	}

	@Test
	void confirmPasswordSetAndGet() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		bean.setConfirmPassword("newPass456");
		assertEquals("newPass456", bean.getConfirmPassword());
	}

	@Test
	void responseSetAndGet() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		bean.setResponse("Password changed successfully");
		assertEquals("Password changed successfully", bean.getResponse());
	}

	@Test
	void isPasswordChangedWhenResponseSet() throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		assertFalse(bean.isPasswordChanged());
		assertTrue(bean.isNotPasswordChanged());
		bean.setResponse("done");
		assertTrue(bean.isPasswordChanged());
		assertFalse(bean.isNotPasswordChanged());
	}

        @Test
        void getBizKeyNotNull() throws Exception {
                ChangePassword bean = ChangePassword.newInstance();
                assertNotNull(bean.getBizKey());
        }
}

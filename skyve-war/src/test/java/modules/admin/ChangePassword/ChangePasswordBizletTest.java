package modules.admin.ChangePassword;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import modules.admin.domain.ChangePassword;

@SuppressWarnings("static-method")
public class ChangePasswordBizletTest {

	private static final ChangePasswordBizlet bizlet = new ChangePasswordBizlet();

	@Test
	void preRerenderWithUnknownSourceDoesNotThrow() throws Exception {
		ChangePassword bean = new ChangePassword();
		bizlet.preRerender("unknownSource", bean, null);
		assertNotNull(bean);
	}

	@Test
	void preRerenderWithNewPasswordSourceAndNullPasswordDoesNotThrow() throws Exception {
		ChangePassword bean = new ChangePassword();
		bean.setNewPassword(null);
		bizlet.preRerender(ChangePassword.newPasswordPropertyName, bean, null);
		assertNotNull(bean);
	}

	@Test
	void postRenderWithPasswordNotChangedDoesNotCallExt() {
		ChangePassword bean = new ChangePassword();
		// isPasswordChanged() returns false by default
		// postRender should not call EXT.getHttpServletRequest()
		bizlet.postRender(bean, null);
		assertNotNull(bean);
	}
}

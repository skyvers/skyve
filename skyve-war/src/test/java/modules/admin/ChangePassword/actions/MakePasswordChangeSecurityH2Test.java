package modules.admin.ChangePassword.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebContainer;
import org.skyve.persistence.Persistence;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import modules.admin.domain.ChangePassword;
import util.AbstractH2Test;

/**
 * The signed-in user's password change: the old password must be right, the new one complex enough, and a
 * successful change ends a forced change (expired flag, reset token). Each refusal leaves the stored hash alone.
 */
@SuppressWarnings("static-method")
class MakePasswordChangeSecurityH2Test extends AbstractH2Test {
	private static final String NEW = "Brand0NewPassword!";

	private int passwordHistoryRetention;

	@BeforeEach
	void setup() {
		passwordHistoryRetention = UtilImpl.PASSWORD_HISTORY_RETENTION;
		UtilImpl.PASSWORD_HISTORY_RETENTION = 0;
		WebContainer.setHttpServletRequestResponse(new MockHttpServletRequest(), new MockHttpServletResponse());
	}

	@AfterEach
	void cleanUp() {
		UtilImpl.PASSWORD_HISTORY_RETENTION = passwordHistoryRetention;
		WebContainer.clear();
	}

	@Test
	void aWrongOldPasswordIsRefusedAndThePasswordStays() {
		assertThrows(ValidationException.class, () -> change("not-the-old-password", NEW, NEW));

		assertTrue(EXT.checkPassword(PASSWORD, storedHash()));
	}

	@Test
	void theRightOldPasswordChangesThePasswordToTheNewOne() throws Exception {
		change(PASSWORD, NEW, NEW);

		assertTrue(EXT.checkPassword(NEW, storedHash()));
		assertFalse(EXT.checkPassword(PASSWORD, storedHash()));
	}

	@Test
	void aPasswordThatIsNotComplexEnoughIsRefusedAndThePasswordStays() {
		assertThrows(ValidationException.class, () -> change(PASSWORD, "abc", "abc"));

		assertTrue(EXT.checkPassword(PASSWORD, storedHash()));
	}

	@Test
	void aSuccessfulChangeEndsAForcedChangeAndClearsAResetToken() throws Exception {
		Persistence p = CORE.getPersistence();
		p.newSQL("update ADM_SecurityUser set passwordExpired = true, passwordResetToken = 'leftover' where bizId = :id")
				.putParameter("id", p.getUser().getId(), false)
				.execute();
		((UserImpl) p.getUser()).setPasswordChangeRequired(true);

		change(PASSWORD, NEW, NEW);

		assertFalse(p.getUser().isPasswordChangeRequired());
		assertEquals(Boolean.FALSE, p.newSQL("select passwordExpired from ADM_SecurityUser where bizId = :id")
				.putParameter("id", p.getUser().getId(), false)
				.scalarResult(Boolean.class));
		assertNull(p.newSQL("select passwordResetToken from ADM_SecurityUser where bizId = :id")
				.putParameter("id", p.getUser().getId(), false)
				.scalarResult(String.class));
	}

	@Test
	void aSignedInUserCannotChangeThePasswordWithoutGivingTheOldOne() {
		assertThrows(ValidationException.class, () -> change(null, NEW, NEW));
		assertThrows(ValidationException.class, () -> change("", NEW, NEW));

		assertTrue(EXT.checkPassword(PASSWORD, storedHash()));
	}

	private static void change(String oldPassword, String newPassword, String confirmPassword) throws Exception {
		ChangePassword bean = ChangePassword.newInstance();
		bean.setOldPassword(oldPassword);
		bean.setNewPassword(newPassword);
		bean.setConfirmPassword(confirmPassword);
		new MakePasswordChange().execute(bean, null);
	}

	private static String storedHash() {
		Persistence p = CORE.getPersistence();
		return p.newSQL("select password from ADM_SecurityUser where bizId = :id")
				.putParameter("id", p.getUser().getId(), false)
				.scalarResult(String.class);
	}
}

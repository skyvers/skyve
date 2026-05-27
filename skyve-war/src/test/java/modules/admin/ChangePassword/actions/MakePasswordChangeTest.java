package modules.admin.ChangePassword.actions;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebContainer;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import modules.admin.domain.ChangePassword;
import util.AbstractH2Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class MakePasswordChangeTest extends AbstractH2Test {

	@BeforeEach
	@SuppressWarnings("static-method")
	void beforeEach() {
		HttpServletRequest httpServletRequest = new MockHttpServletRequest(); // mock session too
		HttpServletResponse httpServletResponse = new MockHttpServletResponse();
		WebContainer.setHttpServletRequestResponse(httpServletRequest, httpServletResponse);
	}

	@Test
	@SuppressWarnings("static-method")
	void testNewAndConfirmPasswordNotTheSame() {
		Assert.assertThrows(ValidationException.class, () -> changePassword("Password0!", "Password0@"));
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testOldPasswordMatchesNewPassword() {
		Assert.assertThrows(ValidationException.class, () -> changePassword(PASSWORD));
	}
	
	@Test
	@SuppressWarnings("static-method")
	void checkNoPasswordHistory() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 0;

		// change password
		changePassword("Password0!0!");
		assertEquals(0, getPasswordHistory(false).length);
		// change password back
		changePassword(PASSWORD);
		assertEquals(0, getPasswordHistory(false).length);
	}
	
	@Test
	@SuppressWarnings("static-method")
	void checkPasswordHistoryOfOne() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 1;

		// change password
		changePassword("Password0!0!"); // sets the password history

		Assert.assertEquals(1, getPasswordHistory(false).length);
		
		// change password back
		changePassword(PASSWORD);

		Assert.assertEquals(1, getPasswordHistory(false).length);

		// change password
		changePassword("Password0!0!");

		Assert.assertEquals(1, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	void checkPasswordHistoryOfTwoThrows() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		// change password
		changePassword("Password0!0!"); // sets the password history
		Assert.assertEquals(1, getPasswordHistory(false).length);

		// change password back
		changePassword(PASSWORD);
		Assert.assertEquals(2, getPasswordHistory(false).length);

		// reusing a recent password should throw
		Assert.assertThrows(ValidationException.class, () -> changePassword("Password0!0!"));
	}

	@Test
	@SuppressWarnings("static-method")
	void checkPasswordHistoryOfTwo() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		// change password
		changePassword("Password0!0!"); // sets the password history

		Assert.assertEquals(1, getPasswordHistory(false).length);
		
		// change password back
		changePassword(PASSWORD);

		Assert.assertEquals(2, getPasswordHistory(false).length);

		// change password
		changePassword("Password0@0@");

		Assert.assertEquals(2, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	void checkLargePasswordHistoryCulledToZero() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!0!");
		changePassword("Password0@0@");
		changePassword("Password0#0#");
		changePassword("Password0$0$");
		changePassword("Password0%0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 0;

		changePassword("Password0^0^");
		Assert.assertEquals(0, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	void checkLargePasswordHistoryCulledToOne() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!0!");
		changePassword("Password0@0@");
		changePassword("Password0#0#");
		changePassword("Password0$0$");
		changePassword("Password0%0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 1;

		changePassword("Password0^0^");
		Assert.assertEquals(1, getPasswordHistory(false).length);

		changePassword("Password0&0&");
		Assert.assertEquals(1, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	void checkLargePasswordHistoryCulledToTwo() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!0!");
		changePassword("Password0@0@");
		changePassword("Password0#0#");
		changePassword("Password0$0$");
		changePassword("Password0%0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		changePassword("Password0^0^");
		Assert.assertEquals(2, getPasswordHistory(false).length);

		changePassword("Password0!0!");
		Assert.assertEquals(2, getPasswordHistory(false).length);
	}
	
	private static void changePassword(String newPassword, String confirmPassword) throws Exception {
		ChangePassword changePassword = ChangePassword.newInstance();
		changePassword.setNewPassword(newPassword);
		changePassword.setConfirmPassword(confirmPassword);
		new MakePasswordChange().execute(changePassword, null);
	}

	private static void changePassword(String newPassword) throws Exception {
		changePassword(newPassword, newPassword);
	}

	private static String[] getPasswordHistory(boolean print) {
		String history = CORE.getPersistence().newSQL("select passwordHistory from ADM_SecurityUser").scalarResult(String.class);
		if (print) System.out.println(history);
		return (history == null) ? new String[0] : history.split("\\t");
	}
}

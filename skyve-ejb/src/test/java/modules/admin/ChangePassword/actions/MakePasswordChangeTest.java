package modules.admin.ChangePassword.actions;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.ChangePassword;
import util.AbstractH2Test;

public class MakePasswordChangeTest extends AbstractH2Test {
	@Test(expected = ValidationException.class)
	@SuppressWarnings("static-method")
	public void testNewAndConfirmPasswordNotTheSame() throws Exception {
		changePassword("Password0!", "Password0@");
	}
	
	@Test(expected = ValidationException.class)
	@SuppressWarnings("static-method")
	public void testOldPasswordMatchesNewPassword() throws Exception {
		changePassword(PASSWORD);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void checkNoPasswordHistory() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 0;

		// change password
		changePassword("Password0!");
		// change password back
		changePassword(PASSWORD);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void checkPasswordHistoryOfOne() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 1;

		// change password
		changePassword("Password0!"); // sets the password history

		Assert.assertEquals(1, getPasswordHistory(false).length);
		
		// change password back
		changePassword(PASSWORD);

		Assert.assertEquals(1, getPasswordHistory(false).length);

		// change password
		changePassword("Password0!");

		Assert.assertEquals(1, getPasswordHistory(false).length);
	}

	@Test(expected = ValidationException.class)
	@SuppressWarnings("static-method")
	public void checkPasswordHistoryOfTwoThrows() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		// change password
		changePassword("Password0!"); // sets the password history

		Assert.assertEquals(1, getPasswordHistory(false).length);
		
		// change password back
		changePassword(PASSWORD);

		Assert.assertEquals(2, getPasswordHistory(false).length);

		// change password
		changePassword("Password0!");
	}

	@Test
	@SuppressWarnings("static-method")
	public void checkPasswordHistoryOfTwo() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		// change password
		changePassword("Password0!"); // sets the password history

		Assert.assertEquals(1, getPasswordHistory(false).length);
		
		// change password back
		changePassword(PASSWORD);

		Assert.assertEquals(2, getPasswordHistory(false).length);

		// change password
		changePassword("Password0@");

		Assert.assertEquals(2, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void checkLargePasswordHistoryCulledToZero() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!");
		changePassword("Password0@");
		changePassword("Password0#");
		changePassword("Password0$");
		changePassword("Password0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 0;

		changePassword("Password0^");
		Assert.assertEquals(0, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void checkLargePasswordHistoryCulledToOne() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!");
		changePassword("Password0@");
		changePassword("Password0#");
		changePassword("Password0$");
		changePassword("Password0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 1;

		changePassword("Password0^");
		Assert.assertEquals(1, getPasswordHistory(false).length);

		changePassword("Password0&");
		Assert.assertEquals(1, getPasswordHistory(false).length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void checkLargePasswordHistoryCulledToTwo() throws Exception {
		UtilImpl.PASSWORD_HISTORY_RETENTION = 5;

		// Set some password history
		changePassword("Password0!");
		changePassword("Password0@");
		changePassword("Password0#");
		changePassword("Password0$");
		changePassword("Password0%");

		UtilImpl.PASSWORD_HISTORY_RETENTION = 2;

		changePassword("Password0^");
		Assert.assertEquals(2, getPasswordHistory(false).length);

		changePassword("Password0!");
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

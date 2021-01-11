package modules.admin.UserList;

import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.domain.Contact;
import modules.admin.domain.User;

public class UserListUtil {

	public static final String SYSTEM_USER_INVITATION = "SYSTEM User Invitation";
	public static final String SYSTEM_USER_INVITATION_DEFAULT_SUBJECT = "Invitation to join";
	public static final String SYSTEM_USER_INVITATION_DEFAULT_BODY = String.format("Hi {%s}, a user account has been created for you.<br /><br />" +
			"Please click below to reset your password.<br />" +
			"<a href=\"%s/pages/resetPassword.jsp?t={%s}\">Reset Password</a>",
			Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
			Util.getSkyveContextUrl(),
			User.passwordResetTokenPropertyName);
	
	

}

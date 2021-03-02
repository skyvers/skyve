package modules.admin.SelfRegistration;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.Binder;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.SelfRegistration;
import modules.admin.domain.User;

public class SelfRegistrationExtension extends SelfRegistration {

	private static final long serialVersionUID = 4437526012340020305L;

	static final String CONFIRM_PASSWORD_REQUIRED = "Confirm Password is required.";
	static final String PASSWORD_MISMATCH = "You did not type the same password.  Please re-enter the password again.";
	static final String PASSWORD_REQUIRED = "Password is required.";

	/**
	 * Validates that the password and confirmPassword entered during a SelfRegistration
	 * match. Throws a {@link ValidationException} if there are any problems.
	 * @throws Exception 
	 */
	public void validateConfirmPassword() {
		if (getUser() != null) {
			ValidationException ve = new ValidationException();
			if ((getUser().getPassword() == null) || StringUtils.isEmpty(getUser().getPassword())) {
				Message message = new Message(Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.passwordPropertyName), PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}
			
			if ((getConfirmPassword() == null) || StringUtils.isEmpty(getConfirmPassword())) {
				Message message = new Message(SelfRegistration.confirmPasswordPropertyName, CONFIRM_PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}
			
			if (!ve.getMessages().isEmpty()) {
				throw ve;
			}
			
			// if both aren't null, check that they are the same
			if (!getUser().getPassword().equals(getConfirmPassword())) {
				Message message = new Message(SelfRegistration.confirmPasswordPropertyName, PASSWORD_MISMATCH);
				ve.getMessages().add(message);
			}
			
			if (!ve.getMessages().isEmpty()) {
				throw ve;
			}
			
			// if they are the same, check the password is suitably complex
			// check for suitable complexity
			ConfigurationExtension configuration = Configuration.newInstance();
			if (!configuration.meetsComplexity(getUser().getPassword())) {
				StringBuilder sb = new StringBuilder(64);
				sb.append("The password you have entered is not sufficiently complex.\n");
				sb.append(configuration.getPasswordRuleDescription());
				sb.append("\nPlease re-enter and confirm the password.");
				Message message = new Message(Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.passwordPropertyName), sb.toString());
				ve.getMessages().add(message);
			}
			
			if(!ve.getMessages().isEmpty()) {
				throw ve;
			}
		}
	}

}

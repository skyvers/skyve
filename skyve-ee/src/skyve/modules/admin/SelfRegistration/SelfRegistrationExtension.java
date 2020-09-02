package modules.admin.SelfRegistration;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.SelfRegistration;
import modules.admin.domain.User;

public class SelfRegistrationExtension extends SelfRegistration {

	private static final long serialVersionUID = 4437526012340020305L;

	static final String CONFIRM_PASSWORD_REQUIRED = "Confirm Password is required.";
	static final String PASSWORD_MISMATCH = "You did not type the same password.  Please re-enter the password again.";
	static final String PASSWORD_REQUIRED = "Password is required.";

	private ConfigurationExtension configuration;
	
	 /**
	 * Get the configuration object on-demand.
	 */
	@Override
	public ConfigurationExtension getConfiguration() {
		try {
			if (configuration == null) {
				configuration = Configuration.newInstance();
			}
			
			return configuration;
		} catch (Exception e) {
			throw new IllegalStateException("Failed to load configuration", e);
		}
	}
	
	@Override
	public String getLoginUrl() {
		return Util.getSkyveContextUrl() + "/login";
	}

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
	
	@Override
	public String getLoginMessage() {
		return String.format("If you are an existing user, please <a href=\"%s\">Log in</a>", getLoginUrl());
	}

	private Boolean selfRegistrationAllowed = null;
	
	@Override
	public boolean isSelfRegistrationAllowed() {
		if (selfRegistrationAllowed == null) {
			selfRegistrationAllowed = getConfiguration().getAllowUserSelfRegistration();
			if (selfRegistrationAllowed == null) {
				selfRegistrationAllowed = Boolean.FALSE;
			}
		}
		return selfRegistrationAllowed.booleanValue();
	}
}

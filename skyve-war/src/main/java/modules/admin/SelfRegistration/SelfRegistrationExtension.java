package modules.admin.SelfRegistration;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.validator.GenericValidator;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.BeanValidator;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.SelfRegistration;
import modules.admin.domain.User;

public class SelfRegistrationExtension extends SelfRegistration {

	private static final long serialVersionUID = 4437526012340020305L;

	private static final String EMAIL_MISMATCH = "You did not type the same email. Please re-enter your email again.";
	private static final String OWASP_EMAIL_PATTERN = "^[a-zA-Z0-9_+&*-]+(?:\\.[a-zA-Z0-9_+&*-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}$";

	static final String CONFIRM_PASSWORD_REQUIRED = "Confirm Password is required.";
	static final String PASSWORD_MISMATCH = "You did not type the same password.  Please re-enter the password again.";
	static final String PASSWORD_REQUIRED = "Password is required.";

	/**
	 * Validates that the confirm email address is valid.
	 * 
	 * @return true if not-null and is invalid, false otherwise
	 */
	public boolean confirmEmailInvalid() {
		if (getConfirmEmail() != null) {
			return !validateEmailAddress(getConfirmEmail());
		}
		return false;
	}

	/**
	 * Validates that the email address is valid.
	 * 
	 * @return true if not-null and is valid, false otherwise
	 */
	public boolean emailInvalid() {
		if (getUser() != null && getUser().getContact() != null && getUser().getContact().getEmail1() != null) {
			return !validateEmailAddress(getUser().getContact().getEmail1());
		}
		return false;
	}

	/**
	 * This is called from a Skyve EL expression within the recaptcha blurb in the edit view.
	 * @return	The site key.
	 */
	public String getSiteKey() {
		if(isShowGoogleRecaptcha()) {
			return UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY;
		}else if(isShowCloudflareTurnstile()) {
			return UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY;
		}else {
			return null;
		}
	}

	/**
	 * Validates that the password and confirmPassword entered during a SelfRegistration
	 * match.
	 * 
	 * @throws A {@link ValidationException} if there are any problems.
	 */
	public void validateConfirmPassword() {
		if (getUser() != null) {
			ValidationException ve = new ValidationException();
			if (StringUtils.isEmpty(getUser().getPassword())) {
				Message message = new Message(Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.passwordPropertyName), PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}

			if (StringUtils.isEmpty(getConfirmPassword())) {
				Message message = new Message(SelfRegistration.confirmPasswordPropertyName, CONFIRM_PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}

			if (!ve.getMessages().isEmpty()) {
				throw ve;
			}
			
			// if both aren't null, check that they are the same
			if (!getUser().getPassword()
					.equals(getConfirmPassword())) {
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

	/**
	 * Validates that the email and confirmEmail entered during a SelfRegistration
	 * match. Throws a {@link ValidationException} if there are any problems.
	 */
	public void validateConfirmEmail() {
		if (getUser() != null) {
			Contact contact = getUser().getContact();
			if (contact != null) {
				ValidationException ve = new ValidationException();
				if (StringUtils.isEmpty(contact.getEmail1())) {
					String emailBinding = Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.contactPropertyName,
							Contact.email1PropertyName);
					Message message = new Message(emailBinding,
							Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY,
									Binder.formatMessage("{disp:" + emailBinding + "}", this)));
					ve.getMessages().add(message);
				}

				if (StringUtils.isEmpty(getConfirmEmail())) {
					Message message = new Message(SelfRegistration.confirmEmailPropertyName,
							Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY,
									Binder.formatMessage("{disp:" + SelfRegistration.confirmEmailPropertyName + "}", this)));
					ve.getMessages().add(message);
				}

				if (!ve.getMessages().isEmpty()) {
					throw ve;
				}

				// if both aren't null, check that they are the same
				if (!getUser().getContact().getEmail1().equals(getConfirmEmail())) {
					Message message = new Message(SelfRegistration.confirmEmailPropertyName, EMAIL_MISMATCH);
					ve.getMessages().add(message);
				}

				if (!ve.getMessages().isEmpty()) {
					throw ve;
				}
			}
		}
	}

	/**
	 * Checks if the specified email address conforms to the OWASP email regular expression.
	 * Used to show a warning to the user during registration if it looks like the
	 * email address is not valid.
	 * 
	 * @param email The email address to validate
	 * 
	 * @see https://owasp.org/www-community/OWASP_Validation_Regex_Repository
	 * @return true if the email address is valid, false otherwise.
	 */
	private static boolean validateEmailAddress(final String email) {
		return GenericValidator.matchRegexp(email, OWASP_EMAIL_PATTERN);
	}
}
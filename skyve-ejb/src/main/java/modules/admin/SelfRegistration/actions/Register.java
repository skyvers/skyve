package modules.admin.SelfRegistration.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.Group.GroupExtension;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.User;

/**
 * Action to register a new user, giving them appropriate permissions and sending a
 * registration email to confirm their user account.
 */
public class Register implements ServerSideAction<SelfRegistrationExtension> {

	private static final Logger LOGGER = LoggerFactory.getLogger(Register.class);

	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();
		UserExtension result = bean.getUser();
		if (result != null && result.getContact() != null) {
			// validate the email and confirm email match
			bean.validateConfirmEmail();

			try {
				// Update the username to be the same as the email
				result.setUserName(result.getContact().getEmail1());

				// Should be a person registering for an account
				result.getContact().setContactType(Contact.ContactType.person);

				String unencodedPassword = result.getPassword();
				try {
					// validate the password and confirm password match
					bean.validateConfirmPassword();

					// Encode password
					encodePassword(result);

					final GroupExtension selfRegistrationGroup = Configuration.newInstance().getUserSelfRegistrationGroup();
					if (selfRegistrationGroup != null) {
						result.getGroups().add(selfRegistrationGroup);
					} else {
						LOGGER.error(
								"Self registration failed because no self-registration group has been set in the configuration.");
						throw new ValidationException(new Message(
								"Registration cannot be completed at this time, please contact an administrator for assistance."));
					}

					// Validate
					// perform UserRegistration specific validations first
					BeanValidator.validateBeanAgainstBizlet(bean);

					BeanValidator.validateBeanAgainstDocument(result.getContact());
					BeanValidator.validateBeanAgainstDocument(result);
					BeanValidator.validateBeanAgainstBizlet(result);
				} catch (ValidationException e) {
					// Reset roles
					result.getGroups().clear();
					// Decode password
					result.setPassword(unencodedPassword);
					// Rethrow validation exceptions
					throw e;
				}

				// generate the activation code and save the new user
				result.generateActivationDetailsAndSave(persistence);

				// Send registration email to the new user
				sendRegistrationEmail(bean);
				
				// Update and save bean for view and action control
				result.setEmailSent(Boolean.TRUE);
				result = CORE.getPersistence().save(result);

				webContext.growl(MessageSeverity.info, String.format(
						"An activation email has been sent to %s. Please use the link in the email to activate your account prior to signing in.",
						result.getContact().getEmail1()));
				
			} catch (Exception e) {
				throw e;
			}
		}
		return new ServerSideActionResult<>(bean);
	}

	private static void encodePassword(User user) throws Exception {
		user.setPassword(EXT.hashPassword(user.getPassword()));
	}

	private static void sendRegistrationEmail(SelfRegistrationExtension bean) throws Exception {
		try {
			// Send the registration email
			CORE.getPersistence().begin();
			bean.getUser().sendUserRegistrationEmail();
			CORE.getPersistence().commit(false);
		} catch (Exception e) {
			LOGGER.warn("Self Registration successful but email failed to send.", e);
		}
	}
}

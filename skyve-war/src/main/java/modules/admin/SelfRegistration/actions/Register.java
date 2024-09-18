package modules.admin.SelfRegistration.actions;

import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.security.HIBPPasswordValidator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import modules.admin.Group.GroupExtension;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.User;

/**
 * Action to register a new user, giving them appropriate permissions and sending a
 * registration email to confirm their user account.
 */
public class Register implements ServerSideAction<SelfRegistrationExtension> {

	private static final Logger LOGGER = LoggerFactory.getLogger(Register.class);

	@Inject
	private GeoIPService geoIPService;
	
	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();

		if (bean.getUser() != null && bean.getUser().getContact() != null) {
			// Get and validate the recaptcha response from the request parameters if captcha is set
			if (bean.isShowGoogleRecaptcha() || bean.isShowCloudflareTurnstile()) {
				HttpServletRequest request = EXT.getHttpServletRequest();
				String captchaResponse = null;
				if (bean.isShowGoogleRecaptcha()) {
					captchaResponse = request.getParameter("g-recaptcha-response");
				} else if(bean.isShowCloudflareTurnstile()) {
					captchaResponse = request.getParameter("cf-turnstile-response");
				}
				if ((captchaResponse == null) || (! WebUtil.validateRecaptcha(captchaResponse))) {
					throw new ValidationException("Captcha is not valid");
				}
			}
			// If configured, check the country and if it is on the blacklist/whitelist
			if (geoIPService.isBlocking()) {
				HttpServletRequest request = EXT.getHttpServletRequest();
				String clientIPAddress = SecurityUtil.getSourceIpAddress(request);
				LOGGER.info("Checking country for IP " + clientIPAddress);
				IPGeolocation geolocation = geoIPService.geolocate(clientIPAddress);
				if (geolocation.isBlocked()) {
					Contact contact = bean.getUser().getContact();
					String message = "Self-registration failed because country " + geolocation.countryCode() +
										(geoIPService.isWhitelist() ? " is not on the whitelist" : " is on the blacklist") + 
										". Suspected bot submission for " + contact.getName() + " - " + contact.getEmail1();
					LOGGER.warn(message);

					// Record security event
					SecurityUtil.log("GEO IP Block", message);

					// Silently pass
					bean.setPassSilently(Boolean.TRUE);
					return new ServerSideActionResult<>(bean);
				}
			}
			
			// validate the email and confirm email match
			bean.validateConfirmEmail();

			try {
				// Update the username to be the same as the email
				bean.getUser().setUserName(bean.getUser().getContact().getEmail1());

				// Should be a person registering for an account
				bean.getUser().getContact().setContactType(Contact.ContactType.person);

				String unencodedPassword = bean.getUser().getPassword();

				// If enabled, evaluate if password is breached
				if (UtilImpl.CHECK_FOR_BREACHED_PASSWORD) {
					if (HIBPPasswordValidator.isPasswordPwned(unencodedPassword)) {
						// If user hasn't been already warned about this password
						if (bean.getPreviouslyAttemptedPassword() == null
								|| !EXT.checkPassword(unencodedPassword, bean.getPreviouslyAttemptedPassword())) {
							// Warn user
							bean.setPreviouslyAttemptedPassword(EXT.hashPassword(unencodedPassword));
							webContext.growl(MessageSeverity.warn, Util.i18n("warning.breachedPasswordConfirm"));
							return new ServerSideActionResult<>(bean);
						}
					}
				}
				try {
					// validate the password and confirm password match
					bean.validateConfirmPassword();

					// Encode password
					encodePassword(bean.getUser());

					final GroupExtension selfRegistrationGroup = Configuration.newInstance().getUserSelfRegistrationGroup();
					if (selfRegistrationGroup != null) {
						bean.getUser().getGroups().add(selfRegistrationGroup);
					} else {
						LOGGER.error(
								"Self registration failed because no self-registration group has been set in the configuration.");
						throw new ValidationException(new Message(
								"Registration cannot be completed at this time, please contact an administrator for assistance."));
					}

					// Validate
					// perform UserRegistration specific validations first
					BeanValidator.validateBeanAgainstBizlet(bean);

					BeanValidator.validateBeanAgainstDocument(bean.getUser().getContact());
					BeanValidator.validateBeanAgainstDocument(bean.getUser());
					BeanValidator.validateBeanAgainstBizlet(bean.getUser());
				} catch (ValidationException e) {
					// Reset roles
					bean.getUser().getGroups().clear();
					// Decode password
					bean.getUser().setPassword(unencodedPassword);
					// Rethrow validation exceptions
					throw e;
				}

				// generate the activation code and save the new user
				bean.getUser().generateActivationDetailsAndSave(persistence);

				// Send registration email to the new user
				sendRegistrationEmail(bean);
				
				webContext.growl(MessageSeverity.info, String.format(
						"An activation email has been sent to %s. Please use the link in the email to activate your account prior to signing in.",
						bean.getUser().getContact().getEmail1()));
				
			} catch (Exception e) {
				// reset the recaptcha on an error
				PrimeFaces pf = PrimeFaces.current();
				if (pf.isAjaxRequest()) {
					pf.executeScript("if(document.getElementById('g-recaptcha-response')){try{grecaptcha.reset();}catch(error){PrimeFaces.error(error);}}");
				}
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

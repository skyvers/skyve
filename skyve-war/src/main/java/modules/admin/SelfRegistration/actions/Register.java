package modules.admin.SelfRegistration.actions;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.StringTokenizer;

import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.cdi.GeoIpService;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import modules.admin.Group.GroupExtension;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.Startup.StartupExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;
import modules.admin.domain.Startup.CountryListType;

/**
 * Action to register a new user, giving them appropriate permissions and sending a
 * registration email to confirm their user account.
 */
public class Register implements ServerSideAction<SelfRegistrationExtension> {

	private static final String WHITE_LIST = CountryListType.whitelist.name();

	private static final String BLACK_LIST = CountryListType.blacklist.name();

	private static final Logger LOGGER = LoggerFactory.getLogger(Register.class);

	@Inject
	private transient GeoIpService geoIpService;
	
	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();

		if (bean.getUser() != null && bean.getUser().getContact() != null) {
			// Get and validate the recaptcha response from the request parameters if captcha is set
			if(bean.isShowGoogleRecaptcha() || bean.isShowCloudflareTurnstile()) {
				HttpServletRequest request = EXT.getHttpServletRequest();
				String captchaResponse = null;
				if(bean.isShowGoogleRecaptcha()) {
					captchaResponse = request.getParameter("g-recaptcha-response");
				} else if(bean.isShowCloudflareTurnstile()) {
					captchaResponse = request.getParameter("cf-turnstile-response");
				}
				if ((captchaResponse == null) || (! WebUtil.validateRecaptcha(captchaResponse))) {
					throw new ValidationException("Captcha is not valid");
				}
			}
			// check the country and if it is on the blacklist, fail
			StartupExtension startup = Startup.newInstance();
			if(startup.isHasIpInfoToken()) {
				HttpServletRequest request = (HttpServletRequest) webContext.getHttpServletRequest();
				String clientIpAddress = getClientIpAddress(request);
				LOGGER.info("Checking country for ip " + clientIpAddress);
				Optional<String> countryCode = geoIpService.getCountryCodeForIp(clientIpAddress);
				if (countryCode.isPresent()) {
					LOGGER.info("Registration request from country " + countryCode.get());
					if(UtilImpl.COUNTRY_CODES != null) {
						List<String> countryList = Arrays.asList(UtilImpl.COUNTRY_CODES.split("//|"));
						// Is country on list
						boolean found = countryList.stream()
								.anyMatch(s -> s.equalsIgnoreCase(countryCode.get()));
						if (found) {
							// Check if the list is a blacklist and ban the country if it is
							if(UtilImpl.COUNTRY_LIST_TYPE.equalsIgnoreCase(BLACK_LIST)) {
								LOGGER.warn(
										"Self-registration failed because country was on the blacklist. Suspect bot submission for "
												+ bean.getUser().getContact().getName() + ", " + bean.getUser().getContact().getEmail1());
								bean.setPassSilently(Boolean.TRUE);
								return new ServerSideActionResult<>(bean);
							}
						} else if(!found) {
							// Check if the list is a white list and ban the country if the list is a white list
							if(UtilImpl.COUNTRY_LIST_TYPE.equalsIgnoreCase(WHITE_LIST)) {
								LOGGER.warn(
										"Self-registration failed because country was not on the whitelist. Suspect bot submission for "
												+ bean.getUser().getContact().getName() + ", " + bean.getUser().getContact().getEmail1());
								bean.setPassSilently(Boolean.TRUE);
								return new ServerSideActionResult<>(bean);
							}
						}
					}
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
	
	private static String getClientIpAddress(HttpServletRequest request) {
		String xForwardedForHeader = request.getHeader("X-Forwarded-For");
		if (xForwardedForHeader == null) {
			return request.getRemoteAddr();
		}

		// As of https://en.wikipedia.org/wiki/X-Forwarded-For
		// The general format of the field is: X-Forwarded-For: client, proxy1, proxy2 ...
		// we only want the client
		return new StringTokenizer(xForwardedForHeader, ",").nextToken().trim();
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

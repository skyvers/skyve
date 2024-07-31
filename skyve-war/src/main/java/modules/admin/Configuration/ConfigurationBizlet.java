package modules.admin.Configuration;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.util.BeanValidator;
import org.skyve.util.Binder;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.web.WebContext;

import modules.admin.Startup.StartupBizlet;
import modules.admin.Startup.StartupExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.TwoFactorType;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;

public class ConfigurationBizlet extends SingletonCachedBizlet<ConfigurationExtension> {

	private static final String TFA_CODE_EXPRESSION = "{tfaCode}";
	private static final String RESET_PASSWORD_URL_EXPRESSION = "{#resetPasswordUrl}";

	@Override
	public ConfigurationExtension newInstance(ConfigurationExtension bean) throws Exception {
		// temporarily elevate access to find existing configuration regardless of user
		ConfigurationExtension result = newInstance(bean, DocumentPermissionScope.customer);

		// Set the startup and set the emailFrom to the startup mailsender
		if (result.getStartup() == null) {
			result.setStartup(Startup.newInstance());
			result.setEmailFrom(result.getStartup().getMailSender());
		}

		if (result.getPasswordMinLength() == null) {
			result.setPasswordMinLength(Integer.valueOf(ConfigurationExtension.PASSWORD_DEFAULT_MIN_LENGTH));
		}
		if (result.getFromEmail() == null) {
			result.setFromEmail(UtilImpl.SMTP_SENDER);
		}
		if (result.getPasswordResetEmailSubject() == null) {
			result.setPasswordResetEmailSubject("Password Reset");
		}
		if (result.getPasswordResetEmailBody() == null) {
			String body = String.format("<html><head/><body>Hi {%s},<p/>" +
					"Please click below to reset your password.<p/>" +
					"<a href=\"{#resetPasswordUrl}\">" +
					"Reset Password</a></body></html>",
					Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName)
				);
			result.setPasswordResetEmailBody(body);
		}

		// initialise the startup bean
		if (result.getStartup() == null) {
			StartupExtension startup = Startup.newInstance();
			result.setStartup(startup);
		}
		
		return result;
	}
	
	@Override
	public List<String> complete(String attributeName, String value, ConfigurationExtension bean) throws Exception {
		if (Configuration.twoFactorEmailBodyPropertyName.equals(attributeName)) {
			if ((value == null) || (TFA_CODE_EXPRESSION.startsWith(value))) {
				return Collections.singletonList(TFA_CODE_EXPRESSION);
			}
		}
		else if (Configuration.passwordResetEmailSubjectPropertyName.equals(attributeName)) {
			Customer c = CORE.getCustomer();
			Module m = c.getModule(User.MODULE_NAME);
			Document d = m.getDocument(c, User.DOCUMENT_NAME);
			return ExpressionEvaluator.completeExpression(value, c, m, d);
		}
		else if (Configuration.passwordResetEmailBodyPropertyName.equals(attributeName)) {
			Customer c = CORE.getCustomer();
			Module m = c.getModule(User.MODULE_NAME);
			Document d = m.getDocument(c, User.DOCUMENT_NAME);
			List<String> result = ExpressionEvaluator.completeExpression(value, c, m, d);
			if ((value == null) || (RESET_PASSWORD_URL_EXPRESSION.startsWith(value))) {
				if (result.isEmpty()) {
					result = Collections.singletonList(RESET_PASSWORD_URL_EXPRESSION);
				}
				else {
					result.add(0, RESET_PASSWORD_URL_EXPRESSION);
				}
			}
			return result;
		}
		
		return Collections.emptyList();
	}
	
	@Override
	public ConfigurationExtension preExecute(ImplicitActionName actionName, ConfigurationExtension bean, Bean parentBean,
			WebContext webContext) throws Exception {

		if (ImplicitActionName.New.equals(actionName)) {
			// initialise the startup bean
			StartupExtension startup = Startup.newInstance();
			startup.loadProperties();
			bean.setStartup(startup);

			// read in password security settings from json
			bean.setPasswordHistoryRetention(
					UtilImpl.PASSWORD_HISTORY_RETENTION > 0 ? String.valueOf(UtilImpl.PASSWORD_HISTORY_RETENTION) : "");
			bean.setPasswordExpiryDays(
					UtilImpl.PASSWORD_EXPIRY_IN_DAYS > 0 ? String.valueOf(UtilImpl.PASSWORD_EXPIRY_IN_DAYS) : "");
			bean.setPasswordAccountLockoutDuration(UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS > 0
					? String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS)
					: "");
			bean.setPasswordAccountLockoutThreshold(
					UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD > 0 ? String.valueOf(UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD) : "");

			// default the two-factor type
			if (bean.getTwoFactorType() == null) {
				bean.setTwoFactorType(TwoFactorType.off);
			}
		} else if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
			// validate the startup values
			BeanValidator.validateBeanAgainstBizlet(bean.getStartup());
			// save any modified values to the override json
			bean.getStartup().saveConfiguration();
		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, ConfigurationExtension bean, WebContext webContext) throws Exception {

		if (Binder.createCompoundBinding(Configuration.startupPropertyName, Startup.mapTypePropertyName).equals(source)) {
			// set the default layers for the selected map type
			switch (bean.getStartup().getMapType()) {
				case gmap:
					bean.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_GMAP);
					break;
				case leaflet:
					bean.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_OPEN_STREET_MAP);
					break;
				default:
					break;
			}
		} else if (Binder.createCompoundBinding(Configuration.startupPropertyName, Startup.backupTypePropertyName).equals(source)) {
			// clear the azure backup config if switching to none/internal
			switch (bean.getStartup().getBackupType()) {
				case azure:
					if (bean.getStartup().getBackupDirectoryName() == null) {
						bean.getStartup().setBackupDirectoryName(UtilImpl.ARCHIVE_NAME);
					}
					break;
				case none:
					bean.getStartup().setBackupConnectionString(null);
					bean.getStartup().setBackupDirectoryName(null);
					break;
				default:
					break;
			}
		} else if (Configuration.twoFactorTypePropertyName.equals(source) && bean.getTwoFactorType() == TwoFactorType.email) {
			if (bean.getTwoFactorEmailBody() == null) {
				StringBuilder sb = new StringBuilder(128);
				sb.append("Hi, <br />");
				sb.append("Your verification code is: {tfaCode}<br />");
				sb.append("Enter the code above where prompted<br />.");
				sb.append("<br />");
				sb.append("Having issues with your 2FA? Reach out to your system administrator.");

				bean.setTwoFactorEmailBody(sb.toString());
			}
			if (bean.getTwoFactorEmailSubject() == null) {
				bean.setTwoFactorEmailSubject("Email verification security code");
			}
			if (bean.getTwofactorPushCodeTimeOutSeconds() == null) {
				// default to 5 minutes
				bean.setTwofactorPushCodeTimeOutSeconds(Integer.valueOf(5 * 60));
			}
		} else if (Binder.createCompoundBinding(Configuration.startupPropertyName, Startup.captchaTypePropertyName)
				.equals(source)) {
			// Use key values found in the json file so that the user can easily switch between different captcha types
			StartupExtension startup = bean.getStartup();
			Map<String, Object> properties = new HashMap<>(UtilImpl.OVERRIDE_CONFIGURATION);
			startup.clearApi(properties);
			Map<String, Object> configuration = UtilImpl.CONFIGURATION;
			@SuppressWarnings("unchecked")
			Map<String, Object> api = (Map<String, Object>) configuration.get("api");
			if(api != null) {
				startup.setApiGoogleRecaptchaSiteKey((String) api.get("googleRecaptchaSiteKey"));
				startup.setApiGoogleRecaptchaSecretKey((String) api.get("googleRecaptchaSecretKey"));
				startup.setApiCloudflareTurnstileSiteKey((String) api.get("cloudflareTurnstileSiteKey"));
				startup.setApiCloudflareTurnstileSecretKey((String) api.get("cloudflareTurnstileSecretKey"));
			}
		}

		super.preRerender(source, bean, webContext);
	}
	
	@Override
	public void postSave(ConfigurationExtension bean) throws Exception {
		TwoFactorType type = bean.getTwoFactorType();
		Integer timeout = bean.getTwofactorPushCodeTimeOutSeconds();
		String subject = bean.getTwoFactorEmailSubject();
		String body = bean.getTwoFactorEmailBody();
		if ((type != null) && (timeout != null) && (subject != null) && (body != null)) {
			// if the bean is new (just new)
			if (bean.getBizVersion().equals(Integer.valueOf(0))) {
				TwoFactorAuthCustomerConfiguration tfaConfig = new TwoFactorAuthCustomerConfiguration(type.toCode(), timeout.intValue(), subject, body);
				TwoFactorAuthConfigurationSingleton.getInstance().add(tfaConfig);
			}
			else {
				Map<String, Object> originalValues = bean.originalValues();
				if (originalValues.containsKey(Configuration.twoFactorTypePropertyName) || 
						originalValues.containsKey(Configuration.twofactorPushCodeTimeOutSecondsPropertyName) || 
						originalValues.containsKey(Configuration.twoFactorEmailSubjectPropertyName) ||
						originalValues.containsKey(Configuration.twoFactorEmailBodyPropertyName) ) {
					TwoFactorAuthCustomerConfiguration tfaConfig = new TwoFactorAuthCustomerConfiguration(type.toCode(), timeout.intValue(), subject, body);
					TwoFactorAuthConfigurationSingleton.getInstance().add(tfaConfig);
				}
			}
		}
	}

	@Override
	public void validate(ConfigurationExtension bean, ValidationException e) throws Exception {
		String expression = bean.getPasswordResetEmailSubject();
		if (expression != null) {
			String error = Binder.validateMessage(expression, User.MODULE_NAME, User.DOCUMENT_NAME);
			if (error != null) {
				e.getMessages().add(new Message(Configuration.passwordResetEmailSubjectPropertyName, error));
			}
		}

		expression = bean.getPasswordResetEmailBody();
		if (expression != null) {
			String error = Binder.validateMessage(expression.replace(RESET_PASSWORD_URL_EXPRESSION, ""), User.MODULE_NAME,
					User.DOCUMENT_NAME);
			if (error != null) {
				e.getMessages().add(new Message(Configuration.passwordResetEmailBodyPropertyName, error));
			}
		}

		expression = bean.getTwoFactorEmailBody();
		if (expression != null) {
			if (BindUtil.containsSkyveExpressions(expression.replace(TFA_CODE_EXPRESSION, ""))) {
				e.getMessages()
						.add(new Message(Configuration.twoFactorEmailBodyPropertyName,
								"The only expression allowed here is " + TFA_CODE_EXPRESSION));
			}
		}
	}
}

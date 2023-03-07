package modules.admin.Configuration;

import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.Startup.StartupBizlet;
import modules.admin.Startup.StartupExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.TwoFactorType;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;

public class ConfigurationBizlet extends SingletonCachedBizlet<ConfigurationExtension> {
	@Override
	public ConfigurationExtension newInstance(ConfigurationExtension bean) throws Exception {
		// temporarily elevate access to find existing configuration regardless of user
		final Persistence p = CORE.getPersistence();
		ConfigurationExtension result = null;
		try {
			p.setDocumentPermissionScopes(DocumentPermissionScope.customer);
			result = super.newInstance(bean);
		}
		finally {
			p.resetDocumentPermissionScopes();
		}

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

		} else if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
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
		}
		
		if (Configuration.twoFactorTypePropertyName.equals(source) && bean.getTwoFactorType() == TwoFactorType.email) {
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
}

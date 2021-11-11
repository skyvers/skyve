package modules.admin.Configuration;

import org.skyve.CORE;
import org.skyve.domain.Bean;
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
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;

public class ConfigurationBizlet extends SingletonCachedBizlet<ConfigurationExtension> {

	private static final long serialVersionUID = -1282437688681930236L;

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

		// Set the user name and email to the logged in user (if logged in)
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
					"<a href=\"{url}/pages/resetPassword.jsp?t={%s}\">" +
					"Reset Password</a></body></html>",
					Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
					User.passwordResetTokenPropertyName);
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

		super.preRerender(source, bean, webContext);
	}

}

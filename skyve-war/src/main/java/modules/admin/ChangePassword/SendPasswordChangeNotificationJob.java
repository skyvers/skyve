package modules.admin.ChangePassword;

import java.util.List;

import org.skyve.EXT;
import org.skyve.impl.bind.BindUtil;
import org.skyve.job.Job;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;

import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;

/**
 * This job sends a notification email to the parsed user, confirming that they just changed their password.
 * <br/>
 * The contents of this email include the environment support email address that users can contact if they suspect malicious
 * activity.
 * <br/>
 * If no support email address is specified for this application, the notification is not sent.
 * 
 * @author Simeon Solomou
 */
public class SendPasswordChangeNotificationJob extends Job {

	private static final String EMAIL_DESCRIPTION_GEOIP_ENABLED = "SYSTEM Password Change Notification (GeoIP enabled)";
	private static final String EMAIL_SUBJECT_GEOIP_ENABLED = "Password change detected";
	private static final String EMAIL_BODY_GEOIP_ENABLED = "Hello {"
			+ BindUtil.createCompoundBinding(UserProxy.contactPropertyName, Contact.namePropertyName)
			+ "},<br/><br/>"
			+ "You recently changed your password. To help keep you safe, here are some details:<br/>"
			+ "Date/Time: {" + User.passwordLastChangedPropertyName + "}<br/>"
			+ "IP Address: {" + User.passwordLastChangedIPPropertyName + "}<br/>"
			+ "Country: {" + User.passwordLastChangedCountryCodePropertyName + "} - {" + User.passwordLastChangedCountryNamePropertyName + "}<br/>"
			+ "<br/>"
			+ "If this was you, then you can safely ignore this email.<br/>"
			+ "If you are not sure if this was you, please contact us <a href=\"mailto: {"
			+ Startup.environmentSupportEmailPropertyName
			+ "}\">here</a>.";

	private static final String EMAIL_DESCRIPTION = "SYSTEM Password Change Notification";
	private static final String EMAIL_SUBJECT = "Password change detected";
	private static final String EMAIL_BODY = "Hello {"
			+ BindUtil.createCompoundBinding(UserProxy.contactPropertyName, Contact.namePropertyName)
			+ "},<br/><br/>"
			+ "You recently changed your password. To help keep you safe, here are some details:<br/>"
			+ "Date/Time: {" + User.passwordLastChangedPropertyName + "}<br/>"
			+ "IP Address: {" + User.passwordLastChangedIPPropertyName + "}<br/>"
			+ "<br/>"
			+ "If this was you, then you can safely ignore this email.<br/>"
			+ "If you are not sure if this was you, please contact us <a href=\"mailto: {"
			+ Startup.environmentSupportEmailPropertyName
			+ "}\">here</a>.";

	/**
	 * Performs the persistJobExecutionOnSuccess operation.
	 * @return the operation result
	 */
	@Override
	public boolean persistJobExecutionOnSuccess() {
		return false;
	}
	
	/**
	 * Performs the execute operation.
	 * @throws Exception if the operation fails
	 */
	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		setPercentComplete(0);
		
		// Check email config
		if (!newConfiguration().isEmailConfigured()) {
			String warningMessage = "Email is not configured. Failed to send password change notification.";
			log.add(warningMessage);
			LOGGER.warn(warningMessage);
			return;
		}

		// Get user
		User user = (User) getBean();
		if (user == null) {
			String warningMessage = "No user has been parsed. Failed to send password change notification.";
			log.add(warningMessage);
			LOGGER.warn(warningMessage);
			return;
		}
		Contact contact = user.getContact();

		// Get startup configuration
		Startup startup = newStartup();
		if (startup.getEnvironmentSupportEmail() == null) {
			String warningMessage = "There is no environment support email specified. Failed to send password change notification.";
			log.add(warningMessage);
			LOGGER.warn(warningMessage);
			return;
		}

		setPercentComplete(25);

		// Send
		try {
			// If GeoIP is configured...
			if (isGeoIPBlocking()) {
				// Send GeoIP template
				sendGeoIPNotification(user, startup);
			} else {
				// Send default template
				sendNotification(user, startup);
			}
			String successMessage = "Successfully sent password change notification to " + contact.getName();
			log.add(successMessage);
			LOGGER.info(successMessage);
		} catch (Exception e) {
			String failureMessage = "Failed to send password change notification to " + contact.getName();
			log.add(failureMessage);
			LOGGER.error(failureMessage, e);
		}

		setPercentComplete(100);
	}

	@SuppressWarnings("static-method") // test seam
	protected Configuration newConfiguration() {
		return Configuration.newInstance();
	}

	@SuppressWarnings("static-method") // test seam
	protected Startup newStartup() {
		return Startup.newInstance();
	}

	@SuppressWarnings("static-method") // test seam
	protected boolean isGeoIPBlocking() {
		return EXT.getGeoIPService().isBlocking();
	}

	@SuppressWarnings("static-method") // test seam
	protected void sendGeoIPNotification(User user, Startup startup) throws Exception {
		CommunicationUtil.sendFailSafeSystemCommunication(EMAIL_DESCRIPTION_GEOIP_ENABLED,
				"{contact.email1}",
				null,
				EMAIL_SUBJECT_GEOIP_ENABLED,
				EMAIL_BODY_GEOIP_ENABLED,
				ResponseMode.EXPLICIT,
				null,
				user,
				startup);
	}

	@SuppressWarnings("static-method") // test seam
	protected void sendNotification(User user, Startup startup) throws Exception {
		CommunicationUtil.sendFailSafeSystemCommunication(EMAIL_DESCRIPTION,
				"{contact.email1}",
				null,
				EMAIL_SUBJECT,
				EMAIL_BODY,
				ResponseMode.EXPLICIT,
				null,
				user,
				startup);
	}
}

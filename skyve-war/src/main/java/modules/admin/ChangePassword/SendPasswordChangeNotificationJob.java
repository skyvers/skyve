package modules.admin.ChangePassword;

import java.util.List;

import org.skyve.EXT;
import org.skyve.impl.bind.BindUtil;
import org.skyve.job.Job;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.util.Util;

import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.User;

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
			+ BindUtil.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName)
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
			+ BindUtil.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName)
			+ "},<br/><br/>"
			+ "You recently changed your password. To help keep you safe, here are some details:<br/>"
			+ "Date/Time: {" + User.passwordLastChangedPropertyName + "}<br/>"
			+ "IP Address: {" + User.passwordLastChangedIPPropertyName + "}<br/>"
			+ "<br/>"
			+ "If this was you, then you can safely ignore this email.<br/>"
			+ "If you are not sure if this was you, please contact us <a href=\"mailto: {"
			+ Startup.environmentSupportEmailPropertyName
			+ "}\">here</a>.";

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		setPercentComplete(0);
		
		// Check email config
		if (!Configuration.newInstance().isEmailConfigured()) {
			String warningMessage = "Email is not configured. Failed to send password change notification.";
			log.add(warningMessage);
			Util.LOGGER.warning(warningMessage);
			return;
		}

		// Get user
		User user = (User) getBean();
		if (user == null) {
			String warningMessage = "No user has been parsed. Failed to send password change notification.";
			log.add(warningMessage);
			Util.LOGGER.warning(warningMessage);
			return;
		}
		Contact contact = user.getContact();

		// Get startup configuration
		Startup startup = Startup.newInstance();
		if (startup.getEnvironmentSupportEmail() == null) {
			String warningMessage = "There is no environment support email specified. Failed to send password change notification.";
			log.add(warningMessage);
			Util.LOGGER.warning(warningMessage);
			return;
		}

		setPercentComplete(25);

		// Send
		try {
			// If GeoIP is configured...
			if (EXT.getGeoIPService().isBlocking()) {
				// Send GeoIP template
				CommunicationUtil.sendFailSafeSystemCommunication(EMAIL_DESCRIPTION_GEOIP_ENABLED,
						contact.getEmail1(),
						null,
						EMAIL_SUBJECT_GEOIP_ENABLED,
						EMAIL_BODY_GEOIP_ENABLED,
						ResponseMode.EXPLICIT,
						null,
						user,
						startup);
			} else {
				// Send default template
				CommunicationUtil.sendFailSafeSystemCommunication(EMAIL_DESCRIPTION,
						contact.getEmail1(),
						null,
						EMAIL_SUBJECT,
						EMAIL_BODY,
						ResponseMode.EXPLICIT,
						null,
						user,
						startup);
			}
			String successMessage = "Successfully sent password change notification to " + contact.getName();
			log.add(successMessage);
			Util.LOGGER.info(successMessage);
		} catch (Exception e) {
			String failureMessage = "Failed to send password change notification to " + contact.getName();
			log.add(failureMessage);
			Util.LOGGER.severe(failureMessage);
			e.printStackTrace();
		}

		setPercentComplete(100);
	}
}

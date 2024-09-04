package modules.admin.UserLoginRecord.jobs;

import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.Util;

import modules.admin.ModulesUtil;
import modules.admin.domain.Contact;
import modules.admin.domain.Startup;
import modules.admin.domain.UserLoginRecord;

/**
 * This job is used to send an email to a user notifying them that their account was accessed from a different country. It is called
 * when the system notices a change in the user's country from their last log in.
 * The job uses a {@code UserLoginRecord} bean parameter that's passed when the scheduler calls the job.
 */
public class DifferentCountryLoginNotificationJob extends Job {

	private static final String SUCCESS_MESSAGE = "Successfully sent email warning of unusual login activity to %s";

	public static final String JOB_NAME = "jDifferentCountryLoginNotification";

	private static final String UNUSUAL_LOGIN_ACTIVITY_EMAIL_DEFAULT_SUBJECT = "Security Alert: Unusual Login Activity Detected";

	private static final String UNUSUAL_LOGIN_EMAIL_BODY = "Dear {"
			+ Contact.namePropertyName
			+ "},<br/><br/>"
			+ "We have detected a login attempt to your account from a new location: {"
			+ UserLoginRecord.countryPropertyName
			+ "} with IP address: {"
			+ UserLoginRecord.ipAddressPropertyName
			+ "}. If this was you, there's no need to take further action.<br/><br/>"
			+ "However, if you do not recognize this activity, we strongly recommend that you change your password immediately to secure your account."
			+ "You can do this by logging into your account and navigating to <strong>Admin -> Password</strong>.<br/><br/>"
			+ "For your safety, please do not share your password with anyone.<br/><br/>"
			+ "If you have any questions or need assistance, please contact our support team at {"
			+ Startup.environmentSupportEmailPropertyName
			+ "}<br/><br/>"
			+ "Best regards,<br/>"
			+ "The Security Team";

	private static final String UNUSUAL_LOGIN_EMAIL_FAILURE_LOG_MESSAGE = "Failed to send security alert email to %s at %s regarding a login from a different country. Exception: %s";

	public static final String COMMUNICATION_DESCRIPTION = "SYSTEM Different Country Login Notification (GeoIP enabled)";

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		setPercentComplete(0);

		UserLoginRecord loginRecord = (UserLoginRecord) this.getBean();
		String country = loginRecord.getCountry();

		if (country != null) {
			Contact contact = ModulesUtil.getCurrentUserContact();
			String userEmail = contact.getEmail1();
			String userName = contact.getName();

			try {
				// Get startup configuration
				Startup startup = Startup.newInstance();
				if (startup.getEnvironmentSupportEmail() == null) {
					String warningMessage = "There is no environment support email specified. Failed to send different country login notification.";
					log.add(warningMessage);
					Util.LOGGER.warning(warningMessage);
					throw new Exception(warningMessage);
				}
				CommunicationUtil.sendFailSafeSystemCommunication(COMMUNICATION_DESCRIPTION,
						UNUSUAL_LOGIN_ACTIVITY_EMAIL_DEFAULT_SUBJECT,
						UNUSUAL_LOGIN_EMAIL_BODY,
						CommunicationUtil.ResponseMode.EXPLICIT, null, contact, loginRecord, startup);

				log.add(String.format(SUCCESS_MESSAGE, userEmail));
				UtilImpl.LOGGER.info(String.format(SUCCESS_MESSAGE, userEmail));
			} catch (Exception e) {

				log.add(String.format(UNUSUAL_LOGIN_EMAIL_FAILURE_LOG_MESSAGE, userName, userEmail, e.getMessage()));
				UtilImpl.LOGGER.severe(String.format(UNUSUAL_LOGIN_EMAIL_FAILURE_LOG_MESSAGE, userName, userEmail, e.getMessage()));
				e.printStackTrace();
			}
		}
		setPercentComplete(100);

	}

}

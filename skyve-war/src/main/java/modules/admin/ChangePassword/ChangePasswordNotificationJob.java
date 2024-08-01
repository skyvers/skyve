package modules.admin.ChangePassword;

import java.util.List;

import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.util.Mail;
import org.skyve.util.Util;

import modules.admin.domain.Contact;
import modules.admin.domain.User;

/**
 * This job sends a notification email to the current user, confirming that they just changed their password.
 * 
 * @author Simeon Solomou
 */
public class ChangePasswordNotificationJob extends Job {

	private static final String EMAIL_SUBJECT = "Security alert: change of password";
	private static final String EMAIL_BODY = "Hello %s,"
			+ "<br/><br/>"
			+ "This is a courtesy email to let you know that your password has been changed as requested."
			+ "<br/><br/>"
			+ "If you have any concerns regarding this, please contact us <a href=\"mailto: info@bizhub.com.au\">here</a>.";

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		setPercentComplete(0);
		
		User user = (User) getBean();
		Contact contact = user.getContact();
		String email = contact.getEmail1();
		if (email == null || email.isBlank()) {
			String exceptionMessage = "User " + user.getUserName()
					+ " does not have an email specified, and can therefore not receive a change password notification";
			log.add(exceptionMessage);
			Util.LOGGER.warning(exceptionMessage);
			return;
		}
		setPercentComplete(25);
		
		try {
			EXT.sendMail(new Mail()
					.addTo(email)
					.from(UtilImpl.SMTP_SENDER)
					.subject(EMAIL_SUBJECT)
					.body(String.format(EMAIL_BODY, contact.getName())));
			log.add("Successfully emailed change password notification email to " + email);
			setPercentComplete(100);
		} catch (Exception e) {
			e.printStackTrace();
			String exceptionMessage = "Failed to email change password notification email to " + email;
			log.add(exceptionMessage);
			Util.LOGGER.warning(exceptionMessage);
			throw new RuntimeException(exceptionMessage);
		}
	}
}
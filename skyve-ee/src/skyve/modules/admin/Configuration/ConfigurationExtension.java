package modules.admin.Configuration;

import static org.apache.commons.lang3.BooleanUtils.isTrue;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.Communication.CommunicationUtil.ResponseMode;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.JobSchedule;
import modules.admin.domain.SelfRegistration;
import modules.admin.domain.User;

public class ConfigurationExtension extends Configuration {

	private static final long serialVersionUID = -5669557826609528645L;
	private static final String BACKUP_JOB_NAME = ".jBackup";

	/**
	 * The minimum length for new usernames
	 */
	public static final int MINIMUM_USERNAME_LENGTH = 4;
	/**
	 * The minimum length for new passwords when one is not defined
	 */
	public static final int PASSWORD_DEFAULT_MIN_LENGTH = 10;

	/* Customised, do not overwrite*/
	public static final String SELF_REGISTRATION_COMMUNICATION = "SYSTEM Self Registration";
	public static final String SELF_REGISTRATION_SUBJECT = "Activate your account";
	public static final String SELF_REGISTRATION_HEADING = "Welcome!";
	public static final String SELF_REGISTRATION_BODY = String.format("<p>Hi <b>{%1$s}</b>,</p><br/>"
			+ "<p>Thank you for registering.</p>"
			+ "<p>To complete your account setup, please click the activation link below.</p>"
			+ "<p><a href=\"{%2$s}\">{%2$s}</a></p>"
			+ "<p>If you have any questions about your new account, contact us at <a href=\"mailto:info@skyve.org\">info@skyve.org</a>.</p>",
			Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.contactPropertyName, Contact.namePropertyName),
			SelfRegistration.activateUrlPropertyName);

	
	public void sendUserRegistrationEmail(SelfRegistration selfRegistration) throws Exception {
		CommunicationUtil.sendFailSafeSystemCommunication(SELF_REGISTRATION_COMMUNICATION, 
				selfRegistration.getUser().getContact().getEmail1(), 
				null, 
				SELF_REGISTRATION_SUBJECT,
				SELF_REGISTRATION_BODY,
				ResponseMode.EXPLICIT,
				null,
				selfRegistration);
	}
	
	
	/**
	 * Generates a text description of the system password complexity to be shown to the
	 * user if their entered password does not comply.
	 */
	@Override
	public String getPasswordRuleDescription() {
		StringBuilder out = new StringBuilder();
		out.append("Passwords must be ")
				.append(getPasswordMinLength() != null ? String.valueOf(getPasswordMinLength())
						: String.valueOf(PASSWORD_DEFAULT_MIN_LENGTH))
				.append(" characters or more");

		int passwordComplexity = 0;

		if (isTrue(getPasswordRequireLowercase())) {
			passwordComplexity++;
		}
		if (isTrue(getPasswordRequireUppercase())) {
			passwordComplexity++;
		}
		if (isTrue(getPasswordRequireNumeric())) {
			passwordComplexity++;
		}
		if (isTrue(getPasswordRequireSpecial())) {
			passwordComplexity++;
		}

		if (passwordComplexity > 0) {
			out.append(" and contain ");

			switch (passwordComplexity) {
				case 1:
					out.append(String.format("at least one %s character", getPasswordArgs()));
					break;
				case 2:
					out.append(String.format("%s and %s characters", getPasswordArgs()));
					break;
				case 3:
					out.append(String.format("%s, %s and %s characters", getPasswordArgs()));
					break;
				default:
					out.append(String.format("%s, %s, %s and %s characters", getPasswordArgs()));
					break;
			}
		}

		out.append(".");

		return out.toString();
	}

	/**
	 * Returns true if the supplied cleartext password matches the complexity rules defined by
	 * the system.
	 * 
	 * @param cleartext The cleartext password to test
	 * @return True if the password meets the system complexity, false otherwise
	 */
	public boolean meetsComplexity(final String cleartext) {
		// check length
		if (StringUtils.isBlank(cleartext) || cleartext.length() < getPasswordMinLength().intValue()) {
			return false;
		}

		// if requires lowercase, check if it contains any
		if (Boolean.TRUE.equals(getPasswordRequireLowercase())) {
			if (!cleartext.matches("^(?=.*[a-z]).{1,}$")) {
				return false;
			}
		}
	
		// if requires uppercase, check if it contains any
		if (Boolean.TRUE.equals(getPasswordRequireUppercase())) {
			if (!cleartext.matches("^(?=.*[A-Z]).{1,}$")) {
				return false;
			}
		}

		// if requires numeric, check if it contains any
		if (Boolean.TRUE.equals(getPasswordRequireNumeric())) {
			if (!cleartext.matches("^(?=.*\\d).{1,}$")) {
				return false;
			}
		}

		// if requires special, check if it contains any
		if (Boolean.TRUE.equals(getPasswordRequireSpecial())) {
			if (!cleartext.matches("^(?=.*[^\\p{L}\\p{Nd}]).{1,}$")) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Check whether a valid SMTP host has been configured
	 * 
	 * @return
	 */
	public static boolean validSMTPHost() {
		return !"localhost".equals(UtilImpl.SMTP);
	}

	public static String defaultSMTPSender() {
		return UtilImpl.SMTP_SENDER;
	}

	public static boolean validBackupConfiguration() {
		boolean result = false;
		DataMaintenance dm = DataMaintenance.newInstance();
		if (dm != null
				&& (dm.getDailyBackupRetention() != null || dm.getWeeklyBackupRetention() != null || dm.getMonthlyBackupRetention() != null
						|| dm.getYearlyBackupRetention() != null)) {
			result = true;
		}
		return result;
	}

	public static boolean validBackupSchedule() {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(JobSchedule.MODULE_NAME, JobSchedule.DOCUMENT_NAME);
		q.getFilter().addNullOrEquals(JobSchedule.disabledPropertyName, Boolean.FALSE);
		q.getFilter().addEquals(JobSchedule.jobNamePropertyName, JobSchedule.MODULE_NAME + BACKUP_JOB_NAME);
		
		return  (q.beanResult() != null);
	}

	/**
	 * Self registration is validly configured if:
	 * - userSelfRegistrationGroup has been assigned, and
	 * - allowUserSelfRegistration is true.
	 * 
	 * @return
	 */
	public boolean validSelfRegistration() {
		return (getUserSelfRegistrationGroup() != null && Boolean.TRUE.equals(getAllowUserSelfRegistration()));
	}

	/**
	 * Anonymous public user (e.g. for public forms) is validly configured if:
	 * - publicUser has been assigned.
	 * 
	 * @return
	 */
	public boolean validAnonymousPublicUser() {
		return (getPublicUser() != null);
	}

	private Object[] getPasswordArgs() {
		List<String> args = new ArrayList<>();

		if (isTrue(getPasswordRequireLowercase())) {
			args.add("lowercase");
		}
		if (isTrue(getPasswordRequireUppercase())) {
			args.add("uppercase");
		}
		if (isTrue(getPasswordRequireNumeric())) {
			args.add("numeric");
		}
		if (isTrue(getPasswordRequireSpecial())) {
			args.add("special");
		}

		return args.toArray();
	}
}

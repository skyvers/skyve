package modules.admin.Configuration;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DocumentQuery;

import modules.admin.domain.Configuration;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.JobSchedule;

public class ConfigurationExtension extends Configuration {

	private static final String BACKUP_JOB_NAME = ".jBackup";
	/**
	 * 
	 */
	private static final long serialVersionUID = -5669557826609528645L;

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
}

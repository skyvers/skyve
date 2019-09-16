package modules.admin.Configuration;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DocumentQuery;

import modules.admin.domain.Configuration;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.JobSchedule;

public class ConfigurationExtension extends Configuration {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5669557826609528645L;
	
	private static Boolean backupsConfigured;
	
	private static Boolean backupsScheduled;

	/**
	 * Check whether a valid SMTP host has been configured
	 * 
	 * @return
	 */
	public static boolean validSMTPHost() {
		return !"localhost".equals(UtilImpl.SMTP);
	}
	
	public static String  defaultSMTPSender() {
		return UtilImpl.SMTP_SENDER;
	}
	
	public static boolean validBackupConfiguration() {
		if(backupsConfigured==null) {
			DataMaintenance dm = DataMaintenance.newInstance();
			if(dm!=null 
					&& (dm.getDailyBackupRetention()!=null || dm.getWeeklyBackupRetention()!=null || dm.getMonthlyBackupRetention()!=null || dm.getYearlyBackupRetention()!=null)) {
				backupsConfigured = Boolean.TRUE;
			}
		}
		return Boolean.TRUE.equals(backupsConfigured);
	}
	
	public static boolean validBackupSchedule() {
		if(backupsScheduled==null) {
			DocumentQuery q = CORE.getPersistence().newDocumentQuery(JobSchedule.MODULE_NAME, JobSchedule.DOCUMENT_NAME);
			q.getFilter().addNullOrEquals(JobSchedule.disabledPropertyName, Boolean.FALSE);
			q.getFilter().addEquals(JobSchedule.jobNamePropertyName, JobSchedule.MODULE_NAME + ".jBackup");
			if(q.beanResult()!=null) {
				backupsScheduled = Boolean.TRUE;
			}
		}
		return Boolean.TRUE.equals(backupsScheduled);
	}

}

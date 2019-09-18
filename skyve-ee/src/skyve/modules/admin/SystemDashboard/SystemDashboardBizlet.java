package modules.admin.SystemDashboard;

import java.util.Set;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.Generic;
import modules.admin.domain.SystemDashboard;

public class SystemDashboardBizlet extends Bizlet<SystemDashboard> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4784606165710946704L;

	@Override
	public SystemDashboard newInstance(SystemDashboard bean) throws Exception {

		// generate status information for display
		
		// email configuration
		Generic emailConfig = Generic.newInstance();
		emailConfig.setMemo1("Email configured");
		emailConfig.setBoolean1(new Boolean(ConfigurationExtension.validSMTPHost()));
		bean.getStatus().add(emailConfig);
		
		// backups configured
		Generic backupConfig = Generic.newInstance();
		backupConfig.setMemo1("Backups configured");
		backupConfig.setBoolean1(new Boolean(ConfigurationExtension.validBackupConfiguration()));
		bean.getStatus().add(backupConfig);
		
		// backup job scheduled
		Generic backupScheduled = Generic.newInstance();
		backupScheduled.setMemo1("Backups scheduled");
		backupScheduled.setBoolean1(new Boolean(ConfigurationExtension.validBackupSchedule()));
		bean.getStatus().add(backupScheduled);
		
		//TODO
		// failed backups
//		Set<String> backups = DataMaintenanceExtension.backups();
		
		// days since last backup
		
		
		// maybe unusual system activity		

		return super.newInstance(bean);
	}

	
}

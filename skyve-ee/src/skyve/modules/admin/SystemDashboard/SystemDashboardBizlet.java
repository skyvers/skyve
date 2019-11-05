package modules.admin.SystemDashboard;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Generic;
import modules.admin.domain.SystemDashboard;

public class SystemDashboardBizlet extends Bizlet<SystemDashboard> {

	private static final long serialVersionUID = -4784606165710946704L;

	@Override
	public SystemDashboard newInstance(SystemDashboard bean) throws Exception {

		// generate status information for display
		
		// email configuration
		Generic emailConfig = Generic.newInstance();
		emailConfig.setMemo1("Email configured");
		emailConfig.setText5001(formatBooleanHTML(ConfigurationExtension.validSMTPHost()));
		bean.getStatus().add(emailConfig);
		
		// backups configured
		Generic backupConfig = Generic.newInstance();
		backupConfig.setMemo1("Backups configured");
		backupConfig.setText5001(formatBooleanHTML(ConfigurationExtension.validBackupConfiguration()));
		bean.getStatus().add(backupConfig);
		
		// backup job scheduled
		Generic backupScheduled = Generic.newInstance();
		backupScheduled.setMemo1("Backups scheduled");
		backupScheduled.setText5001(formatBooleanHTML(ConfigurationExtension.validBackupSchedule()));
		bean.getStatus().add(backupScheduled);
		
		return super.newInstance(bean);
	}

	private static String formatBooleanHTML(final boolean value) {
		final String template = "<i style='color: %1$s;' class='fa %2$s'></i>";

		if (value == true) {
			return String.format(template, "#4bc0c0", "fa-check");
		}
		return String.format(template, "#ff6385", "fa-times");
	}
}

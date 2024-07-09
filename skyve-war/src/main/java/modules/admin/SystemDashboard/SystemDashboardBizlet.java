package modules.admin.SystemDashboard;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.Util;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Configuration.DiskSpaceSummary;
import modules.admin.domain.Configuration;
import modules.admin.domain.Generic;
import modules.admin.domain.SystemDashboard;

public class SystemDashboardBizlet extends Bizlet<SystemDashboard> {

	@Override
	public SystemDashboard newInstance(SystemDashboard bean) throws Exception {

		// generate status information for display
		String valTrue = Util.i18n("ui.true.valueIconStyleClass");
		String valFalse = Util.i18n("ui.false.valueIconStyleClass");
		String valDisabled = Util.i18n("ui.disabled.value");
		String valNo = Util.i18n("ui.no.value");
		String valUnavailable = Util.i18n("ui.unavailable.value");

		// session count
		Generic sessionCount = Generic.newInstance();
		sessionCount.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.sessionCount")));
		sessionCount.setText5001(formatStringValueHTML(Integer.toString(StateUtil.getSessionCount()), ""));
		bean.getStatus().add(sessionCount);

		// email configuration
		Generic emailConfig = Generic.newInstance();
		emailConfig.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.emailConfigured")));
		emailConfig.setText5001(formatBooleanHTML(ConfigurationExtension.validSMTPHost(), valTrue, valFalse,
				Util.i18n("admin.systemDashboard.status.itemLabel.emailConfigured.suggestion")));
		bean.getStatus().add(emailConfig);

		// backups configured
		Generic backupConfig = Generic.newInstance();
		backupConfig.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.backupsConfigured")));
		backupConfig.setText5001(formatBooleanHTML(ConfigurationExtension.validBackupConfiguration(), valTrue, valFalse,
				Util.i18n("admin.systemDashboard.status.itemLabel.backupsConfigured.suggestion")));
		bean.getStatus().add(backupConfig);

		final boolean jobScheduler = UtilImpl.JOB_SCHEDULER;

		// backup job scheduled
		Generic backupScheduled = Generic.newInstance();
		backupScheduled.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.backupsScheduled")));
		if (jobScheduler) {
			backupScheduled.setText5001(formatBooleanHTML(ConfigurationExtension.validBackupSchedule(), valTrue, valFalse,
					Util.i18n("admin.systemDashboard.status.itemLabel.backupsScheduled.suggestion")));
		} else {
			backupScheduled.setText5001(formatStringValueHTML(valUnavailable,
					Util.i18n("admin.systemDashboard.status.itemLabel.backupsScheduled.suggestion")));
		}
		bean.getStatus().add(backupScheduled);

		// job scheduler enabled
		Generic schedulerEnabled = Generic.newInstance();
		schedulerEnabled.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.jobSchedulerEnabled")));
		schedulerEnabled.setText5001(jobScheduler ? formatBooleanHTML(true, valTrue, valFalse, "")
				: formatStringValueHTML(valDisabled,
						Util.i18n("admin.systemDashboard.status.itemLabel.selfRegistrationConfigured.suggestion")));
		bean.getStatus().add(schedulerEnabled);
		
		// available disk space alarm enabled
		Generic availableDiskSpaceAlarmScheduled = Generic.newInstance();
		availableDiskSpaceAlarmScheduled.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.availableDiskSpaceAlarmScheduled")));
		if (jobScheduler) {
			availableDiskSpaceAlarmScheduled.setText5001(formatBooleanHTML(ConfigurationExtension.validAvailableDiskSpaceAlarmSchedule(), valTrue, valFalse,
					Util.i18n("admin.systemDashboard.status.itemLabel.availableDiskSpaceAlarm.suggestion")));
		} else {
			availableDiskSpaceAlarmScheduled.setText5001(formatStringValueHTML(valUnavailable,
					Util.i18n("admin.systemDashboard.status.itemLabel.availableDiskSpaceAlarm.suggestion")));
		}
		bean.getStatus().add(availableDiskSpaceAlarmScheduled);

		ConfigurationExtension config = Configuration.newInstance();
		// current available disk space 
		Generic availableDiskSpace = Generic.newInstance();
		DiskSpaceSummary diskSpaceSummary = new DiskSpaceSummary();
		availableDiskSpace.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.availableDiskSpaceMB")));
		availableDiskSpace.setText5001(formatStringValueHTML(Long.toString(diskSpaceSummary.getTotalAvailable()), ""));
		bean.getStatus().add(availableDiskSpace);
		
		// self registration activated
		Generic selfRegConfigured = Generic.newInstance();
		selfRegConfigured.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.selfRegistrationConfigured")));
		selfRegConfigured.setText5001((config.validSelfRegistration() ? formatBooleanHTML(true, valTrue, valFalse, "")
				: formatStringValueHTML(valNo,
						Util.i18n("admin.systemDashboard.status.itemLabel.selfRegistrationConfigured.suggestion"))));
		bean.getStatus().add(selfRegConfigured);

		// anonymous user configured
		Generic anonymousUserConfigured = Generic.newInstance();
		anonymousUserConfigured.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.anonymousPublicUserConfigured")));
		anonymousUserConfigured.setText5001((config.validAnonymousPublicUser() ? formatBooleanHTML(true, valTrue, valFalse, "")
				: formatStringValueHTML(valNo,
						Util.i18n("admin.systemDashboard.status.itemLabel.anonymousPublicUserConfigured.suggestion"))));
		bean.getStatus().add(anonymousUserConfigured);

		// Password self-reset configured
		Generic passwordSelfResetConfigured = Generic.newInstance();
		passwordSelfResetConfigured.setMemo1(formatLabelHTML(Util.i18n("admin.systemDashboard.status.itemLabel.passwordSelfResetConfigured")));
		passwordSelfResetConfigured.setText5001(formatBooleanHTML((UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null || UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY != null) && ConfigurationExtension.validSMTPHost(), valTrue, valFalse,
				Util.i18n("admin.systemDashboard.status.itemLabel.passwordSelfResetConfigured.suggestion")));
		bean.getStatus().add(passwordSelfResetConfigured);

		return super.newInstance(bean);
	}

	private static String formatBooleanHTML(final boolean value, final String valTrue, final String valFalse, final String suggestion) {
		final String template = "<i style='color: %1$s;' class='fa %2$s' title='%3$s' ></i>";

		if (value == true) {
			return String.format(template, "#4bc0c0", valTrue, "");
		}
		return String.format(template, "#ff6385", valFalse, suggestion);
	}

	private static String formatStringValueHTML(final String value, final String suggestion) {
		return String.format("<div><i title='%1$s'>%2$s</i></div>", suggestion, value);
	}

	private static String formatLabelHTML(final String label) {
		return String.format("<div style='text-align: left'>%1$s<div>", label);
	}
}

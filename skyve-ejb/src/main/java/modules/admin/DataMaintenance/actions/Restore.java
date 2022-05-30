package modules.admin.DataMaintenance.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.job.JobDescription;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final String J_RESTORE = "jRestore";

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(DataMaintenance.MODULE_NAME);
		JobMetaData job = m.getJob(J_RESTORE);
		
		boolean restoreAlreadyRunning = false;
		try {
			List<JobDescription> runningJobs = EXT.getJobScheduler().getCustomerRunningJobs();
			for (JobDescription jd : runningJobs) {
				if (job.getDisplayName().equals(jd.getName())) {
					restoreAlreadyRunning = true;
					break;
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			throw new ValidationException(new Message(Util.i18n("admin.dataMaintenance.actions.restore.cannotDetectRunningJobsException")));
		}

		if (restoreAlreadyRunning) {
			throw new ValidationException(new Message(Util.i18n("admin.dataMaintenance.actions.restore.jobAlreadyRunningException")));
		}
		
		if(bean.getContentRestoreOption()==null) {
			Document d = m.getDocument(c, DataMaintenance.DOCUMENT_NAME);
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.contentRestoreOptionPropertyName).getLocalisedDisplayName();
			String msg = Util.i18n("admin.dataMaintenance.actions.restore.selectContentRestoreOptionException", desc);
			throw new ValidationException(DataMaintenance.contentRestoreOptionPropertyName, msg);
		}
		if(bean.getRestoreIndexingOption()==null) {
			Document d = m.getDocument(c, DataMaintenance.DOCUMENT_NAME);
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.restoreIndexingOptionPropertyName).getLocalisedDisplayName();
			String msg = Util.i18n("admin.dataMaintenance.actions.restore.selectRestoreIndexingOptionException", desc); 
			throw new ValidationException(DataMaintenance.restoreIndexingOptionPropertyName, msg);
		}
		
		if (bean.getRestorePreProcess() == null) {
			Document d = m.getDocument(c, DataMaintenance.DOCUMENT_NAME);
			@SuppressWarnings("null")
			String desc = d.getAttribute(DataMaintenance.restorePreProcessPropertyName).getLocalisedDisplayName();
			String msg = Util.i18n("admin.dataMaintenance.actions.restore.selectPreProcessException", desc); 
			throw new ValidationException(new Message(DataMaintenance.restorePreProcessPropertyName, msg));
		}
		
		EXT.getJobScheduler().runOneShotJob(job, bean, u);
		webContext.growl(MessageSeverity.info, Util.i18n("admin.dataMaintenance.actions.restore.restoreJobCommenced"));

		return new ServerSideActionResult<>(bean);
	}
}

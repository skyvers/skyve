package modules.admin.DataMaintenance;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.job.Job;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;

import modules.admin.DataMaintenance.actions.TruncateAuditLog;
import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Audit;
import modules.admin.domain.DataMaintenance;

public class TruncateAuditLogJob extends Job {
	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {

		List<String> log = getLog();

		DataMaintenance dm = (DataMaintenance) getBean();
		log.add("Started Truncation Job at " + new Date());

		if (dm != null) {
			Persistence pers = CORE.getPersistence();
			DocumentQuery qAudits = TruncateAuditLog.getAuditQuery(pers, dm);

			List<Audit> auditsToDelete = qAudits.beanResults();

			int size = auditsToDelete.size();
			int processed = 0;

			Iterator<Audit> it = auditsToDelete.iterator();
			while (it.hasNext()) {
				Audit a = it.next();
				pers.delete(a);
				pers.commit(false);
				pers.evictCached(a);
				pers.begin();

				setPercentComplete((int) (((float) processed++) / ((float) size) * 100F));
			}

			// TODO and add an audit record that we have truncated the Log

			// send email notification for completion of Job
			log.add("Truncated " + processed + " audits, successfully.");
			try {
				CommunicationUtil.sendFailSafeSystemCommunication(JobsBizlet.SYSTEM_JOB_NOTIFICATION,
						JobsBizlet.SYSTEM_JOB_NOTIFICATION_DEFAULT_SUBJECT, "Truncate Audit Log Job", ResponseMode.SILENT, null, dm);
			} catch (@SuppressWarnings("unused") Exception e) {
				log.add("Email notification failed.");
			}
		}

		setPercentComplete(100);
		log.add("Finished Truncation Job at " + new Date());
	}
}

package modules.admin.DataMaintenance;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.job.Job;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.Communication.CommunicationUtil.ResponseMode;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenanceModuleDocument;
import modules.admin.domain.DataMaintenance.RefreshOption;

public class RefreshDocumentTuplesJob extends Job {
	private static final long serialVersionUID = 6282346785863992703L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {

		List<String> log = getLog();

		DataMaintenance dm = (DataMaintenance) getBean();
		log.add("Started Document Data Refresh Job for " + dm.getModDocName() + " at " + new Date());

		long size = 0;
		int processed = 0;

		// calculate size
		for (DataMaintenanceModuleDocument doc : dm.getRefreshDocuments()) {
			if (Boolean.TRUE.equals(doc.getInclude())) {
				// get relevant document to action
				Persistence pers = CORE.getPersistence();
				DocumentQuery q = pers.newDocumentQuery(doc.getModuleName(), doc.getDocumentName());
				q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
				size = size + q.scalarResult(Long.class).longValue();
			}
		}

		// iterate
		for (DataMaintenanceModuleDocument doc : dm.getRefreshDocuments()) {
			if (Boolean.TRUE.equals(doc.getInclude())) {
				StringBuilder sb = new StringBuilder();
				sb.append("Document Data Refresh requested ");
				sb.append(" for [").append(doc.getModuleName());
				sb.append("].[").append(doc.getDocumentName()).append("]");

				// get relevant document to action
				Persistence pers = CORE.getPersistence();
				DocumentQuery q = pers.newDocumentQuery(doc.getModuleName(), doc.getDocumentName());
				List<Bean> beans = q.beanResults();

				Iterator<Bean> it = beans.iterator();
				while (it.hasNext()) {
					PersistentBean pb = (PersistentBean) it.next();

					try {
						if(RefreshOption.upsert.equals(dm.getRefreshOption())){
							pers.upsertBeanTuple(pb);
						} else if(RefreshOption.save.equals(dm.getRefreshOption())){
							pb = pers.save(pb);
						}
						pers.commit(false);
						pers.evictCached(pb);
						pers.begin();

					} catch (Exception e) {
						StringBuilder sbFail = new StringBuilder(sb);
						sbFail.append(" - Upsert failed for id ").append(pb.getBizId());
						log.add(sbFail.toString());
					}
					processed++;
					setPercentComplete((int) (((float) processed) / ((float) size) * 100F));
				}
				sb.append("Completed");
				log.add(sb.toString());
			}

		}

		if (Boolean.TRUE.equals(dm.getNotification())) {

			// send email notification for completion of Job
			CommunicationUtil.sendFailSafeSystemCommunication(DataMaintenanceBizlet.SYSTEM_DATA_REFRESH_NOTIFICATION,
					DataMaintenanceBizlet.SYSTEM_DATA_REFRESH_DEFAULT_SUBJECT, DataMaintenanceBizlet.SYSTEM_DATA_REFRESH_DEFAULT_BODY,
					ResponseMode.SILENT, null, dm);
		}

		setPercentComplete(100);
		log.add("Finished Document Data Refresh Job at " + new Date());
	}
}

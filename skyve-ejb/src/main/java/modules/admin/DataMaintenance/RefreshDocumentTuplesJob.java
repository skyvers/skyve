package modules.admin.DataMaintenance;

import java.util.Date;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.Job;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.util.PushMessage;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.EvictOption;
import modules.admin.domain.DataMaintenance.RefreshOption;
import modules.admin.domain.ModuleDocument;

public class RefreshDocumentTuplesJob extends Job {
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
		for (ModuleDocument doc : dm.getRefreshDocuments()) {
			if (Boolean.TRUE.equals(doc.getInclude())) {
				// get relevant document to action
				Persistence pers = CORE.getPersistence();
				DocumentQuery q = pers.newDocumentQuery(doc.getModuleName(), doc.getDocumentName());
				q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
				size = size + q.scalarResult(Long.class).longValue();
			}
		}

		RefreshOption refresh = dm.getRefreshOption();
		EvictOption evict = dm.getEvictOption();
		Boolean flagFailedData = dm.getFlagFailed();

		// iterate
		for (ModuleDocument doc : dm.getRefreshDocuments()) {
			if (Boolean.TRUE.equals(doc.getInclude())) {
				StringBuilder sb = new StringBuilder();
				sb.append("Document Data Refresh requested for [")
						.append(doc.getModuleName())
						.append("].[")
						.append(doc.getDocumentName()).append("]");

				// get relevant document to action
				Persistence pers = CORE.getPersistence();
				DocumentQuery q = pers.newDocumentQuery(doc.getModuleName(), doc.getDocumentName());
				for (PersistentBean bean : q.<PersistentBean>beanResults()) {
					try {
						if (EvictOption.bean.equals(evict) || EvictOption.all.equals(evict)) {
                            bean = pers.retrieve(doc.getModuleName(), doc.getDocumentName(), bean.getBizId());
                        }
						
						if (RefreshOption.upsert.equals(refresh)) {
							pers.upsertBeanTuple(bean);
						} else if (RefreshOption.save.equals(refresh)) {
							bean = pers.save(bean);
						}
						pers.commit(false);
						
						if (EvictOption.bean.equals(evict)) {
							pers.evictCached(bean);
						} else if (EvictOption.all.equals(evict)) {
							pers.evictAllCached();
						}
						pers.begin();
					}
					catch (@SuppressWarnings("unused") Exception e) {
						log.add(String.format("%s - %s failed for id: %s",
												sb.toString(),
												dm.getRefreshOption().toLocalisedDescription(),
												bean.getBizId()));
						if (Boolean.TRUE.equals(flagFailedData)) {
							bean.setBizFlagComment("Data refresh failed - Please validate data and try again.");
							CORE.getPersistence().upsertBeanTuple(bean);
						}
					}
					processed++;
					setPercentComplete((int) (((float) processed) / ((float) size) * 100F));
				}

				sb.append(" Completed");
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
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info, "Refresh Documents job completed."));
	}
}

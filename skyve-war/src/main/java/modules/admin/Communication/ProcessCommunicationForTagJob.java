package modules.admin.Communication;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.job.Job;
import org.skyve.persistence.Persistence;
import org.skyve.tag.TagManager;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;

public class ProcessCommunicationForTagJob extends Job {
	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();

		Communication communication = (Communication) getBean();
		Persistence pers = CORE.getPersistence();
		
		if (communication.getActionType() != null) {
			// get relevant document to action
			List<Bean> beans = TagBizlet.getTaggedItemsForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());
			StringBuilder sb = new StringBuilder();
			sb.append("Started Processing Communication for Tagged Items Job at ")
					.append(new Date())
					.append("\nSending communication ").append(communication.getDescription()).append(".")
					.append("\nUsing ").append(beans.size()).append(" tagged ").append(communication.getDocumentName())
					.append(" documents.\n");
			log.add(sb.toString());

			int size = beans.size();
			int processed = 0;
			Iterator<Bean> it = beans.iterator();
			TagManager tm = EXT.getTagManager();
			while (it.hasNext()) {
				PersistentBean pb = (PersistentBean) it.next();

				sb = new StringBuilder();
				sb.append("Processing communication for ").append(pb.getBizKey());

				try {
					switch (communication.getActionType()) {
					case saveForBulkSend:

						CommunicationUtil.generate(communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Saved OK");

						if (Boolean.TRUE.equals(communication.getUnTagSuccessful())) {
							tm.untag(communication.getTag().getBizId(), pb);
						}
						break;
					case testBindingsAndOutput:

						CommunicationUtil.send(communication, CommunicationUtil.RunMode.TEST, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Tested OK");
						break;
					case sendImmediately:

						CommunicationUtil.send(communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Sent OK");
						if (Boolean.TRUE.equals(communication.getUnTagSuccessful())) {
							tm.untag(communication.getTag().getBizId(), pb);
						}
						break;
					default:
						break;
					}

				} catch (Exception e) {
					sb.append(" - Unsuccessful");
					sb.append("\n");
					sb.append(e);
				}
				pers.commit(false);
				pers.evictCached(pb);
				pers.begin();

				setPercentComplete((int) (((float) processed) / ((float) size) * 100F));

				log.add(sb.toString());
			}
			setPercentComplete(100);
			log.add("Finished Processing Communication Action for Tagged Items Job at " + new Date());
			
			if (Boolean.TRUE.equals(communication.getNotification())) {
				
				// send email notification for completion of Job
				try {
					CommunicationUtil.sendFailSafeSystemCommunication(CommunicationBizlet.SYSTEM_COMMUNICATION_JOB_NOTIFICATION, CommunicationBizlet.SYSTEM_COMMUNICATION_JOB_DEFAULT_SUBJECT, CommunicationBizlet.SYSTEM_COMMUNICATION_JOB_DEFAULT_BODY, ResponseMode.SILENT, null, communication);
				} catch (@SuppressWarnings("unused") Exception e) {
					log.add("The job completed successfully, but the final notification could not be sent.");
				}
			}

		} else {
			throw new Exception("Communication job failed to commence because no valid action type was selected.");
		}
	}
}

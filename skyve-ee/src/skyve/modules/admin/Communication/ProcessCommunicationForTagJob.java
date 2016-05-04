package modules.admin.Communication;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.job.Job;

public class ProcessCommunicationForTagJob extends Job {
	private static final long serialVersionUID = 6282346785863992703L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {

		List<String> log = getLog();

		Communication communication = (Communication) getBean();

		if (communication.getActionType() != null) {

			// get relevant document to action
			List<Bean> beans = TagBizlet.getTaggedItemsForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());
			StringBuilder sb = new StringBuilder();
			sb.append("Started Processing Communication for Tagged Items Job at ");
			sb.append(new Date());
			sb.append(" expected ").append(beans.size()).append(" matching documents.");
			log.add(sb.toString());

			int size = beans.size();
			int processed = 0;
			Iterator<Bean> it = beans.iterator();
			while (it.hasNext()) {
				PersistentBean pb = (PersistentBean) it.next();

				sb = new StringBuilder();
				sb.append("Processing communication for ").append(pb.getBizKey());

				try {
					switch (communication.getActionType()) {
					case saveForBulkSend:

						CommunicationUtil.generate(communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Saved OK");
						break;
					case testBindingsAndOutput:

						CommunicationUtil.send(communication, CommunicationUtil.RunMode.TEST, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Tested OK");
						break;
					case sendImmediately:

						CommunicationUtil.send(communication, CommunicationUtil.RunMode.ACTION, CommunicationUtil.ResponseMode.EXPLICIT, null, pb);
						sb.append("\n Sent OK");
						break;
					default:
						break;
					}
				} catch (Exception e) {
					sb.append(" - Unsuccessful");
					sb.append("\n");
					sb.append(e);
				}
				setPercentComplete((int) (((float) processed) / ((float) size) * 100F));

				log.add(sb.toString());
			}
			setPercentComplete(100);
			log.add("Finished Processing Communication Action for Tagged Items Job at " + new Date());

		} else {
			throw new Exception("Communication job failed to commence because not valid action type was selected.");
		}
	}
}

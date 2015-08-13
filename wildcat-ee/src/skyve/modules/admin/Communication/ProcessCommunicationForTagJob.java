package modules.admin.Communication;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.job.WildcatJob;

public class ProcessCommunicationForTagJob extends WildcatJob {
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

						sb.append("\n Saving");
						CommunicationBizlet.generate(communication, pb);
						break;
					case testBindingsAndOutput:

						sb.append("\n Testing");
						break;
					case sendImmediately:

						sb.append("\n Sending");
						CommunicationBizlet.send(communication, pb);
						break;
					default:
						break;
					}
				} catch (Exception e) {
					sb.append(" - Unsuccessful");
					sb.append("\n").append(e.getMessage());
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

package modules.admin.Tag;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.Job;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.tag.TagManager;
import org.skyve.util.PushMessage;

public class DeleteAllTaggedDataForTagJob extends Job {
	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();

		TagExtension tag = (TagExtension) getBean();
		log.add("Started Delete All Tagged Data Job at " + new Date());

		// get relevant document to action
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		TagManager tm = EXT.getTagManager();
		for (Module module : customer.getModules()) {
			for (String documentRef : module.getDocumentRefs().keySet()) {

				List<Bean> beans = TagBizlet.getTaggedItemsForDocument(tag, module.getName(), documentRef);

				int size = beans.size();
				int processed = 0;
				Iterator<Bean> it = beans.iterator();
				while (it.hasNext()) {
					PersistentBean pb = (PersistentBean) it.next();
					StringBuilder sb = new StringBuilder();
					sb.append("Attempting to delete ");
					sb.append(module.getName()).append(".").append(documentRef);
					sb.append(" ").append(pb.getBizKey());
					try {
						// remove from tag and delete
						tm.untag(tag.getBizId(), pb);
						pers.delete(pb);
						pers.commit(false);
						pers.evictCached(pb);
						pers.begin();
					} catch (Exception e) {
						sb.append(" - Unsuccessful");
						sb.append("\n").append(e.getMessage());
					}
					processed++;
					setPercentComplete((int) (((float) processed) / ((float) size) * 100F));

					log.add(sb.toString());
				}
			}
		}

		setPercentComplete(100);
		log.add("Finished Delete All Tagged Data Job at " + new Date());
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info, "Delete All Tagged Data Job completed."));
	}
}

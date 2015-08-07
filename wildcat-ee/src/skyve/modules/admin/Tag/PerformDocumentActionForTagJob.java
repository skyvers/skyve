package modules.admin.Tag;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.job.WildcatJob;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.wildcat.metadata.customer.CustomerImpl;

public class PerformDocumentActionForTagJob extends WildcatJob {
	private static final long serialVersionUID = 6282346785863992703L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {

		List<String> log = getLog();

		Tag tag = (Tag) getBean();
		log.add("Started Document Action for Tagged Items Job at " + new Date());

		if (tag.getDocumentAction() != null) {

			// get relevant document to action
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(tag.getModuleName());
			Document document = module.getDocument(customer, tag.getDocumentName());

			// get action from actionname
			Repository rep = CORE.getRepository();
			String documentActionName = tag.getDocumentAction();
			ServerSideAction<Bean> act  = null;
			if(!TagBizlet.WILDCAT_SAVE_ACTION.equals(documentActionName) 
					&& !TagBizlet.WILDCAT_DELETE_ACTION.equals(documentActionName)
					&& !TagBizlet.WILDCAT_VALIDATE_ACTION.equals(documentActionName)){
				act = rep.getAction(customer, document, documentActionName);
			}

			List<Bean> beans = TagBizlet.getTaggedItemsForDocument(tag, tag.getModuleName(), tag.getDocumentName());
			
			int size=  beans.size();
			int processed = 0;
			Iterator<Bean> it = beans.iterator();
			while (it.hasNext()) {
				PersistentBean pb = (PersistentBean) it.next();
			
				StringBuilder sb=  new StringBuilder();
				sb.append("Action request for [").append(documentActionName);
				sb.append("] for document [").append(tag.getDocumentName()).append("] - ");
				if(tag.getDocumentCondition()!=null){
					sb.append(" with condition [").append(tag.getDocumentCondition()).append("] - ");
				} else {
					sb.append(" unconditionally ");
				}
				sb.append("'").append(pb.getBizKey()).append("'");
				try {
					boolean conditionSatisfied = true;
					if(tag.getDocumentCondition()!=null){
						conditionSatisfied = conditionSatisfied && pb.evaluateCondition(tag.getDocumentCondition());
					}
					if(conditionSatisfied){
						if(act!=null){
							CustomerImpl internalCustomer = (CustomerImpl) customer;
							boolean vetoed = internalCustomer.interceptBeforeServerSideAction(document, 
																								documentActionName,
																								pb,
																								null);
							if (! vetoed) {
								ServerSideActionResult result = act.execute(pb, null);	
								internalCustomer.interceptAfterServerSideAction(document, 
																					documentActionName,
																					result,
																					null);
								pb = (PersistentBean) result.getBean();
							}
						} 
						//remove successfully validated beans
						if(Boolean.TRUE.equals(tag.getUnTagSuccessful())){
							EXT.untag(tag.getBizId(), pb);
						}						

						if(TagBizlet.WILDCAT_DELETE_ACTION.equals(tag.getDocumentAction())){
							//remove from tag and delete
							EXT.untag(tag.getBizId(), pb);
							pers.delete(pb);
							pers.commit(false);
							pers.evictCached(pb);
							pers.begin();
						} else if(TagBizlet.WILDCAT_VALIDATE_ACTION.equals(tag.getDocumentAction())){
							BeanValidator.validateBeanAgainstDocument(document, pb);
							pers.evictCached(pb);
						} else {
							pers.save(pb);
							pers.commit(false);
							pers.evictCached(pb);
							pers.begin();
						}
						sb.append(" - Successful");
						
						
					} else {
						sb.append(" - Condition not satisfied");
					}
					
				} catch (Exception e){
					sb.append(" - Unsuccessful");
					sb.append("\n").append(e.getMessage());
				}
				setPercentComplete((int) (((float) processed) / ((float) size) * 100F));
				
				log.add(sb.toString());		
			}
		}
		
		setPercentComplete(100);
		log.add("Finished Document Action for Tagged Items Job at " + new Date());
	}
}

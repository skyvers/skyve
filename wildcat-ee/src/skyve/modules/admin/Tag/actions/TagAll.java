package modules.admin.Tag.actions;

import java.util.List;

import modules.admin.domain.Tag;
import modules.admin.domain.Tagged;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class TagAll implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult execute(Tag bean, WebContext webContext)
	throws Exception {
		
		Persistence pers = CORE.getPersistence();
		
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module modTag = customer.getModule(Tag.MODULE_NAME);
		Document docTagged = modTag.getDocument(customer, Tagged.DOCUMENT_NAME);
		
		DocumentQuery q = pers.newDocumentQuery(bean.getModuleName(), bean.getDocumentName());
		
		List<Bean> beans = q.projectedResults();
		for(Bean b: beans){
			//add bean to tagged
			Tagged tagged = docTagged.newInstance(user);
			tagged.setTag(bean);
			tagged.setTaggedModule(bean.getModuleName());
			tagged.setTaggedDocument(bean.getDocumentName());
			tagged.setTaggedBizId(b.getBizId());
			
			pers.upsertBeanTuple(tagged);
		}
		
		return new ServerSideActionResult(bean);
	}
}

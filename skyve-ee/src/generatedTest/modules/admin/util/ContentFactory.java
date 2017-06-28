package modules.admin.util;

import modules.admin.domain.Content;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class ContentFactory extends AbstractDomainFactory<Content> {

	@Override
	public Content getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Content.MODULE_NAME);
		Document document = module.getDocument(customer, Content.DOCUMENT_NAME);

		Content content = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return content;
	}
}
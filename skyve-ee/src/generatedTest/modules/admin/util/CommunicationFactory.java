package modules.admin.util;

import modules.admin.domain.Communication;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class CommunicationFactory extends AbstractDomainFactory<Communication> {

	@Override
	public Communication getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Document document = module.getDocument(customer, Communication.DOCUMENT_NAME);

		Communication communication = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return communication;
	}
}
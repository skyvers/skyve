package modules.admin.util;

import modules.admin.domain.DocumentNumber;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class DocumentNumberFactory extends AbstractDomainFactory<DocumentNumber> {

	@Override
	public DocumentNumber getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(DocumentNumber.MODULE_NAME);
		Document document = module.getDocument(customer, DocumentNumber.DOCUMENT_NAME);

		DocumentNumber documentNumber = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return documentNumber;
	}
}
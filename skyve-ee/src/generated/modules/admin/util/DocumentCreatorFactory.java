package modules.admin.util;

import modules.admin.domain.DocumentCreator;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/skyve/modules/admin/DocumentCreator/DocumentCreatorFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class DocumentCreatorFactory extends AbstractDomainFactory<DocumentCreator > {

	@Override
	public DocumentCreator getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(DocumentCreator.MODULE_NAME);
		Document document = module.getDocument(customer, DocumentCreator.DOCUMENT_NAME);

		DocumentCreator documentCreator = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return documentCreator;
	}
}
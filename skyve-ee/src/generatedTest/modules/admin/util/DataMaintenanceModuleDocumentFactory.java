package modules.admin.util;

import modules.admin.domain.DataMaintenanceModuleDocument;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class DataMaintenanceModuleDocumentFactory extends AbstractDomainFactory<DataMaintenanceModuleDocument> {

	@Override
	public DataMaintenanceModuleDocument getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(DataMaintenanceModuleDocument.MODULE_NAME);
		Document document = module.getDocument(customer, DataMaintenanceModuleDocument.DOCUMENT_NAME);

		DataMaintenanceModuleDocument dataMaintenanceModuleDocument = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return dataMaintenanceModuleDocument;
	}
}
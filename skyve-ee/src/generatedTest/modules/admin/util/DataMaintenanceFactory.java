package modules.admin.util;

import modules.admin.domain.DataMaintenance;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class DataMaintenanceFactory extends AbstractDomainFactory<DataMaintenance> {

	@Override
	public DataMaintenance getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(DataMaintenance.MODULE_NAME);
		Document document = module.getDocument(customer, DataMaintenance.DOCUMENT_NAME);

		DataMaintenance dataMaintenance = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return dataMaintenance;
	}
}
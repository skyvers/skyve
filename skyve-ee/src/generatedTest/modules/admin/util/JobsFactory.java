package modules.admin.util;

import modules.admin.domain.Jobs;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class JobsFactory extends AbstractDomainFactory<Jobs> {

	@Override
	public Jobs getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Jobs.MODULE_NAME);
		Document document = module.getDocument(customer, Jobs.DOCUMENT_NAME);

		Jobs jobs = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return jobs;
	}
}
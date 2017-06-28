package modules.admin.util;

import modules.admin.domain.Job;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class JobFactory extends AbstractDomainFactory<Job> {

	@Override
	public Job getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Job.MODULE_NAME);
		Document document = module.getDocument(customer, Job.DOCUMENT_NAME);

		Job job = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return job;
	}
}
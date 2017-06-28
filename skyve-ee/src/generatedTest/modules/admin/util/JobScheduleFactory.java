package modules.admin.util;

import modules.admin.domain.JobSchedule;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class JobScheduleFactory extends AbstractDomainFactory<JobSchedule> {

	@Override
	public JobSchedule getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(JobSchedule.MODULE_NAME);
		Document document = module.getDocument(customer, JobSchedule.DOCUMENT_NAME);

		JobSchedule jobSchedule = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);
		jobSchedule.setRunAs(new UserFactoryExtension().getInstance());

		return jobSchedule;
	}
}
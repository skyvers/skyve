package modules.admin.util;

import modules.admin.domain.Subscription;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class SubscriptionFactory extends AbstractDomainFactory<Subscription> {

	@Override
	public Subscription getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Subscription.MODULE_NAME);
		Document document = module.getDocument(customer, Subscription.DOCUMENT_NAME);

		Subscription subscription = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return subscription;
	}
}
package modules.admin.util;

import modules.admin.domain.Snapshot;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class SnapshotFactory extends AbstractDomainFactory<Snapshot> {

	@Override
	public Snapshot getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Snapshot.MODULE_NAME);
		Document document = module.getDocument(customer, Snapshot.DOCUMENT_NAME);

		Snapshot snapshot = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return snapshot;
	}
}
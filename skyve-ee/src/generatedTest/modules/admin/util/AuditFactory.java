package modules.admin.util;

import modules.admin.domain.Audit;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class AuditFactory extends AbstractDomainFactory<Audit> {

	@Override
	public Audit getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Audit.MODULE_NAME);
		Document document = module.getDocument(customer, Audit.DOCUMENT_NAME);

		Audit audit = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);
		audit.setSourceVersion(new AuditFactoryExtension().getInstance());

		return audit;
	}
}
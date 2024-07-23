package modules.admin.Audit;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Audit crudInstance() throws Exception {
		Audit audit = newAudit();

		Audit source = newAudit();
		audit.setSourceVersion(source);
		source.setSourceVersion(audit);
		
		return audit;
	}

	/**
	 * Completely override the super to avoid infinite recursion generating source version
	 */
	private static Audit newAudit() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Audit.MODULE_NAME);
		Document document = module.getDocument(customer, Audit.DOCUMENT_NAME);
	
		Audit audit = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 0);
		audit.setOperation(Operation.insert);
	
		return audit;
	}

}

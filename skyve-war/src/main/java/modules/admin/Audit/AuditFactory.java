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

/**
 * Builds audit fixtures for the admin module.
 *
 * <p>The factory creates a self-referential source/comparison pair so tests can
 * exercise the audit comparison UI without needing a live archive.
 */
public class AuditFactory {
	/**
	 * Creates a CRUD-capable audit fixture with linked source and comparison beans.
	 *
	 * <p>The returned audit uses an inserted source bean so the comparison workflow
	 * has a stable historical starting point.
	 *
	 * @return a populated audit fixture ready for CRUD tests; never {@code null}
	 * @throws Exception if the generated document metadata cannot be resolved
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static Audit crudInstance() throws Exception {
		Audit audit = newAudit();

		Audit source = newAudit();
		audit.setSourceVersion(source);
		source.setSourceVersion(audit);
		
		return audit;
	}

	/**
	 * Creates a random audit bean without recursively generating nested source versions.
	 *
	 * <p>This helper deliberately bypasses the generated source-version graph so the
	 * fixture remains finite and deterministic for tests.
	 *
	 * @return a new audit bean initialised as an insert operation; never {@code null}
	 * @throws Exception if metadata lookup or random-instance construction fails
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

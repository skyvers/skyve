package modules.admin.domain;

import modules.admin.Audit.AuditFactoryExtension;
import modules.admin.util.AuditFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class AuditTest extends AbstractDomainTest<Audit> {

	private AuditFactory factory;

	@Override
	protected Audit getBean() throws Exception {
		if (factory == null) {
			factory = new AuditFactoryExtension();
		}

		return factory.getInstance();
	}
}
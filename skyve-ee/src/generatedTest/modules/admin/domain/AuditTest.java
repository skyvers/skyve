package modules.admin.domain;

import modules.admin.util.AuditFactory;
import modules.admin.util.AuditFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class AuditTest extends AbstractDomainTest<Audit> {

	private AuditFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new AuditFactoryExtension();
	}

	@Override
	protected Audit getBean() throws Exception {
		return factory.getInstance();
	}
}
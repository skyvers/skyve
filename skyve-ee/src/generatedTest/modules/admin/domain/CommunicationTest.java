package modules.admin.domain;

import modules.admin.util.CommunicationFactory;
import modules.admin.util.CommunicationFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class CommunicationTest extends AbstractDomainTest<Communication> {

	private CommunicationFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new CommunicationFactoryExtension();
	}

	@Override
	protected Communication getBean() throws Exception {
		return factory.getInstance();
	}
}
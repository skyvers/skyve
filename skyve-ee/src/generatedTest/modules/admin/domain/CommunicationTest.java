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
	protected Communication getBean() throws Exception {
		if (factory == null) {
			factory = new CommunicationFactoryExtension();
		}

		return factory.getInstance();
	}
}
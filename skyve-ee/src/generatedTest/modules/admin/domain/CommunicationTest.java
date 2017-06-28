package modules.admin.domain;

import modules.admin.util.CommunicationFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class CommunicationTest extends AbstractDomainTest<Communication> {

	private CommunicationFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new CommunicationFactory();
	}

	@Override
	protected Communication getBean() throws Exception {
		return factory.getInstance();
	}
}
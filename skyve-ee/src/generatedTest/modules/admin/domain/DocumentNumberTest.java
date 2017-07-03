package modules.admin.domain;

import modules.admin.util.DocumentNumberFactory;
import modules.admin.util.DocumentNumberFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DocumentNumberTest extends AbstractDomainTest<DocumentNumber> {

	private DocumentNumberFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new DocumentNumberFactoryExtension();
	}

	@Override
	protected DocumentNumber getBean() throws Exception {
		return factory.getInstance();
	}
}
package modules.admin.domain;

import modules.admin.util.DataGroupFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class DataGroupTest extends AbstractDomainTest<DataGroup> {

	private DataGroupFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new DataGroupFactory();
	}

	@Override
	protected DataGroup getBean() throws Exception {
		return factory.getInstance();
	}
}
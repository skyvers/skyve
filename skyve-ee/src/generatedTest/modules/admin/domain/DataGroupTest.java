package modules.admin.domain;

import modules.admin.util.DataGroupFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DataGroupTest extends AbstractDomainTest<DataGroup> {

	private DataGroupFactory factory;

	@Override
	protected DataGroup getBean() throws Exception {
		if (factory == null) {
			factory = new DataGroupFactory();
		}

		return factory.getInstance();
	}
}
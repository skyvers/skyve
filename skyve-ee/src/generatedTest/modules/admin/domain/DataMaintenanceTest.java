package modules.admin.domain;

import modules.admin.util.DataMaintenanceFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class DataMaintenanceTest extends AbstractDomainTest<DataMaintenance> {

	private DataMaintenanceFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new DataMaintenanceFactory();
	}

	@Override
	protected DataMaintenance getBean() throws Exception {
		return factory.getInstance();
	}
}
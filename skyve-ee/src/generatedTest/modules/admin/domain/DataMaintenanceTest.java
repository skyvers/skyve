package modules.admin.domain;

import modules.admin.util.DataMaintenanceFactory;
import modules.admin.util.DataMaintenanceFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DataMaintenanceTest extends AbstractDomainTest<DataMaintenance> {

	private DataMaintenanceFactory factory;

	@Override
	protected DataMaintenance getBean() throws Exception {
		if (factory == null) {
			factory = new DataMaintenanceFactoryExtension();
		}

		return factory.getInstance();
	}
}
package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DataMaintenanceTest extends AbstractDomainTest<DataMaintenance> {

	@Override
	protected DataMaintenance getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
	}
}
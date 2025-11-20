package modules.admin.MonitoringDashboard.actions;

import modules.admin.domain.MonitoringDashboard;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class PurgeTest extends AbstractActionTest<MonitoringDashboard, Purge> {

	@Override
	protected Purge getAction() {
		return new Purge();
	}

	@Override
	protected MonitoringDashboard getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME);
	}
}
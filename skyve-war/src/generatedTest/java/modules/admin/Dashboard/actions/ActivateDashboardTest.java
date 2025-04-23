package modules.admin.Dashboard.actions;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.Dashboard;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class ActivateDashboardTest extends AbstractActionTest<DashboardExtension, ActivateDashboard> {

	@Override
	protected ActivateDashboard getAction() {
		return new ActivateDashboard();
	}

	@Override
	protected DashboardExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Dashboard.MODULE_NAME, Dashboard.DOCUMENT_NAME);
	}
}
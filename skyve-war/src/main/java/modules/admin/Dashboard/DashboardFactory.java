package modules.admin.Dashboard;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;

@SkyveFactory
public class DashboardFactory {
	@SkyveFixture(
			types = SkyveFixture.FixtureType.crud
	)
	public DashboardExtension crudInstance() {
		return new DataBuilder().fixture(SkyveFixture.FixtureType.crud).factoryBuild(DashboardExtension.MODULE_NAME, DashboardExtension.DOCUMENT_NAME);
	}
}

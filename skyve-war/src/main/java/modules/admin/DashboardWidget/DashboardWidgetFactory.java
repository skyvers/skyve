package modules.admin.DashboardWidget;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;

@SkyveFactory
public class DashboardWidgetFactory {
	@SkyveFixture(
			types = SkyveFixture.FixtureType.crud
	)
	public DashboardWidgetExtension crudInstance() {
		return new DataBuilder().fixture(SkyveFixture.FixtureType.crud).factoryBuild(DashboardWidgetExtension.MODULE_NAME, DashboardWidgetExtension.DOCUMENT_NAME);
	}
}

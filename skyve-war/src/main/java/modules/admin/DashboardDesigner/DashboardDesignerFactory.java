package modules.admin.DashboardDesigner;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;

@SkyveFactory
public class DashboardDesignerFactory {
	@SkyveFixture(
			types = SkyveFixture.FixtureType.crud
	)
	public DashboardDesignerExtension crudInstance() {
		return new DataBuilder().fixture(SkyveFixture.FixtureType.crud).factoryBuild(DashboardDesignerExtension.MODULE_NAME, DashboardDesignerExtension.DOCUMENT_NAME);
	}
}

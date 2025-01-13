package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DashboardTileTest extends AbstractDomainTest<DashboardTile> {

	@Override
	protected DashboardTile getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(DashboardTile.MODULE_NAME, DashboardTile.DOCUMENT_NAME);
	}
}
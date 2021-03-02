package modules.admin.ReportDataset.actions;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.domain.ReportDataset;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class TestQueryTest extends AbstractActionTest<ReportDatasetExtension, TestQuery> {

	@Override
	protected TestQuery getAction() {
		return new TestQuery();
	}

	@Override
	protected ReportDatasetExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ReportDataset.MODULE_NAME, ReportDataset.DOCUMENT_NAME);
	}
}
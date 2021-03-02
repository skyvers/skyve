package modules.admin.ReportTemplate.actions;

import modules.admin.domain.ReportTemplate;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class CopyReportTest extends AbstractActionTest<ReportTemplate, CopyReport> {

	@Override
	protected CopyReport getAction() {
		return new CopyReport();
	}

	@Override
	protected ReportTemplate getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
	}
}
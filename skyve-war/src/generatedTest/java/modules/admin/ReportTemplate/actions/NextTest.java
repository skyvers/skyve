package modules.admin.ReportTemplate.actions;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class NextTest extends AbstractActionTest<ReportTemplateExtension, Next> {

	@Override
	protected Next getAction() {
		return new Next();
	}

	@Override
	protected ReportTemplateExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
	}
}
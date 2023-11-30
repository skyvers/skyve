package modules.admin.ReportTemplate.actions;

import modules.admin.domain.ReportTemplate;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class AddUserToEmailTest extends AbstractActionTest<ReportTemplate, AddUserToEmail> {

	@Override
	protected AddUserToEmail getAction() {
		return new AddUserToEmail();
	}

	@Override
	protected ReportTemplate getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
	}
}
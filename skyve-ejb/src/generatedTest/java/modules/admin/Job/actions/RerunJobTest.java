package modules.admin.Job.actions;

import modules.admin.domain.Job;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class RerunJobTest extends AbstractActionTest<Job, RerunJob> {

	@Override
	protected RerunJob getAction() {
		return new RerunJob();
	}

	@Override
	protected Job getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Job.MODULE_NAME, Job.DOCUMENT_NAME);
	}
}
package modules.admin.Configuration.actions;

import modules.admin.domain.Configuration;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class InviteUsersTest extends AbstractActionTest<Configuration, InviteUsers> {

	@Override
	protected InviteUsers getAction() {
		return new InviteUsers();
	}

	@Override
	protected Configuration getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
	}
}
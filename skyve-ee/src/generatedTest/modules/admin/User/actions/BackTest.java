package modules.admin.User.actions;

import modules.admin.domain.User;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class BackTest extends AbstractActionTest<User, Back> {

	@Override
	protected Back getAction() {
		return new Back();
	}

	@Override
	protected User getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(User.MODULE_NAME, User.DOCUMENT_NAME);
	}
}
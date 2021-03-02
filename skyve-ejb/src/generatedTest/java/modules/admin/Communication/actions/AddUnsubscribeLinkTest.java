package modules.admin.Communication.actions;

import modules.admin.domain.Communication;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class AddUnsubscribeLinkTest extends AbstractActionTest<Communication, AddUnsubscribeLink> {

	@Override
	protected AddUnsubscribeLink getAction() {
		return new AddUnsubscribeLink();
	}

	@Override
	protected Communication getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
	}
}
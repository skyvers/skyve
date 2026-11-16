package modules.kitchensink.DialogsFixture.actions;

import modules.kitchensink.domain.DialogsFixture;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class AskOnceTest extends AbstractActionTest<DialogsFixture, AskOnce> {

	@Override
	protected AskOnce getAction() {
		return new AskOnce();
	}

	@Override
	protected DialogsFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME);
	}
}
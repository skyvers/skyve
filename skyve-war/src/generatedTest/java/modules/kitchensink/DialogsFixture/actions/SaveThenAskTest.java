package modules.kitchensink.DialogsFixture.actions;

import modules.kitchensink.domain.DialogsFixture;
import modules.kitchensink.domain.KitchensinkDocument;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SaveThenAskTest extends AbstractActionTest<DialogsFixture, SaveThenAsk> {

	@Override
	protected SaveThenAsk getAction() {
		return new SaveThenAsk();
	}

	@Override
	protected DialogsFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(KitchensinkDocument.DIALOGS_FIXTURE);
	}
}
package modules.kitchensink.ActionsFixture.actions;

import modules.kitchensink.domain.ActionsFixture;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class RunFromTabButtonTest extends AbstractActionTest<ActionsFixture, RunFromTabButton> {

	@Override
	protected RunFromTabButton getAction() {
		return new RunFromTabButton();
	}

	@Override
	protected ActionsFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME);
	}
}
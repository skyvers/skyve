package modules.kitchensink.ActionsFixture.actions;

import modules.kitchensink.domain.ActionsFixture;
import modules.kitchensink.domain.KitchensinkDocument;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class RunSharedTest extends AbstractActionTest<ActionsFixture, RunShared> {

	@Override
	protected RunShared getAction() {
		return new RunShared();
	}

	@Override
	protected ActionsFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(KitchensinkDocument.ACTIONS_FIXTURE);
	}
}
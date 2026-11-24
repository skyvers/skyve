package modules.kitchensink.ActionsFixtureRow.actions;

import modules.kitchensink.domain.ActionsFixtureRow;
import modules.kitchensink.domain.KitchensinkDocument;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class RunFromRowTest extends AbstractActionTest<ActionsFixtureRow, RunFromRow> {

	@Override
	protected RunFromRow getAction() {
		return new RunFromRow();
	}

	@Override
	protected ActionsFixtureRow getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(KitchensinkDocument.ACTIONS_FIXTURE_ROW);
	}
}
package modules.kitchensink.ActionsFixtureRow.actions;

import modules.kitchensink.domain.ActionsFixtureRow;
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
			.build(ActionsFixtureRow.MODULE_NAME, ActionsFixtureRow.DOCUMENT_NAME);
	}
}
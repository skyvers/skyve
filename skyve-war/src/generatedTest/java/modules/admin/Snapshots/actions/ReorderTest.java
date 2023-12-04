package modules.admin.Snapshots.actions;

import modules.admin.domain.Snapshots;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class ReorderTest extends AbstractActionTest<Snapshots, Reorder> {

	@Override
	protected Reorder getAction() {
		return new Reorder();
	}

	@Override
	protected Snapshots getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Snapshots.MODULE_NAME, Snapshots.DOCUMENT_NAME);
	}
}
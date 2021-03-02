package modules.admin.Snapshot.actions;

import modules.admin.domain.Snapshot;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class CopySnapshotToUserTest extends AbstractActionTest<Snapshot, CopySnapshotToUser> {

	@Override
	protected CopySnapshotToUser getAction() {
		return new CopySnapshotToUser();
	}

	@Override
	protected Snapshot getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
	}
}
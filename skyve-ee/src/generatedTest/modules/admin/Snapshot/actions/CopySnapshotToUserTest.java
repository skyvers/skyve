package modules.admin.Snapshot.actions;

import modules.admin.domain.Snapshot;
import modules.admin.util.SnapshotFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class CopySnapshotToUserTest extends AbstractActionTest<Snapshot, CopySnapshotToUser> {

	private SnapshotFactory factory;

	@Override
	protected CopySnapshotToUser getAction() {
		return new CopySnapshotToUser();
	}

	@Override
	protected Snapshot getBean() throws Exception {
		if (factory == null) {
			factory = new SnapshotFactory();
		}

		return factory.getInstance();
	}
}
package modules.admin.Snapshot.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.domain.Snapshot;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class CopySnapshotToUserH2Test extends AbstractH2Test {
	@Test
	void executeWithNoTargetUserReturnsSnapshotUnchanged() throws Exception {
		Snapshot snapshot = Snapshot.newInstance();
		snapshot.setName("Saved view");
		snapshot.setCopyToUser(null);

		ServerSideActionResult<Snapshot> result = new CopySnapshotToUser().execute(snapshot, null);

		assertThat(result.getBean(), is(snapshot));
		assertThat(snapshot.getName(), is("Saved view"));
	}
}

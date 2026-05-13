package modules.admin.HeapDumpList.actions;

import modules.admin.domain.HeapDumpList;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SelectedHeapDumpTest extends AbstractActionTest<HeapDumpList, SelectedHeapDump> {

	@Override
	protected SelectedHeapDump getAction() {
		return new SelectedHeapDump();
	}

	@Override
	protected HeapDumpList getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(HeapDumpList.MODULE_NAME, HeapDumpList.DOCUMENT_NAME);
	}
}
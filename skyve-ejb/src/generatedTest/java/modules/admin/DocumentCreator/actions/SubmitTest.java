package modules.admin.DocumentCreator.actions;

import modules.admin.domain.DocumentCreator;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SubmitTest extends AbstractActionTest<DocumentCreator, Submit> {

	@Override
	protected Submit getAction() {
		return new Submit();
	}

	@Override
	protected DocumentCreator getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(DocumentCreator.MODULE_NAME, DocumentCreator.DOCUMENT_NAME);
	}
}
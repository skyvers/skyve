package modules.admin.Audit.actions;

import modules.admin.domain.Audit;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SourceVersionChangedTest extends AbstractActionTest<Audit, SourceVersionChanged> {

	@Override
	protected SourceVersionChanged getAction() {
		return new SourceVersionChanged();
	}

	@Override
	protected Audit getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
	}
}
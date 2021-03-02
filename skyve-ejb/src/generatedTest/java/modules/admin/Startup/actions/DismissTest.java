package modules.admin.Startup.actions;

import modules.admin.Startup.StartupExtension;
import modules.admin.domain.Startup;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class DismissTest extends AbstractActionTest<StartupExtension, Dismiss> {

	@Override
	protected Dismiss getAction() {
		return new Dismiss();
	}

	@Override
	protected StartupExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
	}
}
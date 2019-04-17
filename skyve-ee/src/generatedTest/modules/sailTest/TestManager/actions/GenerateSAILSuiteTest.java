package modules.sailTest.TestManager.actions;

import modules.sailTest.domain.TestManager;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class GenerateSAILSuiteTest extends AbstractActionTest<TestManager, GenerateSAILSuite> {

	@Override
	protected GenerateSAILSuite getAction() {
		return new GenerateSAILSuite();
	}

	@Override
	protected TestManager getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(TestManager.MODULE_NAME, TestManager.DOCUMENT_NAME);
	}
}
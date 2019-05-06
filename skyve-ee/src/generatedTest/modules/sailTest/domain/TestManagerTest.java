package modules.sailTest.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class TestManagerTest extends AbstractDomainTest<TestManager> {

	@Override
	protected TestManager getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(TestManager.MODULE_NAME, TestManager.DOCUMENT_NAME);
	}
}
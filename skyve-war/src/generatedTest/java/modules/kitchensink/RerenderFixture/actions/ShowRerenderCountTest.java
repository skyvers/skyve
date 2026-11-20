package modules.kitchensink.RerenderFixture.actions;

import modules.kitchensink.domain.RerenderFixture;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class ShowRerenderCountTest extends AbstractActionTest<RerenderFixture, ShowRerenderCount> {

	@Override
	protected ShowRerenderCount getAction() {
		return new ShowRerenderCount();
	}

	@Override
	protected RerenderFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(RerenderFixture.MODULE_NAME, RerenderFixture.DOCUMENT_NAME);
	}
}
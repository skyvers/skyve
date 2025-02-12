package modules.kitchensink.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class HomeTest extends AbstractDomainTest<Home> {

	@Override
	protected Home getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Home.MODULE_NAME, Home.DOCUMENT_NAME);
	}
}
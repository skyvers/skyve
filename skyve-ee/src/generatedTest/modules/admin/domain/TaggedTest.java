package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class TaggedTest extends AbstractDomainTest<Tagged> {

	@Override
	protected Tagged getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
	}
}
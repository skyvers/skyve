package modules.kitchensink.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class LookupDescriptionTest extends AbstractDomainTest<LookupDescription> {

	@Override
	protected LookupDescription getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(LookupDescription.MODULE_NAME, LookupDescription.DOCUMENT_NAME);
	}
}
package modules.kitchensink.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class ModelLookupFixtureTest extends AbstractDomainTest<ModelLookupFixture> {

	@Override
	protected ModelLookupFixture getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME);
	}
}
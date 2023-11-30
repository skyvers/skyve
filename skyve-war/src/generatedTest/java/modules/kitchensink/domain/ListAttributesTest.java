package modules.kitchensink.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class ListAttributesTest extends AbstractDomainTest<ListAttributes> {

	@Override
	protected ListAttributes getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ListAttributes.MODULE_NAME, ListAttributes.DOCUMENT_NAME);
	}
}
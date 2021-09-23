package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class DynamicRelationTest extends AbstractDomainTest<DynamicRelation> {

	@Override
	protected DynamicRelation getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(DynamicRelation.MODULE_NAME, DynamicRelation.DOCUMENT_NAME);
	}
}
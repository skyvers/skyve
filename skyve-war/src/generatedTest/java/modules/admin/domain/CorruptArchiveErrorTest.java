package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class CorruptArchiveErrorTest extends AbstractDomainTest<CorruptArchiveError> {

	@Override
	protected CorruptArchiveError getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(CorruptArchiveError.MODULE_NAME, CorruptArchiveError.DOCUMENT_NAME);
	}
}
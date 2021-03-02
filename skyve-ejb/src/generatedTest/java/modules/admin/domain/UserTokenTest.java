package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class UserTokenTest extends AbstractDomainTest<UserToken> {

	@Override
	protected UserToken getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(UserToken.MODULE_NAME, UserToken.DOCUMENT_NAME);
	}
}
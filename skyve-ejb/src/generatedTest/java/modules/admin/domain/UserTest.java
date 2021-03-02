package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class UserTest extends AbstractDomainTest<User> {

	@Override
	protected User getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(User.MODULE_NAME, User.DOCUMENT_NAME);
	}
}
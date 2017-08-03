package modules.admin.domain;

import modules.admin.util.UserFactory;
import modules.admin.util.UserFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class UserTest extends AbstractDomainTest<User> {

	private UserFactory factory;

	@Override
	protected User getBean() throws Exception {
		if (factory == null) {
			factory = new UserFactoryExtension();
		}

		return factory.getInstance();
	}
}
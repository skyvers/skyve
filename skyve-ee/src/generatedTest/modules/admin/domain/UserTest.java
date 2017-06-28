package modules.admin.domain;

import modules.admin.util.UserFactory;
import modules.admin.util.UserFactoryExtension;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class UserTest extends AbstractDomainTest<User> {

	private UserFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new UserFactoryExtension();
	}

	@Override
	protected User getBean() throws Exception {
		return factory.getInstance();
	}
}
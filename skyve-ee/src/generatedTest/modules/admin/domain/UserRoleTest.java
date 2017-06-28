package modules.admin.domain;

import modules.admin.util.UserRoleFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class UserRoleTest extends AbstractDomainTest<UserRole> {

	private UserRoleFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new UserRoleFactory();
	}

	@Override
	protected UserRole getBean() throws Exception {
		return factory.getInstance();
	}
}
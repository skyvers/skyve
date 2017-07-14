package modules.admin.domain;

import modules.admin.util.UserLoginRecordFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class UserLoginRecordTest extends AbstractDomainTest<UserLoginRecord> {

	private UserLoginRecordFactory factory;

	@Override
	protected UserLoginRecord getBean() throws Exception {
		if (factory == null) {
			factory = new UserLoginRecordFactory();
		}

		return factory.getInstance();
	}
}
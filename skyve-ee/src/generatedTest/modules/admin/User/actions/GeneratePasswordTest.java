package modules.admin.User.actions;

import modules.admin.domain.User;
import modules.admin.util.UserFactory;
import modules.admin.util.UserFactoryExtension;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class GeneratePasswordTest extends AbstractActionTest<User, GeneratePassword> {

	private UserFactory factory;

	@Override
	protected GeneratePassword getAction() {
		return new GeneratePassword();
	}

	@Override
	protected User getBean() throws Exception {
		if (factory == null) {
			factory = new UserFactoryExtension();
		}

		return factory.getInstance();
	}
}
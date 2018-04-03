package modules.admin.User.actions;

import modules.admin.User.UserFactoryExtension;
import modules.admin.domain.User;
import modules.admin.util.UserFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class BackTest extends AbstractActionTest<User, Back> {

	private UserFactory factory;

	@Override
	protected Back getAction() {
		return new Back();
	}

	@Override
	protected User getBean() throws Exception {
		if (factory == null) {
			factory = new UserFactoryExtension();
		}

		return factory.getInstance();
	}
}
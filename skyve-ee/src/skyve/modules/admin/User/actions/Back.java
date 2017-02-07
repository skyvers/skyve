package modules.admin.User.actions;

import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Back implements ServerSideAction<User> {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = -4667349358677521637L;

	@Override
	public ServerSideActionResult<User> execute(User adminUser, WebContext webContext) throws Exception {

		if(WizardState.confirmGroupMemberships.equals(adminUser.getWizardState())){
			adminUser.setWizardState(WizardState.confirmUserNameAndPassword);
		} else {
			adminUser.setWizardState(WizardState.confirmContact);
		}
		
		return new ServerSideActionResult<>(adminUser);
	}
}

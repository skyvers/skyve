package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

public class Back implements ServerSideAction<User> {
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

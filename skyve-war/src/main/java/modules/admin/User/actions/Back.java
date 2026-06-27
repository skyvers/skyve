package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

/**
 * Moves the user wizard one step backwards.
 */
public class Back implements ServerSideAction<User> {
	/**
	 * Rewinds wizard state from groups to credentials, otherwise to contact confirmation.
	 *
	 * @param adminUser The user wizard bean.
	 * @param webContext The current web context.
	 * @return The same user bean.
	 * @throws Exception If state update fails.
	 */
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

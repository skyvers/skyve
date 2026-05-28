package modules.admin.UserCandidateContact.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.User.actions.GenerateUniqueUserName;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.UserCandidateContact;

/**
 * Applies a selected candidate contact to the parent user wizard.
 */
public class Select implements ServerSideAction<UserCandidateContact> {
	/**
	 * Copies the candidate contact to the parent user and advances wizard state.
	 *
	 * @param candidate The selected candidate contact row.
	 * @param webContext The current web context.
	 * @return The unchanged candidate row result.
	 * @throws Exception If state transition fails.
	 */
	@Override
	public ServerSideActionResult<UserCandidateContact> execute(UserCandidateContact candidate, WebContext webContext)
	throws Exception {
		User user = candidate.getParent();
		user.setContact(candidate.getContact());
		user.getCandidateContacts().clear();
		
		user.setWizardState(WizardState.confirmUserNameAndPassword);
		user.setUserName(GenerateUniqueUserName.generateUniqueUserNameFromContactName(user));

		return new ServerSideActionResult<>(candidate);
	}
}

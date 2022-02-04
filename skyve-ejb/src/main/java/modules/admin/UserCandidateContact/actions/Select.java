package modules.admin.UserCandidateContact.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.User.actions.GenerateUniqueUserName;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.UserCandidateContact;

public class Select implements ServerSideAction<UserCandidateContact> {
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

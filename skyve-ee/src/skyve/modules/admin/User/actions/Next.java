package modules.admin.User.actions;

import modules.admin.User.UserBizlet;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Next implements ServerSideAction<User> {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = -4667349358677521637L;

	@Override
	public ServerSideActionResult<User> execute(User adminUser, WebContext webContext) throws Exception {

		ValidationException e = new ValidationException();
		
		if(WizardState.confirmContact.equals(adminUser.getWizardState())){
			throw new ValidationException(new Message("You must either search for an existing contact or choose to create a new contact."));
		} else if(WizardState.createContact.equals(adminUser.getWizardState())){
			UserBizlet.validateUserContact(adminUser, e);
			if(e.getMessages().size()>0){
				throw e;
			}
			
			adminUser.setUserName(GenerateUniqueUserName.generateUniqueUserNameFromContactName(adminUser));
			adminUser.setWizardState(WizardState.confirmUserNameAndPassword);
		} else if(WizardState.confirmUserNameAndPassword.equals(adminUser.getWizardState())){
			UserBizlet.validateUserNameAndPassword(adminUser,e );
			if(e.getMessages().size()>0){
				throw e;
			}
			
			adminUser.setWizardState(WizardState.confirmGroupMemberships);
		}
		
		
		return new ServerSideActionResult<>(adminUser);
	}
}

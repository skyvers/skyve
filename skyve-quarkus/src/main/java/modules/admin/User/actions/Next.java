package modules.admin.User.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.User.UserBizlet;
import modules.admin.User.UserExtension;
import modules.admin.domain.Group;
import modules.admin.domain.User.GroupSelection;
import modules.admin.domain.User.WizardState;

public class Next implements ServerSideAction<UserExtension> {
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension adminUser, WebContext webContext)
	throws Exception {
		next(adminUser);
		return new ServerSideActionResult<>(adminUser);
	}
	
	public static void next(UserExtension adminUser) throws Exception{
		ValidationException e = new ValidationException();
		
		if(WizardState.confirmContact.equals(adminUser.getWizardState())){
			
			e.getMessages().add(new Message("You must either search for an existing contact or choose to create a new contact."));
			
		} else if(WizardState.createContact.equals(adminUser.getWizardState())){
			
			// validate previous data entry
			UserBizlet.validateUserContact(adminUser, e);
			
			// propose a new username
			if(adminUser.getContact()!=null && adminUser.getContact().getEmail1()!=null) {
				adminUser.setUserName(adminUser.getContact().getEmail1());
			} else {
				adminUser.setUserName(GenerateUniqueUserName.generateUniqueUserNameFromContactName(adminUser));
			}
			adminUser.setWizardState(WizardState.confirmUserNameAndPassword);
			
		} else if(WizardState.confirmUserNameAndPassword.equals(adminUser.getWizardState())){
			
			// validate previous data entry
			UserBizlet.validateUserNameAndPassword(adminUser,e );
			
			// create a new empty group for group creation, if selected
			if(GroupSelection.newGroup.equals(adminUser.getGroupSelection())) {
				adminUser.setNewGroup(Group.newInstance());
			} else {
				adminUser.setNewGroup(null);
			}
			
			adminUser.setWizardState(WizardState.confirmGroupMemberships);
		}
		
		// throw any validation exceptions collected so far
		if(e.getMessages().size()>0){
			throw e;
		}
	}
}

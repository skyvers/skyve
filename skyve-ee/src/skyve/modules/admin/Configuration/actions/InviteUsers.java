package modules.admin.Configuration.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.Configuration;

public class InviteUsers implements ServerSideAction<Configuration> {
	private static final long serialVersionUID = -4884065778373508731L;

	@Override
	public ServerSideActionResult<Configuration> execute(Configuration bean, WebContext webContext)
			throws Exception {

		//all emails need to be validated (in case a comma separated list is provided)
		// fail if emails are invalid
		
		// create a contact
		// create a user - not with a generated password
		// assign groups as selected
		
		// send invitation email
//		for (User newUser: newUsers){
//			CommunicationUtil.sendFailSafeSystemCommunication(ConfigurationBizlet.SYSTEM_USER_INVITATION, ConfigurationBizlet.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT, ConfigurationBizlet.SYSTEM_USER_INVITATION_DEFAULT_BODY, ResponseMode.EXPLICIT, null, newUser);
//		}
		
		
		return new ServerSideActionResult<>(bean);
	}
}

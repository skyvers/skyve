package modules.admin.Configuration.actions;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;

public class InviteUsers implements ServerSideAction<Configuration> {

	private static final String SPACE_COMMA_OR_SEMICOLON = "[\\s,;]+";

	public static final String SYSTEM_USER_INVITATION = "SYSTEM User Invitation";
	public static final String SYSTEM_USER_INVITATION_DEFAULT_SUBJECT = "New user account";
	public static final String SYSTEM_USER_INVITATION_DEFAULT_BODY = "A user account has been created for you. Click <a href=\"someUr\" to log in";
	
	private static final long serialVersionUID = -4884065778373508731L;

	@Override
	public ServerSideActionResult<Configuration> execute(Configuration bean, WebContext webContext)
			throws Exception {

		// validate that some groups are selected
		if(bean.getUserInvitationGroups().isEmpty()) {
			throw new ValidationException(new Message("You must select at least one permission group for invited users."));
		}
		
		if(bean.getUserInvitiationEmailList()==null) {
			throw new ValidationException(new Message("Enter one or more email addresses, separated by space ( ), comma (,) or semicolon (;)."));
		}
		
		// all emails need to be validated prior to any sending
		// (comma separated or semicolon separated list is provided)
		Persistence pers = CORE.getPersistence();
		Customer customer = pers.getUser().getCustomer();
		Module module = customer.getModule(Configuration.MODULE_NAME);
		
		Document docContact = module.getDocument(customer, Contact.DOCUMENT_NAME);
		List<Contact> validatedContacts = new ArrayList<>();
		for(String emailAddress: bean.getUserInvitiationEmailList().split(SPACE_COMMA_OR_SEMICOLON)) {
			
			// construct contact and validate
			Contact c = Contact.newInstance();
			c.setName(emailAddress);
			c.setContactType(ContactType.person);
			c.setEmail1(emailAddress);
			try {
				ValidationUtil.validateBeanAgainstDocument(docContact, c);
			} catch (ValidationException ve) {
				
				// rethrow with a more meaningful message
				StringBuilder errorMessage = new StringBuilder(64);
				errorMessage.append(emailAddress).append(" is not a valid Email Address");
				throw new ValidationException(new Message(errorMessage.toString()));
			}
			validatedContacts.add(c);
		}

//		for(Contact c: validatedContacts) {
			// create a user - not with a generated password
			// assign groups as selected
			
			// send invitation email
//			for (User newUser: newUsers){
//				CommunicationUtil.sendFailSafeSystemCommunication(ConfigurationBizlet.SYSTEM_USER_INVITATION, ConfigurationBizlet.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT, ConfigurationBizlet.SYSTEM_USER_INVITATION_DEFAULT_BODY, ResponseMode.EXPLICIT, null, newUser);
//			}
//		}
		
		
		return new ServerSideActionResult<>(bean);
	}
}

package modules.admin.UserList.actions;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.UserList.UserListUtil;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User;
import modules.admin.domain.UserList;

public class InviteUsers implements ServerSideAction<UserList> {

	private static final String SPACE_COMMA_OR_SEMICOLON = "[\\s,;]+";
	
	private static final long serialVersionUID = -4884065778373508731L;

	@Override
	public ServerSideActionResult<UserList> execute(UserList bean, WebContext webContext)
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
		Module module = customer.getModule(UserList.MODULE_NAME);
		
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

		for (Contact contact: validatedContacts) {
			contact = CORE.getPersistence().save(contact);

			final String token = WebUtil.generatePasswordResetToken();

			// create a user - not with a generated password
			final User newUser = User.newInstance();
			newUser.setUserName(contact.getEmail1());
			newUser.setPassword(EXT.hashPassword(token));
			newUser.setPasswordExpired(Boolean.TRUE);
			newUser.setPasswordResetToken(token);
			newUser.setContact(contact);
			
			// assign groups as selected
			newUser.getGroups().addAll(bean.getUserInvitationGroups());

			CORE.getPersistence().save(newUser);

			// send invitation email
			CommunicationUtil.sendFailSafeSystemCommunication(UserListUtil.SYSTEM_USER_INVITATION,
					UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT,
					UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_BODY,
					CommunicationUtil.ResponseMode.EXPLICIT, null, newUser);
		}

		return new ServerSideActionResult<>(bean);
	}
}

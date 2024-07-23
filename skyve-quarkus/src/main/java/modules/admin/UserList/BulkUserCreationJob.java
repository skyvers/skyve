package modules.admin.UserList;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.job.Job;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.Group.GroupExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.UserList;

public class BulkUserCreationJob extends Job {
	private static final String SPACE_COMMA_OR_SEMICOLON = "[\\s,;]+";

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {

		List<String> log = getLog();

		UserList userList = (UserList) getBean();

		log.add("Job to create new users has commenced");
		
		List<Contact> validatedContacts = getValidatedContacts(userList);
		int size = validatedContacts.size();
		int processed = 1;
		int created = 0;
		for (Contact contact : validatedContacts) {

			User newUser = createUserFromContact(contact, userList, log);
			if (newUser != null) {
				created++;
				if (Boolean.TRUE.equals(userList.getBulkCreateWithEmail())) {
					try {
						// send invitation email
						CommunicationUtil.sendFailSafeSystemCommunication(UserListUtil.SYSTEM_USER_INVITATION,
								UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT,
								UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_BODY,
								CommunicationUtil.ResponseMode.EXPLICIT, null, newUser);

						log.add("New user '" + newUser.getUserName() + "' created and emailed ok");
					} catch (@SuppressWarnings("unused") Exception e) {
						log.add("New user '" + newUser.getUserName() + "' created ok but emailed FAILED");
					}
				} else {
					log.add("New user '" + newUser.getUserName() + "'created ok");
				}
			}

			setPercentComplete((int) (((float) processed++) / ((float) size) * 100F));
		}

		setPercentComplete(100);
		log.add("Job to create new users has completed - " + created + " users created");
	}

	public static void kickoffJob(UserList bean, WebContext webContext) throws Exception {

		// validate that some groups are selected
		if (bean.getUserInvitationGroups().isEmpty()) {
			throw new ValidationException(new Message("You must select at least one permission group for invited users"));
		}

		if (bean.getUserInvitiationEmailList() == null) {
			throw new ValidationException(new Message("Enter one or more email addresses, separated by space ( ), comma (,) or semicolon (;)."));
		}

		// validate email address before commencing
		@SuppressWarnings("unused")
		List<Contact> validatedContacts = getValidatedContacts(bean);

		Persistence persistence = CORE.getPersistence();
		org.skyve.metadata.user.User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(UserList.MODULE_NAME);
		JobMetaData job = module.getJob("jBulkUserCreation");

		EXT.getJobScheduler().runOneShotJob(job, bean, user);

		webContext.growl(MessageSeverity.info, "The creation job has started - check job log for detailed results");
	}

	/**
	 * Perform basic validation of Contact email addresses prior to new users being created
	 * and return a list of Valid Contacts
	 * 
	 * @param bean
	 * @return
	 */
	public static List<Contact> getValidatedContacts(UserList bean) {

		List<Contact> validatedContacts = new ArrayList<>();

		// all emails need to be validated prior to any sending
		// (comma separated or semicolon separated list is provided)
		Persistence pers = CORE.getPersistence();
		Customer customer = pers.getUser().getCustomer();
		Module module = customer.getModule(UserList.MODULE_NAME);

		Document docContact = module.getDocument(customer, Contact.DOCUMENT_NAME);
		for (String emailAddress : bean.getUserInvitiationEmailList().split(SPACE_COMMA_OR_SEMICOLON)) {

			// construct contact and validate
			Contact c = Contact.newInstance();
			c.setName(emailAddress);
			c.setContactType(ContactType.person);
			c.setEmail1(emailAddress);
			try {
				ValidationUtil.validateBeanAgainstDocument(docContact, c);
			} catch (@SuppressWarnings("unused") ValidationException ve) {
				// rethrow with a more meaningful message
				StringBuilder errorMessage = new StringBuilder(64);
				errorMessage.append("'").append(emailAddress).append("' is not a valid Email Address");
				throw new ValidationException(new Message(errorMessage.toString()));
			}
			validatedContacts.add(c);
		}

		return validatedContacts;
	}

	/**
	 * Create a new user from a contact, using the groups assigned in the UserList bean
	 * 
	 * @param contact
	 * @param bean
	 * @return the new User (saved)
	 */
	public static User createUserFromContact(Contact c, UserList bean, List<String> log) throws Exception {

		// check if user already exists
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(User.userNamePropertyName, c.getEmail1());
		q.setMaxResults(1);

		User found = q.beanResult();
		if (found != null) {
			log.add("The user '" + c.getEmail1() + "' already exists - no action will be taken");
			return null;
		}


		try {
			Contact contact = c;
			contact = CORE.getPersistence().save(contact);

			final String token = UUID.randomUUID().toString() + Long.toString(System.currentTimeMillis());
			// create a user - not with a generated password
			User newUser = User.newInstance();
			newUser.setUserName(contact.getEmail1());
			newUser.setPassword(EXT.hashPassword(token));
			newUser.setPasswordExpired(Boolean.TRUE);
			newUser.setPasswordResetToken(token);
			newUser.setContact(contact);

			// set default module
			String defaultModuleName = bean.getDefaultModuleName();
			if (defaultModuleName != null) {
				newUser.setHomeModule(defaultModuleName);
			}

			// assign groups as selected
			List<GroupExtension> groups = bean.getUserInvitationGroups();
			for (GroupExtension group : groups) {
				// this job is in its own thread, own persistence, own transaction
				// and UserList bean is from another persistence that haven�t been fully populagted
				// so we need to re-retrieve each group
				String id = group.getBizId();
				CORE.getPersistence().evictCached(group);
				group = CORE.getPersistence().retrieve(Group.MODULE_NAME, Group.DOCUMENT_NAME, id);

				// now the group can be added to the new user
				newUser.getGroups().add(group);
			}

			newUser = CORE.getPersistence().save(newUser);
			return newUser;
		} catch (@SuppressWarnings("unused") Exception e) {
			log.add("The user '" + c.getEmail1()+ "' could not be created");
			return null;
		}
	}
}

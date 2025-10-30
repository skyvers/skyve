package modules.admin.User;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.CommunicationUtil;

import jakarta.enterprise.inject.Default;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Contact.ContactExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.User.actions.GenerateUniqueUserName;
import modules.admin.UserList.UserListUtil;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.UserProxy;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient UserService userService;
 */
@Default
public class UserService {

	/**
	 * Validates that the user has a valid contact associated with them.
	 * Checks that the user has a contact assigned, the contact has a name,
	 * and the contact has an email address.
	 * 
	 * @param bean The UserExtension instance to validate
	 * @param e The ValidationException to accumulate validation errors
	 */
	@SuppressWarnings("static-method")
	public void validateUserContact(UserExtension bean, ValidationException e) {
		if (bean.getContact() == null) {
			e.getMessages()
					.add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
							"You must specify a contact person for this user."));
		} else if (bean.getContact().getName() == null) {
			e.getMessages()
					.add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
							"You must enter a name."));
		} else if (bean.getContact().getEmail1() == null) {
			e.getMessages()
					.add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.email1PropertyName),
							"You must enter an email address."));
		}
	}

	/**
	 * Validates the username and password for a user.
	 * Validates:
	 * - Username is not null and meets minimum length requirements
	 * - Username uniqueness across the system
	 * - Password complexity requirements
	 * - Password confirmation matching
	 * - Proper password hashing and storage
	 * 
	 * @param user The UserExtension instance to validate
	 * @param e The ValidationException to accumulate validation errors
	 * @throws Exception if there are issues accessing the database or configuration
	 */
	@SuppressWarnings("static-method")
	public void validateUserNameAndPassword(UserExtension user, ValidationException e) throws Exception {

		// validate username is not null, not too short and unique
		if (user.getUserName() == null) {
			e.getMessages().add(new Message(User.userNamePropertyName, "Username is required."));
		} else if (!user.isPersisted() && user.getUserName().length() < ConfigurationExtension.MINIMUM_USERNAME_LENGTH) {
			e.getMessages().add(new Message(User.userNamePropertyName, "Username is too short."));
		} else {
			Persistence pers = CORE.getPersistence();
			DocumentQuery q = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
			q.getFilter().addEquals(User.userNamePropertyName, user.getUserName());
			q.getFilter().addNotEquals(Bean.DOCUMENT_ID, user.getBizId());

			List<User> otherUsers = q.beanResults();
			if (!otherUsers.isEmpty()) {
				e.getMessages().add(new Message(User.userNamePropertyName, "This username is already being used - try again."));
			} else {

				// validate password
				String hashedPassword = user.getPassword();
				String newPassword = user.getNewPassword();
				String confirmPassword = user.getConfirmPassword();

				if ((newPassword == null) && (confirmPassword == null)) {
					if (hashedPassword == null) {
						Message message = new Message(User.newPasswordPropertyName, "A password is required.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					}
				} else {
					if ((newPassword == null) || (confirmPassword == null)) {
						Message message = new Message(User.newPasswordPropertyName,
								"New Password and Confirm Password are required to change the password.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					} else if (newPassword.equals(confirmPassword)) {

						// check for suitable complexity
						ConfigurationExtension configuration = Configuration.newInstance();
						if (!configuration.meetsComplexity(newPassword)) {
							StringBuilder sb = new StringBuilder(64);
							sb.append("The password you have entered is not sufficiently complex. ");
							sb.append(configuration.getPasswordRuleDescription());
							sb.append(" Please re-enter and confirm the password.");
							Message message = new Message(ChangePassword.newPasswordPropertyName, sb.toString());
							e.getMessages().add(message);
						}

						hashedPassword = EXT.hashPassword(newPassword);
						user.setPassword(hashedPassword);

						// clear reset password details
						if (user.getGeneratedPassword() != null && !user.getGeneratedPassword().equals(user.getNewPassword())) {
							user.setPasswordExpired(Boolean.FALSE);
							user.setGeneratedPassword(null);
						}
						// clear out the new password entry fields
						user.setNewPassword(null);
						user.setConfirmPassword(null);
					} else {
						Message message = new Message(User.newPasswordPropertyName,
								"You did not type the same password.  Please re-enter the password again.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					}
				}
			}
		}

	}

	/**
	 * Validates that an active user has at least one role or group assigned.
	 * This ensures that users can properly log in and have appropriate permissions.
	 * Inactive users are not required to have roles or groups.
	 * 
	 * @param user The User instance to validate
	 * @param e The ValidationException to accumulate validation errors
	 */
	@SuppressWarnings("static-method")
	public void validateGroups(User user, ValidationException e) {
		if (!Boolean.TRUE.equals(user.getInactive()) && user.getRoles().isEmpty() && user.getGroups().isEmpty()) {
			e.getMessages().add(new Message("At least 1 role or group is required to enable correct login for this user."));
		}
	}

	/**
	 * Retrieves all available customer roles and module roles for a given user.
	 * This includes both customer-specific roles and module roles (if allowed by the customer).
	 * Module role descriptions are truncated to 50 characters for display purposes.
	 * 
	 * @param user The Skyve user for whom to retrieve available roles
	 * @return A list of DomainValue objects representing available roles with their names and descriptions
	 */
	@SuppressWarnings("static-method")
	public List<DomainValue> getCustomerRoleValues(org.skyve.metadata.user.User user) {
		List<DomainValue> result = new ArrayList<>();

		Customer customer = user.getCustomer();

		// Add customer roles
		for (CustomerRole role : customer.getRoles()) {
			result.add(new DomainValue(role.getName()));
		}

		if (customer.isAllowModuleRoles()) {
			for (Module module : customer.getModules()) {
				for (Role role : module.getRoles()) {

					String roleName = role.getName();
					String roleDescription = role.getLocalisedDescription();

					if (roleDescription != null) {
						if (roleDescription.length() > 50) {
							roleDescription = roleDescription.substring(0, 47) + "...";
						}
						result.add(new DomainValue(String.format("%s.%s", module.getName(), roleName),
								String.format("%s - %s (%s)", module.getLocalisedTitle(), roleName, roleDescription)));
					} else {
						result.add(new DomainValue(String.format("%s.%s", module.getName(), roleName),
								String.format("%s - %s", module.getLocalisedTitle(), roleName)));
					}
				}
			}
		}

		return result;
	}

	/**
	 * Handles the progression through the user creation wizard states.
	 * This method manages the multi-step user creation process, validating data
	 * at each step and advancing to the next appropriate wizard state.
	 * 
	 * @param adminUser The UserExtension instance progressing through the wizard
	 * @throws Exception if validation fails or there are system errors during processing
	 */
	public void next(UserExtension adminUser) throws Exception {
		ValidationException e = new ValidationException();

		if (WizardState.confirmContact.equals(adminUser.getWizardState())) {

			e.getMessages().add(new Message("You must either search for an existing contact or choose to create a new contact."));

		} else if (WizardState.createContact.equals(adminUser.getWizardState())) {

			// validate previous data entry
			validateUserContact(adminUser, e);

			// propose a new username
			if (adminUser.getContact() != null && adminUser.getContact().getEmail1() != null) {
				adminUser.setUserName(adminUser.getContact().getEmail1());
			} else {
				adminUser.setUserName(GenerateUniqueUserName.generateUniqueUserNameFromContactName(adminUser));
			}
			adminUser.setWizardState(WizardState.confirmUserNameAndPassword);

		} else if (WizardState.confirmUserNameAndPassword.equals(adminUser.getWizardState())) {

			// validate previous data entry
			validateUserNameAndPassword(adminUser, e);

			// create a new empty group for group creation, if selected
			if (GroupSelection.newGroup.equals(adminUser.getGroupSelection())) {
				adminUser.setNewGroup(Group.newInstance());
			} else {
				adminUser.setNewGroup(null);
			}

			adminUser.setWizardState(WizardState.confirmGroupMemberships);
		}

		// throw any validation exceptions collected so far
		if (e.getMessages().size() > 0) {
			throw e;
		}
	}

	/**
	 * Evicts a UserProxy bean from the shared cache if it exists.
	 * 
	 * @param bean The UserExtension instance whose proxy should be evicted from cache
	 */
	@SuppressWarnings("static-method")
	public void evictUserProxy(UserExtension bean) {
		Persistence p = CORE.getPersistence();
		String bizId = bean.getBizId();
		if (p.sharedCacheBean(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, bizId)) {
			p.evictSharedCachedBean(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, bizId);
		}
	}

	/**
	 * Returns the current session/conversation user as an Admin module User
	 *
	 * @return The current {@link modules.admin.User.UserExtension}
	 */
	@SuppressWarnings("static-method")
	public UserExtension currentAdminUser() {
		UserExtension result = null;
		try {
			Persistence p = CORE.getPersistence();
			result = p.retrieve(User.MODULE_NAME,
					User.DOCUMENT_NAME,
					p.getUser().getId());
		} catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}

		return result;
	}

	/**
	 * Creates a new admin User for a given contact
	 * - sets the new user name to be the contact email address
	 * - adds the specified group privileges to the user
	 * - sets their home Module (if provided)
	 * - sets an expired password (to force them to reset their password)
	 * - sets a password reset token that can be provided to the user to reset their password
	 * - optionally sends an invitation email
	 *
	 * @param contact the Contact to create the new User from
	 * @param groupName The name of the group
	 * @param homeModuleName
	 * @param sendInvitation
	 * @return
	 */
	@SuppressWarnings("static-method")
	public UserExtension createAdminUserFromContactWithGroup(ContactExtension contact, final String groupName,
			final String homeModuleName, final boolean sendInvitation) {

		if (contact == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.contact");
		}

		if (groupName == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.groupName");
		}

		// check if user already exists
		DocumentQuery q = CORE.getPersistence()
				.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(User.userNamePropertyName, contact.getEmail1());
		q.setMaxResults(1);

		UserExtension found = q.beanResult();
		if (found != null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.duplicateUser");
		}

		// check the group exists
		DocumentQuery qGroup = CORE.getPersistence().newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		qGroup.getFilter().addEquals(Group.namePropertyName, groupName);
		qGroup.setMaxResults(1);
		GroupExtension group = qGroup.beanResult();

		if (group == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invalidGroup");
		}

		// check the home module name exists (Skyve will throw if it doesn't)
		CORE.getCustomer().getModule(homeModuleName);

		// save the contact to validate the contact and so that it can be referenced by the user
		ContactExtension newContact = CORE.getPersistence().save(contact);

		final String token = UUID.randomUUID().toString() + Long.toString(System.currentTimeMillis());
		// create a user - not with a generated password
		UserExtension newUser = User.newInstance();
		newUser.setUserName(newContact.getEmail1());
		newUser.setPassword(EXT.hashPassword(token));
		newUser.setPasswordExpired(Boolean.TRUE);
		newUser.setPasswordResetToken(token);
		newUser.setHomeModule(homeModuleName);
		newUser.setContact(newContact);

		// assign group
		newUser.getGroups().add(group);

		newUser = CORE.getPersistence().save(newUser);

		if (sendInvitation) {
			try {
				// send invitation email
				CommunicationUtil.sendFailSafeSystemCommunication(UserListUtil.SYSTEM_USER_INVITATION,
						UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT,
						UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_BODY,
						CommunicationUtil.ResponseMode.EXPLICIT, null, newUser);

			} catch (Exception e) {
				throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invitation", e);
			}
		}
		return newUser;
	}

	/**
	 * Returns the current session/conversation user as an Admin module UserProxy
	 *
	 * @return The current {@link modules.admin.domain.UserProxy}
	 */
	@SuppressWarnings("static-method")
	public UserProxyExtension currentAdminUserProxy() {
		UserProxyExtension result = null;
		try {
			Persistence p = CORE.getPersistence();
			result = p.retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, p.getUser().getId());
		} catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}

		return result;
	}

	/**
	 * Returns whether the current user has access to the specified module
	 *
	 * @param moduleName
	 * @return
	 */
	@SuppressWarnings("static-method")
	public boolean currentUserHasModule(String moduleName) {
		boolean result = false;
		org.skyve.metadata.user.User user = CORE.getPersistence().getUser();
		Customer customer = user.getCustomer();
		for (Module module : customer.getModules()) {
			if (module.getName().equals(moduleName)) {
				result = true;
				break;
			}
		}
		return result;
	}
}

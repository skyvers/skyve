package modules.admin.User;

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import modules.admin.Configuration.ComplexityModel;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

import org.apache.commons.codec.binary.Base64;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;

public class UserBizlet extends Bizlet<User> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5947293714061984815L;

	/**
	 * Populate the data group association if required.
	 */
	@Override
	public User newInstance(User bean) throws Exception {
		Persistence persistence = CORE.getPersistence();
		org.skyve.metadata.user.User user = persistence.getUser();
		String myDataGroupId = user.getDataGroupId();
		if (myDataGroupId != null) {
			DocumentQuery query = persistence.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
			query.getFilter().addEquals(Bean.DOCUMENT_ID, myDataGroupId);
			List<DataGroup> dataGroups = query.beanResults();
			for (DataGroup dataGroup : dataGroups) {
				bean.setDataGroup(dataGroup);
			}
			// ensure that the new user record belongs to user's data group
			bean.setBizDataGroupId(myDataGroupId);
		}

		// set defaults
		bean.setCreatedDateTime(new DateTime());
		bean.setWizardState(WizardState.confirmContact);

		return bean;
	}

	/**
	 * Ensure that if a password is entered, it is applied to the user's hashed
	 * password.
	 */
	@Override
	public void validate(User user, ValidationException e) throws Exception {

		validateUserContact(user, e);

		validateUserNameAndPassword(user, e);

		validateGroups(user, e);

		// ensure that the user record belongs to assigned user's data group
		user.setBizDataGroupId((user.getDataGroup() != null) ? user.getDataGroup().getBizId() : null);
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName) throws Exception {
		Persistence persistence = CORE.getPersistence();

		if (User.groupsPropertyName.equals(fieldName)) {
			DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
			query.addBoundOrdering(Group.namePropertyName, SortDirection.ascending);
			List<Group> groups = query.beanResults();
			List<DomainValue> result = new ArrayList<>(groups.size());
			for (Group group : groups) {
				result.add(new DomainValue(group.getBizId(), group.getBizKey()));
			}

			return result;
		} else if (User.dataGroupPropertyName.equals(fieldName)) {
			DocumentQuery query = persistence.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
			query.addBoundOrdering(DataGroup.namePropertyName, SortDirection.ascending);
			List<DataGroup> groups = query.beanResults();
			List<DomainValue> result = new ArrayList<>(groups.size());
			for (DataGroup group : groups) {
				result.add(new DomainValue(group.getBizId(), group.getBizKey()));
			}

			return result;
		} else if (User.homeModulePropertyName.equals(fieldName)) {
			org.skyve.metadata.user.User user = persistence.getUser();
			Customer customer = user.getCustomer();
			Set<String> moduleNames = user.getAccessibleModuleNames();
			List<DomainValue> result = new ArrayList<>();
			for (String moduleName : moduleNames) {
				result.add(new DomainValue(moduleName, customer.getModule(moduleName).getTitle()));
			}

			return result;
		}

		return super.getVariantDomainValues(fieldName);
	}

	public static List<DomainValue> getCustomerRoleValues(org.skyve.metadata.user.User user) {
		List<DomainValue> result = new ArrayList<>();
		for (Module module : user.getCustomer().getModules()) {
			for (Role role : module.getRoles()) {
				result.add(new DomainValue(module.getName() + '.' + role.getName(), module.getTitle() + " - " + role.getName()));
			}
		}

		return result;
	}

	@Override
	public void preSave(User bean) throws Exception {
		if (bean.getGeneratedPassword() != null) {
			bean.setPasswordExpired(Boolean.TRUE);
		}

		// contact must be same datagroup as user
		if (bean.getContact() != null) {
			if (bean.getDataGroup() == null) {
				bean.getContact().setBizDataGroupId(null);
			} else {
				bean.getContact().setBizDataGroupId(bean.getDataGroup().getBizId());
			}
		}
	}

	public static void validateUserContact(User bean, ValidationException e) {
		if (bean.getContact() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName), "You must specify a contact person for this user."));
		} else if (bean.getContact().getName() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName), "You must enter a name."));
		} else if (bean.getContact().getEmail1() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.email1PropertyName), "You must enter an email address."));
		}
	}

	public static void validateUserNameAndPassword(User user, ValidationException e) throws Exception {

		// validate username is not null, not too short and unique
		if (user.getUserName() == null) {
			e.getMessages().add(new Message(User.userNamePropertyName, "Username is required."));
		} else if (!user.isPersisted() && user.getUserName().length() < ComplexityModel.MINIMUM_USERNAME_LENGTH) {
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
						Message message = new Message(User.newPasswordPropertyName, "New Password and Confirm Password are required to change the password.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					} else if (newPassword.equals(confirmPassword)) {

						// check for suitable complexity
						Configuration configuration = Configuration.newInstance();
						ComplexityModel cm = new ComplexityModel(configuration.getPasswordComplexityModel());
						if (!newPassword.matches(cm.getComparison())) {
							StringBuilder sb = new StringBuilder(64);
							sb.append("The password you have entered is not sufficiently complex.\n");
							sb.append(cm.getRule());
							sb.append("\nPlease re-enter and confirm the password.");
							Message message = new Message(ChangePassword.newPasswordPropertyName, sb.toString());
							e.getMessages().add(message);
						}

						MessageDigest md = MessageDigest.getInstance(Util.getPasswordHashingAlgorithm());
						Base64 base64Codec = new Base64();
						hashedPassword = new String(base64Codec.encode(md.digest(newPassword.getBytes())));
						user.setPassword(hashedPassword);

						// clear reset password details
						if (user.getGeneratedPassword() != null && !user.getGeneratedPassword().equals(user.getNewPassword())) {
							user.setPasswordExpired(Boolean.FALSE);
							user.setGeneratedPassword(null);
							user.setPasswordLastChanged(new DateTime());
						}
						// clear out the new password entry fields
						user.setNewPassword(null);
						user.setConfirmPassword(null);
					} else {
						Message message = new Message(User.newPasswordPropertyName, "You did not type the same password.  Please re-enter the password again.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					}
				}
			}
		}

	}

	public static void validateGroups(User user, ValidationException e) {
		if (user.getRoles().isEmpty() && user.getGroups().isEmpty()) {
			e.getMessages().add(new Message("At least 1 role or group is required to enable correct login for this user."));
		}
	}

	public static String bizKey(User user) {
		org.skyve.metadata.user.User mUser = CORE.getUser();

		StringBuilder sb = new StringBuilder(64);
		try {
			Customer customer = mUser.getCustomer();
			if (Boolean.TRUE.equals(user.getInactive())) {
				sb.append("INACTIVE ");
			}
			sb.append(Binder.formatMessage(customer, "{userName} - {contact.bizKey}", user));

		} catch (Exception e) {
			sb.append("Unknown");
		}
		return sb.toString();

	}
}

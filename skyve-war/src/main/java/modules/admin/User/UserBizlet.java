package modules.admin.User;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.security.HIBPPasswordValidator;
import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.UserProxy;

public class UserBizlet extends Bizlet<UserExtension> {

	/**
	 * Populate the data group association if required.
	 */
	@Override
	public UserExtension newInstance(UserExtension bean) throws Exception {

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

		// check whether creating a new security group will be mandatory when creating a user
		// this occurs if there are no security groups yet defined
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		bean.setGroupSelection((q.beanResults().isEmpty() ? GroupSelection.newGroup : GroupSelection.existingGroups));
		bean.setGroupsExist((q.beanResults().isEmpty() ? Boolean.FALSE : Boolean.TRUE));

		return bean;
	}

	public static String bizKey(User user) {
		StringBuilder sb = new StringBuilder(64);
		try {
			if (Boolean.TRUE.equals(user.getInactive())) {
				sb.append("INACTIVE ");
			}
			sb.append(Binder.formatMessage("{userName} - {contact.bizKey}", user));

		} catch (@SuppressWarnings("unused") Exception e) {
			sb.append("Unknown");
		}
		return sb.toString();
	}

	@Override
	public void preRerender(String source, UserExtension bean, WebContext webContext) throws Exception {

		if (User.groupSelectionPropertyName.equals(source)) {
			if (GroupSelection.newGroup.equals(bean.getGroupSelection())) {
				bean.setNewGroup(Group.newInstance());
			} else {
				bean.setNewGroup(null);
			}
		}
		if (User.newPasswordPropertyName.equals(source)) {
			String newPassword = bean.getNewPassword();
			if (newPassword != null) {
				if (UtilImpl.CHECK_FOR_BREACHED_PASSWORD) {
					if (HIBPPasswordValidator.isPasswordPwned(newPassword)) {
						webContext.growl(MessageSeverity.warn, Util.i18n("warning.breachedPassword"));
					}
				}
			}
		}

		super.preRerender(source, bean, webContext);
	}

	@Override
	public UserExtension preExecute(ImplicitActionName actionName,
			UserExtension bean,
			Bean parentBean,
			WebContext webContext)
			throws Exception {
		if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {

			// if a new group was created, assign the user to the group membership
			if (bean.getNewGroup() != null) {
				bean.getGroups().add(bean.getNewGroup());
			}
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	/**
	 * Ensure that if a password is entered, it is applied to the user's hashed
	 * password.
	 */
	@Override
	public void validate(UserExtension user, ValidationException e) throws Exception {

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
				result.add(new DomainValue(moduleName, customer.getModule(moduleName).getLocalisedTitle()));
			}

			return result;
		}

		return super.getVariantDomainValues(fieldName);
	}

	public static List<DomainValue> getCustomerRoleValues(org.skyve.metadata.user.User user) {
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

	@Override
	public void preSave(UserExtension bean) throws Exception {

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
		
		// user must be saved to be visible within the users own User-scope
		bean.setBizUserId(bean.getBizId());

		// If password has changed...
		if (bean.isPersisted() && (bean.originalValues().containsKey(User.passwordPropertyName) 
				|| (bean.originalValues().containsKey(User.newPasswordPropertyName) && bean.originalValues().containsKey(User.confirmPasswordPropertyName)))) {
			// Set password last changed date/time, IP & region (if configured)
			bean.setPasswordLastChanged(new DateTime());
			HttpServletRequest request = EXT.getHttpServletRequest();
			if (request != null) {
				String ipAddress = SecurityUtil.getSourceIpAddress(request);
				bean.setPasswordLastChangedIP(ipAddress);
				if (ipAddress != null) {
					String countryCode = EXT.getGeoIPService().geolocate(ipAddress).countryCode();
					if (countryCode != null) {
						bean.setPasswordLastChangedCountryCode(countryCode);
					}
				}
			}
			// Set switch in stash (see postSave)
			CORE.getStash().put("passwordChanged", Boolean.TRUE);
		}
	}

	/**
	 * Reset the assigned roles model.
	 */
	@Override
	public void postSave(UserExtension bean) throws Exception {
		// If password has changed...
		if (Boolean.TRUE.equals(CORE.getStash().get("passwordChanged"))) {
			// Remove any remember-me tokens
			Persistence persistence = CORE.getPersistence();
			new SkyveRememberMeTokenRepository().removeUserTokens(persistence, bean.getBizCustomer() + '/' + bean.getUserName());

			// Remove any active user sessions
			org.skyve.metadata.user.User user = persistence.getUser();
			StateUtil.removeSessions(user.getId());

			// Send email notification
			try {
				Customer customer = user.getCustomer();
				Module module = customer.getModule(ChangePassword.MODULE_NAME);
				final JobMetaData passwordChangeNotificationJobMetadata = module.getJob("jPasswordChangeNotification");
				EXT.getJobScheduler().runOneShotJob(passwordChangeNotificationJobMetadata, bean, user);
			} catch (Exception e) {
				Util.LOGGER.warning("Failed to kick off password change notification job");
				e.printStackTrace();
			}

			// Record security event in security log
			SecurityUtil.log("Password Change", bean.getUserName() + " changed their password");
			
			// Clear stash
			CORE.getStash().remove("passwordChanged");
		}
		
		bean.clearAssignedRoles();
		bean.setNewGroup(null);
		bean.setNewPassword(null);
		evictUserProxy(bean);
	}
	
	@Override
	public void preDelete(UserExtension bean) throws Exception {
		evictUserProxy(bean);
	}

	// Evict UserProxy bean if its been cached
	private static void evictUserProxy(UserExtension bean) {
		Persistence p = CORE.getPersistence();
		String bizId = bean.getBizId();
		if (p.sharedCacheBean(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, bizId)) {
			p.evictSharedCachedBean(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, bizId);
		}
	}
	
	public static void validateUserContact(UserExtension bean, ValidationException e) {
		if (bean.getContact() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName), "You must specify a contact person for this user."));
		} else if (bean.getContact().getName() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName), "You must enter a name."));
		} else if (bean.getContact().getEmail1() == null) {
			e.getMessages().add(new Message(Binder.createCompoundBinding(User.contactPropertyName, Contact.email1PropertyName), "You must enter an email address."));
		}
	}

	public static void validateUserNameAndPassword(UserExtension user, ValidationException e) throws Exception {

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
						Message message = new Message(User.newPasswordPropertyName, "New Password and Confirm Password are required to change the password.");
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
						Message message = new Message(User.newPasswordPropertyName, "You did not type the same password.  Please re-enter the password again.");
						message.addBinding(User.confirmPasswordPropertyName);
						e.getMessages().add(message);
					}
				}
			}
		}

	}

	public static void validateGroups(User user, ValidationException e) {
		if (!Boolean.TRUE.equals(user.getInactive()) && user.getRoles().isEmpty() && user.getGroups().isEmpty()) {
			e.getMessages().add(new Message("At least 1 role or group is required to enable correct login for this user."));
		}
	}

}

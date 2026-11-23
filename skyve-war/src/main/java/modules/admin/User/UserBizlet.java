package modules.admin.User;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.security.HIBPPasswordValidator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import modules.admin.Contact.ContactExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.AdminJob;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.UserProxy;

/**
 * Implements admin user document lifecycle rules for defaults, validation, and security events.
 */
public class UserBizlet extends Bizlet<UserExtension> {
	private static final String PASSWORD_CHANGED_STASH_KEY = "passwordChanged";
	private static final String PREVIOUS_USER_NAME_STASH_KEY = "previousUserName";

	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	/**
	 * Populate the data group association if required.
	 */
	@Override
	public UserExtension newInstance(UserExtension bean) throws Exception {

		Persistence persistence = CORE.getPersistence();
		org.skyve.metadata.user.User user = persistence.getUser();
		String myDataGroupId = user.getDataGroupId();
		if (myDataGroupId != null) {
			DocumentQuery query = persistence.newDocumentQuery(AdminDocument.DATA_GROUP);
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
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(AdminDocument.GROUP);
		bean.setGroupSelection((q.beanResults().isEmpty() ? GroupSelection.newGroup : GroupSelection.existingGroups));
		bean.setGroupsExist((q.beanResults().isEmpty() ? Boolean.FALSE : Boolean.TRUE));

		return bean;
	}

	/**
	 * Reacts to wizard-selection and password changes before rerender.
	 *
	 * @param source The field that triggered rerender.
	 * @param bean The current user bean.
	 * @param webContext The current web context.
	 * @throws Exception If superclass handling fails.
	 */
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
						webContext.growl(MessageSeverity.warn, Util.nullSafeI18n("warning.breachedPassword"));
					}
				}
			}
		}

		super.preRerender(source, bean, webContext);
	}

	/**
	 * Applies pre-execute save defaults such as assigning a newly-created group.
	 *
	 * @param actionName The implicit action being processed.
	 * @param bean The current user bean.
	 * @param parentBean Optional parent bean.
	 * @param webContext The current web context.
	 * @return The user bean to continue processing.
	 * @throws Exception If superclass processing fails.
	 */
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

		userService.validateUserContact(user, e);

		userService.validateUserNameAndPassword(user, e);

		userService.validateGroups(user, e);

		// ensure that the user record belongs to assigned user's data group
		user.setBizDataGroupId((user.getDataGroup() != null) ? user.getDataGroup().getBizId() : null);
	}

	/**
	 * Resolves selectable values for groups, data groups, and home modules.
	 *
	 * @param fieldName The field requesting values.
	 * @return Domain values for supported fields.
	 * @throws Exception If query or metadata access fails.
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName) throws Exception {
		Persistence persistence = CORE.getPersistence();

		if (User.groupsPropertyName.equals(fieldName)) {
			DocumentQuery query = persistence.newDocumentQuery(AdminDocument.GROUP);
			query.addBoundOrdering(Group.namePropertyName, SortDirection.ascending);
			List<Group> groups = query.beanResults();
			List<DomainValue> result = new ArrayList<>(groups.size());
			for (Group group : groups) {
				result.add(new DomainValue(group.getBizId(), group.getBizKey()));
			}

			return result;
		} else if (User.dataGroupPropertyName.equals(fieldName)) {
			DocumentQuery query = persistence.newDocumentQuery(AdminDocument.DATA_GROUP);
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

	/**
	 * Applies persistence-side transformations before user save.
	 *
	 * @param bean The user bean about to be saved.
	 * @throws Exception If state mutation fails.
	 */
	@Override
	public void preSave(UserExtension bean) throws Exception {
		if (bean.getGeneratedPassword() != null) {
			bean.setPasswordExpired(Boolean.TRUE);
		}

		// user and contact must be visible within the user's own User-scope
		String bizId = bean.getBizId();
		bean.setBizUserId(bizId);
		ContactExtension contact = bean.getContact();
		if (contact != null) {
			contact.setBizUserId(bizId);
			DataGroup dataGroup = bean.getDataGroup();
			if (dataGroup == null) {
				bean.getContact().setBizDataGroupId(null);
			} else {
				bean.getContact().setBizDataGroupId(dataGroup.getBizId());
			}
		}

		// If password has changed...
		if (bean.isPersisted() && (bean.originalValues().containsKey(User.passwordPropertyName)
				|| (bean.originalValues().containsKey(User.newPasswordPropertyName)
						&& bean.originalValues().containsKey(User.confirmPasswordPropertyName)))) {
			// Set password last changed date/time, IP & region (if configured)
			bean.setPasswordLastChanged(new DateTime());
			if (EXT.isWebRequest()) {
				HttpServletRequest request = EXT.getHttpServletRequest();
				String ipAddress = SecurityUtil.getSourceIpAddress(request);
				bean.setPasswordLastChangedIP(ipAddress);
				String countryCode = EXT.getGeoIPService().geolocate(ipAddress).countryCode();
				if (countryCode != null) {
					bean.setPasswordLastChangedCountryCode(countryCode);
				}
			}
			// Set switch in stash (see postSave)
			CORE.getStash().put(PASSWORD_CHANGED_STASH_KEY, Boolean.TRUE);
		}

		// If the user name has changed, remember the old one (see postSave)
		if (bean.isPersisted() && bean.originalValues().containsKey(UserProxy.userNamePropertyName)) {
			Object previousUserName = bean.originalValues().get(UserProxy.userNamePropertyName);
			if (previousUserName instanceof String) {
				CORE.getStash().put(PREVIOUS_USER_NAME_STASH_KEY, previousUserName);
			}
		}
	}

	/**
	 * Reset the assigned roles model.
	 */
	@Override
	public void postSave(UserExtension bean) throws Exception {
		// If password has changed...
		if (Boolean.TRUE.equals(CORE.getStash().get(PASSWORD_CHANGED_STASH_KEY))) {
			// Remove any remember-me tokens and active user sessions
			signOutEverywhere(bean, bean.getUserName());

			// Send email notification
			try {
				org.skyve.metadata.user.User user = bean.toMetaDataUser();
				Customer customer = user.getCustomer();
				final JobMetaData passwordChangeNotificationJobMetadata = AdminJob.J_PASSWORD_CHANGE_NOTIFICATION.toJob(customer);
				EXT.getJobScheduler().runOneShotJob(passwordChangeNotificationJobMetadata, bean, user);
			} catch (Exception e) {
				LOGGER.warn("Failed to kick off password change notification job", e);
			}

			// Record security event in security log
			SecurityUtil.log("Password Change", bean.getUserName() + " changed their password",
					UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS);

			// Clear stash
			CORE.getStash().remove(PASSWORD_CHANGED_STASH_KEY);
		}

		// If the user name has changed, the old name's remember-me tokens would sign in a later user of that name
		Object previousUserName = CORE.getStash().remove(PREVIOUS_USER_NAME_STASH_KEY);
		if (previousUserName instanceof String name) {
			signOutEverywhere(bean, name);
		}

		// A disabled user must not be signed in by an old cookie or session, nor have them come back if re-enabled.
		// (Spring refuses to sign in a disabled user, so this only removes what would otherwise live on.)
		if (isDisabled(bean)) {
			signOutEverywhere(bean, bean.getUserName());
		}

		bean.clearAssignedRoles();
		bean.setNewGroup(null);
		bean.setNewPassword(null);
		userService.evictUserProxy(bean);
	}

	/**
	 * Removes a deleted user's remember-me tokens and sessions, so a user created later with the same
	 * user name cannot be signed in by the old cookies.
	 *
	 * @param bean The user that was deleted.
	 * @throws Exception If removal fails.
	 */
	@Override
	public void postDelete(UserExtension bean) throws Exception {
		signOutEverywhere(bean, bean.getUserName());
	}

	/**
	 * Evicts user-proxy cache prior to deleting the user.
	 *
	 * @param bean The user being deleted.
	 * @throws Exception If proxy eviction fails.
	 */
	@Override
	public void preDelete(UserExtension bean) throws Exception {
		userService.evictUserProxy(bean);
	}

	/**
	 * Whether Spring Security refuses to sign this user in: {@code inactive}, or not {@code activated}
	 * (see {@code not inactive and activated} in the sign in queries).
	 */
	private static boolean isDisabled(UserExtension bean) {
		return Boolean.TRUE.equals(bean.getInactive()) || Boolean.FALSE.equals(bean.getActivated());
	}

	/**
	 * Removes all of a user's remember-me tokens (every device, in the current transaction) and ends all their sessions.
	 *
	 * @param bean The user.
	 * @param userName The user name the tokens are keyed by, which is not the bean's if it was renamed.
	 */
	private static void signOutEverywhere(UserExtension bean, String userName) {
		SignOut.everywhere(CORE.getPersistence(), Objects.requireNonNull(bean.getBizCustomer()), userName, bean.getBizId());
	}
}

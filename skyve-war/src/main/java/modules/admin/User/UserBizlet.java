package modules.admin.User;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
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
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import modules.admin.domain.User.WizardState;

public class UserBizlet extends Bizlet<UserExtension> {
	@Inject
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
			StateUtil.removeSessions(bean.getBizId());

			// Send email notification
			try {
				org.skyve.metadata.user.User user = bean.toMetaDataUser();
				Customer customer = user.getCustomer();
				Module module = customer.getModule(ChangePassword.MODULE_NAME);
				final JobMetaData passwordChangeNotificationJobMetadata = module.getJob("jPasswordChangeNotification");
				EXT.getJobScheduler().runOneShotJob(passwordChangeNotificationJobMetadata, bean, user);
			} catch (Exception e) {
				LOGGER.warn("Failed to kick off password change notification job", e);
			}

			// Record security event in security log
			SecurityUtil.log("Password Change", bean.getUserName() + " changed their password",
					UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS);

			// Clear stash
			CORE.getStash().remove("passwordChanged");
		}

		bean.clearAssignedRoles();
		bean.setNewGroup(null);
		bean.setNewPassword(null);
		userService.evictUserProxy(bean);
	}

	@Override
	public void preDelete(UserExtension bean) throws Exception {
		userService.evictUserProxy(bean);
	}

}

package org.skyve.impl.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.domain.Bean;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

public class SQLMetaDataUtil {
	/**
	 * Prevent construction.
	 */
	private SQLMetaDataUtil() {
		// no implementation
	}

	public static final String ADMIN_MODULE_NAME = "admin";
	public static final String CHANGE_PASSWORD_DOCUMENT_NAME = "ChangePassword";
	public static final String CONTACT_DOCUMENT_NAME = "Contact";
	public static final String CONFIGURATION_DOCUMENT_NAME = "Configuration";
	public static final String USER_DOCUMENT_NAME = "User";
	public static final String USER_ROLE_DOCUMENT_NAME = "UserRole";
	
	public static final String CONFIRM_PASSWORD_PROPERTY_NAME = "confirmPassword";
	public static final String CONTACT_PROPERTY_NAME = "contact";
	public static final String CONTACT_TYPE_PROPERTY_NAME = "contactType";
	public static final String EMAIL1_PROPERTY_NAME = "email1";
	public static final String FROM_EMAIL_PROPERTY_NAME = "fromEmail";
	public static final String NAME_PROPERTY_NAME = "name";
	public static final String NEW_PASSWORD_PROPERTY_NAME = "newPassword";
	public static final String OLD_PASSWORD_PROPERTY_NAME = "oldPassword";
	public static final String PASSWORD_PROPERTY_NAME = "password";
	public static final String PASSWORD_RESET_EMAIL_BODY_PROPERTY_NAME = "passwordResetEmailBody";
	public static final String PASSWORD_RESET_EMAIL_SUBJECT_PROPERTY_NAME = "passwordResetEmailSubject";
	public static final String PASSWORD_RESET_TOKEN_PROPERTY_NAME = "passwordResetToken";
	public static final String ROLE_NAME_PROPERTY_NAME = "roleName";
	public static final String ROLES_PROPERTY_NAME = "roles";
	public static final String USER_NAME_PROPERTY_NAME = "userName";
	
	public static final String MAKE_PASSWORD_CHANGE_ACTION_NAME = "MakePasswordChange";
	
	public static void populateUser(User user) {
		UserImpl internalUser = (UserImpl) user;
		try {
			Customer customer = user.getCustomer();
			AbstractRepository repository = AbstractRepository.get();
			Module admin = repository.getModule(customer, ADMIN_MODULE_NAME);
			String ADM_SecurityUser = admin.getDocument(customer, USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			String ADM_SecurityUserRole = admin.getDocument(customer, USER_ROLE_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			String ADM_Contact = admin.getDocument(customer, CONTACT_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			String ADM_SecurityUser_groups = ADM_SecurityUser + "_groups";
			String ADM_SecurityGroup = admin.getDocument(customer, "Group").getPersistent().getPersistentIdentifier();
			String ADM_SecurityGroupRole = admin.getDocument(customer, "GroupRole").getPersistent().getPersistentIdentifier();
			
			StringBuilder sql = new StringBuilder(512);
			sql.append("select u.bizId, " +
						"u.password, " +
						"u.passwordExpired, " +
						"c.bizId as contactId, " +
						"c.name as contactName, " +
						"u.dataGroup_id as dataGroupId, " +
						"u.homeModule, " +
						"r.roleName ");
			sql.append("from ").append(ADM_SecurityUser).append(" u ");
			sql.append("inner join ").append(ADM_SecurityUserRole).append(" r ");
			sql.append("on r.parent_id = u.bizId " +
						"and r.bizCustomer = u.bizCustomer ");
			sql.append("inner join ").append(ADM_Contact).append(" c ");
			sql.append("on u.contact_id = c.bizId ");
			sql.append("where u.userName = '").append(user.getName()).append("' ");
			sql.append("and u.bizCustomer = '").append(customer.getName()).append("' ");
			sql.append("union " +
						"select u.bizId, " +
						"u.password, " +
						"u.passwordExpired, " +
						"c.bizId as contactId, " +
						"c.name as contactName, " +
						"u.dataGroup_id as dataGroupId, " +
						"u.homeModule, " +
						"r.roleName ");
			sql.append("from ").append(ADM_SecurityUser).append(" u ");
			sql.append("inner join ").append(ADM_SecurityUser_groups).append(" gs ");
			sql.append("on gs.owner_id = u.bizId ");
			sql.append("inner join ").append(ADM_SecurityGroup).append(" g ");
			sql.append("on g.bizId = gs.element_id ");
			sql.append("inner join ").append(ADM_SecurityGroupRole).append(" r ");
			sql.append("on r.parent_id = g.bizId " +
						"and r.bizCustomer = g.bizCustomer ");
			sql.append("inner join ").append(ADM_Contact).append(" c ");
			sql.append("on u.contact_id = c.bizId ");
			sql.append("where u.userName = '").append(user.getName()).append("' ");
			sql.append("and u.bizCustomer = '").append(customer.getName()).append("'");

			boolean firstRow = true;
			for (DynaBean userRoleRow : SQLUtil.retrieveListForSQL(null, null, sql.toString(), null, false, false)) {
				if (firstRow) {
					internalUser.setId((String) userRoleRow.get("bizid"));
					internalUser.setPasswordHash((String) userRoleRow.get("password"));
					Boolean passwordChangeRequired = (Boolean) userRoleRow.get("passwordexpired");
					internalUser.setPasswordChangeRequired(Boolean.TRUE.equals(passwordChangeRequired));
					internalUser.setContactId((String) userRoleRow.get("contactid"));
					internalUser.setContactName((String) userRoleRow.get("contactname"));
					internalUser.setDataGroupId((String) userRoleRow.get("datagroupid"));
					internalUser.setHomeModuleName((String) userRoleRow.get("homemodule"));
					firstRow = false;
				}

				String moduleDotRoleName = (String) userRoleRow.get("rolename");
				int dotIndex = moduleDotRoleName.indexOf('.');
				if (dotIndex > 0) {
					String moduleName = moduleDotRoleName.substring(0, dotIndex);
					String roleName = moduleDotRoleName.substring(dotIndex + 1);
					Module module = customer.getModule(moduleName);
					Role role = module.getRole(roleName);
					if (role != null) {
						internalUser.addRole((RoleImpl) role);
					}
				}
				else {
					String roleName = moduleDotRoleName;
					CustomerRole customerRole = customer.getRole(roleName);
					if (customerRole != null) {
						for (Role role : customerRole.getModuleRoles()) {
							internalUser.addRole((RoleImpl) role);
						}
					}
				}
			}
			if (firstRow) { // no data for this user
				throw new DomainException("The user " + user.getName() + " does not exist.");
			}
		}
		catch (DomainException e) {
			throw new MetaDataException(e);
		}
		finally {
			try {
				SQLUtil.commit(true);
			}
			catch (DomainException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public static List<Bean> retrieveAllJobSchedulesForAllCustomers() {
		List<Bean> result = new ArrayList<>();
		
		// Principal -> User
		Map<String, User> users = new TreeMap<>();
		
		AbstractRepository repository = AbstractRepository.get();

		try {
			Module admin = repository.getModule(null, "admin");
			String ADM_JobSchedule = admin.getDocument(null, "JobSchedule").getPersistent().getPersistentIdentifier();
			String ADM_SecurityUser = admin.getDocument(null, "User").getPersistent().getPersistentIdentifier();

			StringBuilder sql = new StringBuilder(256);
			sql.append("select s.*, u.userName from ").append(ADM_JobSchedule).append(" s inner join ");
			sql.append(ADM_SecurityUser).append(" u on s.runAs_id = u.bizId order by u.bizCustomer");
			
			for (DynaBean jobScheduleRow : SQLUtil.retrieveListForSQL(null, null, sql.toString(), null, false, false)) {
				StringBuilder userPrincipalBuilder = new StringBuilder(128);
				userPrincipalBuilder.append(jobScheduleRow.get("bizcustomer"));
				userPrincipalBuilder.append('/').append(jobScheduleRow.get("username"));
				String userPrincipal = userPrincipalBuilder.toString();
				User user = users.get(userPrincipal);
				if (user == null) {
					user = repository.retrieveUser(userPrincipal);
					users.put(userPrincipal, user);
				}

				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, jobScheduleRow.get("bizid"));
				properties.put("jobName", jobScheduleRow.get("jobname"));
				properties.put("startTime", jobScheduleRow.get("starttime"));
				properties.put("endTime", jobScheduleRow.get("endtime"));
				properties.put("cronExpression", jobScheduleRow.get("cronexpression"));
				properties.put("user", user);
				
				MapBean jobSchedule = new MapBean("admin", "JobSchedule", properties);
				result.add(jobSchedule);
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
		finally {
			try {
				SQLUtil.commit(true);
			}
			catch (DomainException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		return result;
	}
}

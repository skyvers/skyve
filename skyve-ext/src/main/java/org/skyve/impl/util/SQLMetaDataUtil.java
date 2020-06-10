package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;

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
	public static final String PASSWORD_LAST_CHANGED_PROPERTY_NAME = "passwordLastChanged";
	
	public static final String MAKE_PASSWORD_CHANGE_ACTION_NAME = "MakePasswordChange";
	
	public static void populateUser(User user) {
		try (Connection connection = EXT.getDataStoreConnection()) {
			populateUser(user, connection);
		}
		catch (SQLException e) {
			throw new MetaDataException("Could not obtain a data store connection", e);
		}
	}
	
	public static void populateUser(User user, Connection connection) {
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
			String ADM_Configuration = admin.getDocument(customer, CONFIGURATION_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			
			StringBuilder sql = new StringBuilder(512);
			sql.append("select u.bizId, " +
						"u.password, " +
						"u.passwordExpired, " +
						"u.passwordLastChanged, " +
						"p.publicUser_id, " +
						"c.bizId as contactId, " +
						"c.name as contactName, " +
						"c.image as contactImageId, " +
						"u.dataGroup_id as dataGroupId, " +
						"u.homeModule, " +
						"r.roleName ");
			sql.append("from ").append(ADM_SecurityUser).append(" u ");
			sql.append("inner join ").append(ADM_SecurityUserRole).append(" r ");
			sql.append("on r.parent_id = u.bizId ");
			sql.append("inner join ").append(ADM_Contact).append(" c ");
			sql.append("on u.contact_id = c.bizId ");
			sql.append("left outer join ").append(ADM_Configuration).append(" p ");
			sql.append("on u.bizId = p.publicUser_id ");
			sql.append("where u.userName = ? ");
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				sql.append("and u.bizCustomer = ? ");
			}
			sql.append("union " +
						"select u.bizId, " +
						"u.password, " +
						"u.passwordExpired, " +
						"u.passwordLastChanged, " +
						"p.publicUser_id, " +
						"c.bizId as contactId, " +
						"c.name as contactName, " +
						"c.image as contactImageId, " +
						"u.dataGroup_id as dataGroupId, " +
						"u.homeModule, " +
						"r.roleName ");
			sql.append("from ").append(ADM_SecurityUser).append(" u ");
			sql.append("inner join ").append(ADM_SecurityUser_groups).append(" gs ");
			sql.append("on gs.owner_id = u.bizId ");
			sql.append("inner join ").append(ADM_SecurityGroup).append(" g ");
			sql.append("on g.bizId = gs.element_id ");
			sql.append("inner join ").append(ADM_SecurityGroupRole).append(" r ");
			sql.append("on r.parent_id = g.bizId ");
			sql.append("inner join ").append(ADM_Contact).append(" c ");
			sql.append("on u.contact_id = c.bizId ");
			sql.append("left outer join ").append(ADM_Configuration).append(" p ");
			sql.append("on u.bizId = p.publicUser_id ");
			sql.append("where u.userName = ? ");
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				sql.append("and u.bizCustomer = ?");
			}

			String query = sql.toString();
			if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(query + " executed on thread " + Thread.currentThread() + ", connection = " + connection);
			try (PreparedStatement s = connection.prepareStatement(query)) {
				s.setString(1, user.getName());
				if (UtilImpl.CUSTOMER == null) { // multi-tenant
					s.setString(2, customer.getName());
					s.setString(3, user.getName());
					s.setString(4, customer.getName());
				}
				else {
					s.setString(2, user.getName());
				}
				
				try (ResultSet rs = s.executeQuery()) {
					boolean firstRow = true;
					while (rs.next()) {
						if (firstRow) {
							internalUser.setId(rs.getString(1)); // bizId
							internalUser.setPasswordHash(rs.getString(2)); // password
							
							// Determine if a password change is required
							boolean passwordChangeRequired = rs.getBoolean(3); // passwordExpired
							Timestamp passwordLastChanged = rs.getTimestamp(4);
							String publicUserId = rs.getString(5);
							// the public user never requires a password change
							if (publicUserId != null) {
								passwordChangeRequired = false;
							}
							else if (! passwordChangeRequired) {
								if (UtilImpl.PASSWORD_EXPIRY_IN_DAYS > 0) {
									if (passwordLastChanged == null) {
										passwordChangeRequired = true;
									}
									else {
										long passwordExpiryInMillis = passwordLastChanged.getTime() + (UtilImpl.PASSWORD_EXPIRY_IN_DAYS * 86400000);
										if (System.currentTimeMillis() >= passwordExpiryInMillis) {
											passwordChangeRequired = true;
										}
									}
								}
							}
							internalUser.setPasswordChangeRequired(passwordChangeRequired);

							internalUser.setContactId(rs.getString(6)); // contactId
							internalUser.setContactName(rs.getString(7)); // contactName
							internalUser.setContactImageId(rs.getString(8)); // contactImageId
							internalUser.setDataGroupId(rs.getString(9)); // dataGroupId
							internalUser.setHomeModuleName(rs.getString(10)); // homeModule
							firstRow = false;
						}

						String moduleDotRoleName = rs.getString(11); // roleName
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
							CustomerRoleMetaData customerRole = (CustomerRoleMetaData) customer.getRole(roleName);
							if (customerRole != null) {
								for (Role role : customerRole.getModuleRoles(customer)) {
									internalUser.addRole((RoleImpl) role);
								}
							}
						}
					}
					if (firstRow) { // no data for this user
						throw new DomainException("The user " + user.getName() + " does not exist.");
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	public static List<Bean> retrieveAllJobSchedulesForAllCustomers() throws Exception {
		List<Bean> result = new ArrayList<>();
		
		// Principal -> User
		Map<String, User> users = new TreeMap<>();
		
		AbstractRepository repository = AbstractRepository.get();
		Module admin = repository.getModule(null, "admin");
		String ADM_JobSchedule = admin.getDocument(null, "JobSchedule").getPersistent().getPersistentIdentifier();
		String ADM_SecurityUser = admin.getDocument(null, "User").getPersistent().getPersistentIdentifier();

		StringBuilder sql = new StringBuilder(256);
		sql.append("select s.bizId, s.bizCustomer, s.jobName, s.startTime, s.endTime, s.cronExpression, s.disabled,  u.userName from ");
		sql.append(ADM_JobSchedule).append(" s inner join ").append(ADM_SecurityUser).append(" u on s.runAs_id = u.bizId order by u.bizCustomer");
		
		try (SQLDataAccess da = EXT.newSQLDataAccess()) {
			List<Object[]> rows = da.newSQL(sql.toString()).tupleResults();
			for (Object[] row : rows) {
				StringBuilder userPrincipalBuilder = new StringBuilder(128);
				userPrincipalBuilder.append(row[1]); // bizCustomer
				userPrincipalBuilder.append('/').append(row[7]); // userName
				String userPrincipal = userPrincipalBuilder.toString();
				User user = users.get(userPrincipal);
				if (user == null) {
					user = repository.retrieveUser(userPrincipal);
					users.put(userPrincipal, user);
				}

				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, row[0]); // bizId
				properties.put("jobName", row[2]);
				properties.put("startTime", row[3]);
				properties.put("endTime", row[4]);
				properties.put("cronExpression", row[5]);
				properties.put("disabled", row[6]);
				properties.put("user", user);
				
				MapBean jobSchedule = new MapBean("admin", "JobSchedule", properties);
				result.add(jobSchedule);
			}
		}
		
		return result;
	}
	
	public static String retrievePublicUserName(String customerName) {
		String result = null;
		
		String sql = "select u.userName from ADM_Configuration c " +
						"inner join ADM_SecurityUser u on u.bizId = c.publicUser_id ";
		if (UtilImpl.CUSTOMER == null) { // multi-tenant
			sql += "where c.bizCustomer = :bizCustomer";
		}
		try (SQLDataAccess da = EXT.newSQLDataAccess()) {
			SQL s = da.newSQL(sql);
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				s.putParameter(Bean.CUSTOMER_NAME, customerName, false);
			}
			result = s.retrieveScalar(String.class);
		}
		catch (Exception e) {
			UtilImpl.LOGGER.warning("Could not retrieve public user for customer " + customerName);
			e.printStackTrace();
		}
		
		return result;
	}
}

package org.skyve.impl.metadata.repository;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobSchedule;
import org.skyve.job.UserJobSchedule;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.SQL;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Adds security integration to LocalDesignRepository.
 * 
 * @author Mike
 */
public class LocalDataStoreRepository extends LocalDesignRepository {

    private static final Logger QUERY_LOGGER = Category.QUERY.logger();
    private static final Logger LOGGER = LoggerFactory.getLogger(LocalDataStoreRepository.class);

	@Override
	public UserImpl retrieveUser(String userPrincipal) {
		if (userPrincipal == null) {
			throw new IllegalStateException("No-one is logged in - cannot retrieve the skyve user.");
		}

		UserImpl result = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(userPrincipal);
		if (result != null) {
			resetUserPermissions(result);
			
			if (result.getLanguageTag() == null) {
				result.setLanguageTag(result.getCustomer().getLanguageTag());
			}
		}
		
		return result;
	}
	
	@Override
	public boolean populatePermissions(User user) {
		try (Connection connection = EXT.getDataStoreConnection()) {
			return populateUser(user, connection);
		}
		catch (SQLException e) {
			throw new MetaDataException("Could not obtain a data store connection", e);
		}
	}
	
	@Override
	public void resetUserPermissions(User user) {
		UserImpl impl = (UserImpl) user;
		impl.clearAllPermissionsAndMenus();

		if (!populatePermissions(user)) {
			throw new SecurityException("the system", user.getName());
		}

		resetMenus(user);
	}
	
	@Override
	public boolean populateUser(User user, Connection connection) {
		UserImpl internalUser = (UserImpl) user;
		try {
			Customer customer = user.getCustomer();
			Module admin = getModule(customer, AppConstants.ADMIN_MODULE_NAME);
			@SuppressWarnings("null")
			String ADM_SecurityUser = admin.getDocument(customer, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			@SuppressWarnings("null")
			String ADM_SecurityUserRole = admin.getDocument(customer, AppConstants.USER_ROLE_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			@SuppressWarnings("null")
			String ADM_Contact = admin.getDocument(customer, AppConstants.CONTACT_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			String ADM_SecurityUser_groups = ADM_SecurityUser + '_' + AppConstants.GROUPS_ATTRIBUTE_NAME;
			@SuppressWarnings("null")
			String ADM_SecurityGroup = admin.getDocument(customer, AppConstants.GROUP_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			@SuppressWarnings("null")
			String ADM_SecurityGroupRole = admin.getDocument(customer, AppConstants.GROUP_ROLE_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			@SuppressWarnings("null")
			String ADM_Configuration = admin.getDocument(customer, AppConstants.CONFIGURATION_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
			
			StringBuilder sql = new StringBuilder(512);
			sql.append("select u.bizId, " +
						"u.inactive, " +
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
						"u.inactive, " +
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
			if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} executed on thread {}, connection = {}", query, Thread.currentThread(), connection);
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

							// Check if user is inactive
							if (rs.getBoolean(2)) { // inactive
								return false;
							}
							
							internalUser.setPasswordHash(rs.getString(3)); // password

							// Determine if a password change is required
							boolean passwordChangeRequired = rs.getBoolean(4); // passwordExpired
							Timestamp passwordLastChanged = rs.getTimestamp(5);
							String publicUserId = rs.getString(6);
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
										long passwordExpiryInMillis = passwordLastChanged.getTime() + (UtilImpl.PASSWORD_EXPIRY_IN_DAYS * 86400000L);
										if (System.currentTimeMillis() >= passwordExpiryInMillis) {
											passwordChangeRequired = true;
										}
									}
								}
							}
							internalUser.setPasswordChangeRequired(passwordChangeRequired);

							internalUser.setContactId(rs.getString(7)); // contactId
							internalUser.setContactName(rs.getString(8)); // contactName
							internalUser.setContactImageId(rs.getString(9)); // contactImageId
							internalUser.setDataGroupId(rs.getString(10)); // dataGroupId
							internalUser.setHomeModuleName(rs.getString(11)); // homeModule
							
							firstRow = false;
						}

						String moduleDotRoleName = rs.getString(12); // roleName
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
						return false;
					}
				}
			}
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}

		return true;
	}
	
	@Override
	public List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		List<UserJobSchedule> result = new ArrayList<>();
		
		// Principal -> User
		Map<String, User> users = new TreeMap<>();
		
		Module admin = getModule(null, AppConstants.ADMIN_MODULE_NAME);
		@SuppressWarnings("null")
		final String admJobSchedule = admin.getDocument(null, AppConstants.JOB_SCHEDULE_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
		@SuppressWarnings("null")
		final String admSecurityUser = admin.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();

		StringBuilder sql = new StringBuilder(256);
		sql.append("select s.bizId, s.bizCustomer, s.jobName, s.startTime, s.endTime, s.cronExpression, s.disabled, u.userName from ");
		sql.append(admJobSchedule).append(" s inner join ").append(admSecurityUser).append(" u on s.runAs_id = u.bizId order by u.bizCustomer");
		
		try (SQLDataAccess da = EXT.newSQLDataAccess();
				AutoClosingIterable<Object[]> rows = da.newSQL(sql.toString()).tupleIterable()) {
			for (Object[] row : rows) {
				// Discard jobs that are disabled.
				// NB Done here as SQL for booleans is dialect specific
				if (Boolean.TRUE.equals(row[6])) { // disabled
					continue;
				}

				StringBuilder userPrincipalBuilder = new StringBuilder(128);
				userPrincipalBuilder.append(row[1]); // bizCustomer
				userPrincipalBuilder.append('/').append(row[7]); // userName
				String userPrincipal = userPrincipalBuilder.toString();
				User user = users.get(userPrincipal);
				if (user == null) {
					user = retrieveUser(userPrincipal);
					users.put(userPrincipal, user);
				}

				JobSchedule jobSchedule = new JobSchedule();
				jobSchedule.setUuid((String) row[0]);
				jobSchedule.setJobName((String) row[2]);
				jobSchedule.setStartTime((Date) row[3]);
				jobSchedule.setEndTime((Date) row[4]);
				jobSchedule.setCronExpression((String) row[5]);

				if (user == null) {
					throw new DomainException("Could not schedule job " + jobSchedule.getJobName() + " as user " + userPrincipal + " does not exist");
				}
				result.add(new UserJobSchedule(jobSchedule, user));
			}
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException("Problem occurred retrieving job schedules", e);
		}
		
		return result;
	}
	
	@Override
	public List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		List<UserJobSchedule> result = new ArrayList<>();

		// Principal -> User
		Map<String, User> users = new TreeMap<>();

		Module admin = getModule(null, AppConstants.ADMIN_MODULE_NAME);
		@SuppressWarnings("null")
		String admReportTemplate = admin.getDocument(null, AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();
		@SuppressWarnings("null")
		String admSecurityUser = admin.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier();

		StringBuilder sql = new StringBuilder(256);
		sql.append("select s.bizId, s.bizCustomer, s.name, s.startTime, s.endTime, s.cronExpression, s.scheduled, u.userName from ");
		sql.append(admReportTemplate).append(" s left join ").append(admSecurityUser).append(" u on s.runAs_id = u.bizId").append(" order by u.bizCustomer");

		try (SQLDataAccess da = EXT.newSQLDataAccess();
				AutoClosingIterable<Object[]> rows = da.newSQL(sql.toString()).tupleIterable()) {
			for (Object[] row : rows) {
				// Discard reports that are not scheduled.
				// NB Done here as SQL for booleans is dialect specific
				if (! Boolean.TRUE.equals(row[6])) { // scheduled
					continue;
				}
				
				StringBuilder userPrincipalBuilder = new StringBuilder(128);
				userPrincipalBuilder.append(row[1]); // bizCustomer
				userPrincipalBuilder.append('/').append(row[7]); // userName
				String userPrincipal = userPrincipalBuilder.toString();
				User user = users.get(userPrincipal);
				if (user == null) {
					user = retrieveUser(userPrincipal);
					users.put(userPrincipal, user);
				}

				JobSchedule jobSchedule = new JobSchedule();
				jobSchedule.setUuid((String) row[0]);
				jobSchedule.setJobName((String) row[2]);
				jobSchedule.setStartTime((Date) row[3]);
				jobSchedule.setStartTime((Date) row[4]);
				jobSchedule.setCronExpression((String) row[5]);

				if (user == null) {
					throw new DomainException("Could not schedule report " + jobSchedule.getJobName() + " as user " + userPrincipal + " does not exist");
				}
				result.add(new UserJobSchedule(jobSchedule, user));
			}
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException("Problem occurred retrieving report schedules", e);
		}

		return result;
	}

	@Override
	public String retrievePublicUserName(String customerName) {
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
			LOGGER.warn("Could not retrieve public user for customer {}", customerName, e);
			e.printStackTrace();
		}
		
		return result;
	}
}

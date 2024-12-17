package org.skyve.impl.web.spring;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.skyve.EXT;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UUIDv7;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.stereotype.Component;

@Component
public class SecurityListener {

    private static final Logger LOGGER = LoggerFactory.getLogger(SecurityListener.class);

	@EventListener
	@SuppressWarnings("static-method")
	public void onAuthenticationFailure(AuthenticationFailureBadCredentialsEvent evt) {
		String userName = SkyveSpringSecurity.userNameFromPrincipal(evt.getAuthentication().getPrincipal());
		LOGGER.warn("Login Attempt failed for user " + userName);
		if (userName != null) {
			recordLoginFailure(userName);
		}
	}
	
	@EventListener
	@SuppressWarnings("static-method")
	public void onAuthenticationSuccess(AuthenticationSuccessEvent evt) {
		String userName = SkyveSpringSecurity.userNameFromPrincipal(evt.getAuthentication().getPrincipal());
		if (userName == null) {
			LOGGER.warn("Cannot reset login failures in org.skyve.impl.web.spring.SecurityListener.onAuthenticationSuccess() as the principal type is not known. If you are using a Spring Security plugin, please override this class in your project and handle the principal yourself.");
		}
		LOGGER.info("Login Attempt succeeded for user " + userName);
		if (userName != null) {
			resetLoginFailure(userName);
		}
	}
	
	private static void recordLoginFailure(String username) {
		SkyveDialect dialect = AbstractHibernatePersistence.getDialect(UtilImpl.DATA_STORE.getDialectClassName());
		RDBMS rdbms = dialect.getRDBMS();
		String sql = null;
		if (RDBMS.h2.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = ifNull(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizId = ?";
		}
		else if (RDBMS.mysql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = ifNull(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizId = ?";
		}
		else if (RDBMS.sqlserver.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = coalesce(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizId = ?";
		}
		else if (RDBMS.postgresql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = coalesce(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizId = ?";
		}
		else {
			LOGGER.warn("Login Failure for " + username + " was not recorded because " + rdbms + " is not suported in SecurityListener");
			return;
		}

		// NB select and then update by bizId to ensure row lock across all databases.
		try (Connection c = EXT.getDataStoreConnection()) {
			String bizId = getBizId(username, c, false);
			if (bizId != null) {
				try (PreparedStatement ps = c.prepareStatement(sql)) {
					ps.setTimestamp(1, new java.sql.Timestamp(System.currentTimeMillis()));
					ps.setString(2, bizId);
					ps.executeUpdate();
				}
				catch (Exception e) {
					c.rollback();
					throw e;
				}
			}
			
			sql = "insert into ADM_UserLoginRecord (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, loginDateTime, failed) values (?, ?, ?, ?, ?, ?, ?, ?, ?)";
			try (PreparedStatement ps = c.prepareStatement(sql)) {
				String bizCustomer = UtilImpl.CUSTOMER;
				String userName = username;
				int slashIndex = username.indexOf('/');
				if (slashIndex > 0) {
					userName = UtilImpl.processStringValue(userName.substring(slashIndex + 1));
					if (bizCustomer == null) {
						bizCustomer = UtilImpl.processStringValue(username.substring(0, slashIndex));
					}
					if (userName == null) {
						userName = "unknown";
					}
					if (bizCustomer == null) {
						bizCustomer = "unknown";
					}
				}
				java.sql.Timestamp now = new java.sql.Timestamp(System.currentTimeMillis());
				ps.setString(1, UUIDv7.create().toString());
				ps.setInt(2, 0);
				ps.setString(3, new OptimisticLock("unknown", now).toString());
				ps.setString(4, "Failed Login attempt: " + userName + " @ " + TimeUtil.formatISODate(now, false));
				ps.setString(5, bizCustomer);
				ps.setString(6, "unknown");
				ps.setString(7, userName);
				ps.setTimestamp(8, new java.sql.Timestamp(System.currentTimeMillis()));
				ps.setBoolean(9, true);
				ps.executeUpdate();
			}
			catch (Exception e) {
				c.rollback();
				throw e;
			}

			c.commit();
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	private static void resetLoginFailure(String username) {
		String sql = "update ADM_SecurityUser set authenticationFailures = 0, lastAuthenticationFailure = null where bizId = ?";

		// NB select and then update by bizId to ensure row lock across all databases.
		try (Connection c = EXT.getDataStoreConnection()) {
			String bizId = getBizId(username, c, true);
			if (bizId != null) {
				try (PreparedStatement ps = c.prepareStatement(sql)) {
					ps.setString(1, bizId);
					ps.executeUpdate();
					c.commit();
				}
				catch (Exception e) {
					c.rollback();
					throw e;
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}
	
	private static String getBizId(String fullUsername, Connection c, boolean forReset) throws SQLException {
		String result = null;
		
		String bizCustomer = null;
		String username = fullUsername;
		if (username != null) {
			int slashIndex = username.indexOf('/');
			if (slashIndex > 0) {
				bizCustomer = username.substring(0, slashIndex);
				username = username.substring(slashIndex + 1);
			}
		}
		// Cater for multi-tenancy
		String sql = (UtilImpl.CUSTOMER == null) ? 
						"select bizId, authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where bizCustomer = ? and userName = ?":
							"select bizId, authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where userName = ?";
		try (PreparedStatement ps = c.prepareStatement(sql)) {
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				ps.setString(1, bizCustomer);
				ps.setString(2, username);
			}
			else {
				ps.setString(1, username);
			}
			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					// NB only return the bizId for resetLoginFailure if authenticationFailures is not 0 or lastAuthenticationFailure is not null
					if (forReset) {
						// authenticationFailures is not 0 
						int authenticationFailures = rs.getInt(2);
						if (rs.wasNull() || (authenticationFailures != 0)) {
							result = rs.getString(1);
						}
						else {
							// lastAuthenticationFailure is not null
							rs.getObject(3);
							if (! rs.wasNull()) {
								result = rs.getString(1);
							}
						}
					}
					// always return the bizId for recordLoginFailure
					else {
						result = rs.getString(1);
					}
				}
			}
		}
		
		return result;
	}
}

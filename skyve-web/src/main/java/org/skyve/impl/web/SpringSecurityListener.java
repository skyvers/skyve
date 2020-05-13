package org.skyve.impl.web;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.skyve.EXT;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.springframework.context.event.EventListener;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.core.userdetails.User;
import org.springframework.stereotype.Component;

@Component
public class SpringSecurityListener {
	@EventListener
	@SuppressWarnings("static-method")
	public void onAuthenticationFailure(AuthenticationFailureBadCredentialsEvent evt) {
		String username = (String) evt.getAuthentication().getPrincipal();
		UtilImpl.LOGGER.warning("Login Attempt failed for user " + username);
		recordLoginFailure(username);
	}
	
	@EventListener
	@SuppressWarnings("static-method")
	public void onAuthenticationSuccess(AuthenticationSuccessEvent evt) {
		String username = ((User) evt.getAuthentication().getPrincipal()).getUsername();
		UtilImpl.LOGGER.info("Login Attempt succeeded for user " + username);
		resetLoginFailure(username);
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
			UtilImpl.LOGGER.warning("Login Failure for " + username + " was not recorded because " + rdbms + " is not suported in SpringSecurityListener");
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
		try (PreparedStatement ps = c.prepareStatement("select bizId, authenticationFailures, lastAuthenticationFailure from ADM_SecurityUser where bizCustomer = ? and userName = ?")) {
			ps.setString(1, bizCustomer);
			ps.setString(2, username);
			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					// NB only return the bizId for resetLoginFailure if authenticationFailures is not 0 or lastAuthenticationFailure is not null
					if (forReset) {
						int authenticationFailures = rs.getInt(2);
						if (rs.wasNull() || (authenticationFailures != 0)) {
							result = rs.getString(1);
						}
						else {
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

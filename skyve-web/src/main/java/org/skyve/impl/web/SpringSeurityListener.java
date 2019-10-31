package org.skyve.impl.web;

import java.sql.Connection;
import java.sql.PreparedStatement;

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
public class SpringSeurityListener {
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
			sql = "update ADM_SecurityUser set authenticationFailures = ifNull(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizCustomer || '/' || userName = ?";
		}
		else if (RDBMS.mysql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = ifNull(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where concat(bizCustomer, '/', userName) = ?";
		}
		else if (RDBMS.sqlserver.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = coalesce(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizCustomer + '/' + userName = ?";
		}
		else if (RDBMS.postgresql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = coalesce(authenticationFailures, 0) + 1, lastAuthenticationFailure = ? where bizCustomer || '/' || userName = ?";
		}
		else {
			UtilImpl.LOGGER.warning("Login Failure for " + username + " was not recorded because " + rdbms + " is not suported in SpringSecurityListener");
			return;
		}

		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement ps = c.prepareStatement(sql)) {
				ps.setTimestamp(1, new java.sql.Timestamp(System.currentTimeMillis()));
				ps.setString(2, username);
				ps.executeUpdate();
				c.commit();
			}
			catch (Exception e) {
				c.rollback();
				throw e;
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	private static void resetLoginFailure(String username) {
		SkyveDialect dialect = AbstractHibernatePersistence.getDialect(UtilImpl.DATA_STORE.getDialectClassName());
		RDBMS rdbms = dialect.getRDBMS();
		String sql = null;
		if (RDBMS.h2.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = 0, lastAuthenticationFailure = null where bizCustomer || '/' || userName = ?";
		}
		else if (RDBMS.mysql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = 0, lastAuthenticationFailure = null where concat(bizCustomer, '/', userName) = ?";
		}
		else if (RDBMS.sqlserver.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = 0, lastAuthenticationFailure = null where bizCustomer + '/' + userName = ?";
		}
		else if (RDBMS.postgresql.equals(rdbms)) {
			sql = "update ADM_SecurityUser set authenticationFailures = 0, lastAuthenticationFailure = null where bizCustomer || '/' || userName = ?";
		}
		else {
			UtilImpl.LOGGER.warning("Reset Login Failure for " + username + " was not recorded because " + rdbms + " is not suported in SpringSecurityListener");
			return;
		}

		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement ps = c.prepareStatement(sql)) {
				ps.setString(1, username);
				ps.executeUpdate();
				c.commit();
			}
			catch (Exception e) {
				c.rollback();
				throw e;
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}
}

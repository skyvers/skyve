package org.skyve.impl.web.spring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.provisioning.UserDetailsManager;

import util.AbstractH2Test;

class SkyveSpringSecurityH2Test extends AbstractH2Test {
	@Test
	void testLoadUserByUsernameMapsMfaConfigurationColumn() throws Exception {
		String username = "tfa.user." + UUID.randomUUID().toString().replace("-", "").substring(0, 12);
		String email = username + "@skyve.org";
		String mfaConfiguration = "[{\"method\":\"EMAIL\",\"enabled\":true}]";
		insertSecurityUser(username, email, mfaConfiguration);

		UserDetailsManager manager = new SkyveSpringSecurity().jdbcUserDetailsManager();
		UserDetails details = manager.loadUserByUsername(CUSTOMER + "/" + username);

		assertTrue(details instanceof TwoFactorAuthUser);
		TwoFactorAuthUser user = (TwoFactorAuthUser) details;
		assertThat(user.getUsername(), is(CUSTOMER + "/" + username));
		assertThat(user.getEmail(), is(email));
		assertThat(user.getMfaConfiguration(), is(mfaConfiguration));
	}

	@Test
	void testLoadUserByUsernameMapsNullMfaConfiguration() throws Exception {
		String username = "tfa.user." + UUID.randomUUID().toString().replace("-", "").substring(0, 12);
		String email = username + "@skyve.org";
		insertSecurityUser(username, email, null);

		UserDetailsManager manager = new SkyveSpringSecurity().jdbcUserDetailsManager();
		UserDetails details = manager.loadUserByUsername(CUSTOMER + "/" + username);

		assertTrue(details instanceof TwoFactorAuthUser);
		TwoFactorAuthUser user = (TwoFactorAuthUser) details;
		assertNull(user.getMfaConfiguration());
	}

	private static void insertSecurityUser(String username, String email, String mfaConfiguration)
	throws SQLException {
		String contactId = UUID.randomUUID().toString();
		String userId = UUID.randomUUID().toString();

		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement contactInsert = c.prepareStatement(
						"insert into ADM_Contact (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, email1) values (?, ?, ?, ?, ?, ?, ?)");
				PreparedStatement userInsert = c.prepareStatement(
						"insert into ADM_SecurityUser (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, password, contact_id, inactive, activated, mfaConfiguration) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")) {
			c.setAutoCommit(true);

			contactInsert.setString(1, contactId);
			contactInsert.setInt(2, 0);
			contactInsert.setString(3, "0");
			contactInsert.setString(4, username);
			contactInsert.setString(5, CUSTOMER);
			contactInsert.setString(6, USER);
			contactInsert.setString(7, email);
			contactInsert.executeUpdate();

			userInsert.setString(1, userId);
			userInsert.setInt(2, 0);
			userInsert.setString(3, "0");
			userInsert.setString(4, username);
			userInsert.setString(5, CUSTOMER);
			userInsert.setString(6, USER);
			userInsert.setString(7, username);
			userInsert.setString(8, EXT.hashPassword(PASSWORD));
			userInsert.setString(9, contactId);
			userInsert.setBoolean(10, false);
			userInsert.setBoolean(11, true);
			userInsert.setString(12, mfaConfiguration);
			userInsert.executeUpdate();
		}
	}
}

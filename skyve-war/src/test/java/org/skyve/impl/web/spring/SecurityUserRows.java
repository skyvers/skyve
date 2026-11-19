package org.skyve.impl.web.spring;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.skyve.EXT;

/**
 * Inserts bare {@code ADM_SecurityUser} (and contact) rows straight into the H2 test database, for tests of
 * the sign in queries that must not go through the admin User bizlet, and removes them (and the login
 * records written for them) afterwards.
 */
final class SecurityUserRows implements AutoCloseable {
	private final List<String> userIds = new ArrayList<>();
	private final List<String> contactIds = new ArrayList<>();
	private final List<String> userNames = new ArrayList<>();

	/**
	 * Describes the user to insert.
	 */
	static final class Spec {
		String customer;
		String userName;
		String password;
		boolean inactive;
		boolean activated = true;
		Integer authenticationFailures;
		Timestamp lastAuthenticationFailure;

		Spec(String customer, String userName, String password) {
			this.customer = customer;
			this.userName = userName;
			this.password = password;
		}

		Spec inactive() {
			inactive = true;
			return this;
		}

		Spec notActivated() {
			activated = false;
			return this;
		}

		Spec failures(int count, long lastFailureMillis) {
			authenticationFailures = Integer.valueOf(count);
			lastAuthenticationFailure = new Timestamp(lastFailureMillis);
			return this;
		}
	}

	/**
	 * Inserts the user and its contact.
	 */
	void insert(Spec spec) throws SQLException {
		String contactId = UUID.randomUUID().toString();
		String userId = UUID.randomUUID().toString();
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement contact = c.prepareStatement(
						"insert into ADM_Contact (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, email1) values (?, 0, '0', ?, ?, 'x', ?)");
				PreparedStatement user = c.prepareStatement(
						"insert into ADM_SecurityUser (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, password, contact_id, inactive, activated, authenticationFailures, lastAuthenticationFailure) values (?, 0, '0', ?, ?, 'x', ?, ?, ?, ?, ?, ?, ?)")) {
			c.setAutoCommit(true);
			contact.setString(1, contactId);
			contact.setString(2, spec.userName);
			contact.setString(3, spec.customer);
			contact.setString(4, spec.userName + "@skyve.org");
			contact.executeUpdate();

			user.setString(1, userId);
			user.setString(2, spec.userName);
			user.setString(3, spec.customer);
			user.setString(4, spec.userName);
			user.setString(5, EXT.hashPassword(spec.password));
			user.setString(6, contactId);
			user.setBoolean(7, spec.inactive);
			user.setBoolean(8, spec.activated);
			if (spec.authenticationFailures == null) {
				user.setNull(9, java.sql.Types.INTEGER);
			} else {
				user.setInt(9, spec.authenticationFailures.intValue());
			}
			user.setTimestamp(10, spec.lastAuthenticationFailure);
			user.executeUpdate();
		}
		userIds.add(userId);
		contactIds.add(contactId);
		userNames.add(spec.userName);
	}

	/**
	 * @return the failure count of the user, or null when the count is null
	 */
	static Integer failures(String customer, String userName) throws SQLException {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement ps = c.prepareStatement(
						"select authenticationFailures from ADM_SecurityUser where bizCustomer = ? and userName = ?")) {
			ps.setString(1, customer);
			ps.setString(2, userName);
			try (ResultSet rs = ps.executeQuery()) {
				rs.next();
				int result = rs.getInt(1);
				return rs.wasNull() ? null : Integer.valueOf(result);
			}
		}
	}

	/**
	 * @return the number of failed login records written for the user name
	 */
	static int failedLoginRecords(String userName) throws SQLException {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement ps = c.prepareStatement(
						"select count(*) from ADM_UserLoginRecord where userName = ? and failed = true")) {
			ps.setString(1, userName);
			try (ResultSet rs = ps.executeQuery()) {
				rs.next();
				return rs.getInt(1);
			}
		}
	}

	@Override
	public void close() {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement records = c.prepareStatement("delete from ADM_UserLoginRecord where userName = ?");
				PreparedStatement users = c.prepareStatement("delete from ADM_SecurityUser where bizId = ?");
				PreparedStatement contacts = c.prepareStatement("delete from ADM_Contact where bizId = ?")) {
			c.setAutoCommit(true);
			for (String name : userNames) {
				records.setString(1, name);
				records.executeUpdate();
			}
			for (String id : userIds) {
				users.setString(1, id);
				users.executeUpdate();
			}
			for (String id : contactIds) {
				contacts.setString(1, id);
				contacts.executeUpdate();
			}
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.authentication.AccountExpiredException;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.event.AbstractAuthenticationFailureEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.core.Authentication;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings({"static-method", "resource", "boxing"})
class SecurityListenerTest {
	private final String originalCustomer = UtilImpl.CUSTOMER;

	@AfterEach
	void restoreCustomerSetting() {
		UtilImpl.CUSTOMER = originalCustomer;
	}

	@Test
	void testBadCredentialsCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new BadCredentialsException("bad credentials")));
	}

	@Test
	void testLockedAccountFailuresCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new LockedException("locked")));
	}

	@Test
	void testAccountExpiryDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AccountExpiredException("expired")));
	}

	@Test
	void testAuthenticationServiceFailureDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AuthenticationServiceException("service failure")));
	}

	@Test
	void onAuthenticationFailureWithNonLockoutExceptionDoesNotThrow() {
		SecurityListener listener = new SecurityListener();
		AbstractAuthenticationFailureEvent event = mock(AbstractAuthenticationFailureEvent.class);
		Authentication auth = mock(Authentication.class);
		when(event.getException()).thenReturn(new AccountExpiredException("expired"));
		when(event.getAuthentication()).thenReturn(auth);
		when(auth.getPrincipal()).thenReturn("user1");

		listener.onAuthenticationFailure(event);
		verify(event).getException();
		assertEquals("user1", SkyveSpringSecurity.userNameFromPrincipal(auth.getPrincipal()));
	}

	@Test
	void onAuthenticationFailureWithLockoutExceptionAndUnknownPrincipalDoesNotThrow() {
		SecurityListener listener = new SecurityListener();
		AbstractAuthenticationFailureEvent event = mock(AbstractAuthenticationFailureEvent.class);
		Authentication auth = mock(Authentication.class);
		when(event.getException()).thenReturn(new BadCredentialsException("bad credentials"));
		when(event.getAuthentication()).thenReturn(auth);
		when(auth.getPrincipal()).thenReturn(new Object());

		listener.onAuthenticationFailure(event);
		assertNull(SkyveSpringSecurity.userNameFromPrincipal(auth.getPrincipal()));
	}

	@Test
	void onAuthenticationSuccessWithUnknownPrincipalDoesNotThrow() {
		SecurityListener listener = new SecurityListener();
		Authentication auth = mock(Authentication.class);
		when(auth.getPrincipal()).thenReturn(new Object());
		AuthenticationSuccessEvent event = new AuthenticationSuccessEvent(auth);

		listener.onAuthenticationSuccess(event);
		assertNull(SkyveSpringSecurity.userNameFromPrincipal(auth.getPrincipal()));
	}

	@Test
	void getBizIdForRecordLoginFailureUsesTenantAndUserSegments() throws Exception {
		UtilImpl.CUSTOMER = null;
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true);
		when(resultSet.getString(1)).thenReturn("biz-1");

		String bizId = invokeGetBizId("acme/jane", connection, false);

		assertEquals("biz-1", bizId);
		verify(statement).setString(1, "acme");
		verify(statement).setString(2, "jane");
	}

	@Test
	void getBizIdForResetReturnsValueWhenFailuresNonZero() throws Exception {
		UtilImpl.CUSTOMER = "singleTenant";
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true);
		when(resultSet.getInt(2)).thenReturn(2);
		when(resultSet.wasNull()).thenReturn(false);
		when(resultSet.getString(1)).thenReturn("biz-2");

		String bizId = invokeGetBizId("acme/john", connection, true);

		assertEquals("biz-2", bizId);
		verify(statement).setString(1, "john");
	}

	@Test
	void getBizIdForResetReturnsValueWhenLastFailureIsNotNull() throws Exception {
		UtilImpl.CUSTOMER = "singleTenant";
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true);
		when(resultSet.getInt(2)).thenReturn(0);
		when(resultSet.wasNull()).thenReturn(false, false);
		when(resultSet.getString(1)).thenReturn("biz-3");

		String bizId = invokeGetBizId("susan", connection, true);

		assertEquals("biz-3", bizId);
	}

	@Test
	void getBizIdForResetReturnsNullWhenNoFailureStatePresent() throws Exception {
		UtilImpl.CUSTOMER = "singleTenant";
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true);
		when(resultSet.getInt(2)).thenReturn(0);
		when(resultSet.wasNull()).thenReturn(false, true);

		String bizId = invokeGetBizId("susan", connection, true);

		assertNull(bizId);
	}

	@Test
	void getBizIdReturnsNullWhenUserRecordNotFound() throws Exception {
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(false);

		String bizId = invokeGetBizId("missing", connection, false);

		assertNull(bizId);
	}

	private static String invokeGetBizId(String username, Connection connection, boolean forReset) throws Exception {
		Method method = SecurityListener.class.getDeclaredMethod("getBizId", String.class, Connection.class, boolean.class);
		method.setAccessible(true);
		return (String) method.invoke(null, username, connection, Boolean.valueOf(forReset));
	}
}

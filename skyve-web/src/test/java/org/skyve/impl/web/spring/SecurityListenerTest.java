package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.springframework.security.authentication.AccountExpiredException;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.LockedException;

class SecurityListenerTest {

	@Test
	@SuppressWarnings("static-method")
	void testBadCredentialsCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new BadCredentialsException("bad credentials")));
	}

	@Test
	@SuppressWarnings("static-method")
	void testLockedAccountFailuresCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new LockedException("locked")));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAccountExpiryDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AccountExpiredException("expired")));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAuthenticationServiceFailureDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AuthenticationServiceException("service failure")));
	}
}

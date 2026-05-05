package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.springframework.security.authentication.AccountExpiredException;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.LockedException;

public class SecurityListenerTest {

	@Test
	public void testBadCredentialsCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new BadCredentialsException("bad credentials")));
	}

	@Test
	public void testLockedAccountFailuresCountTowardLockout() {
		assertTrue(SecurityListener.countsTowardLockout(new LockedException("locked")));
	}

	@Test
	public void testAccountExpiryDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AccountExpiredException("expired")));
	}

	@Test
	public void testAuthenticationServiceFailureDoesNotCountTowardLockout() {
		assertFalse(SecurityListener.countsTowardLockout(new AuthenticationServiceException("service failure")));
	}
}

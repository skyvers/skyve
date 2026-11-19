package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.SQLException;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.SecurityUtil;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.authentication.event.AbstractAuthenticationFailureEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;
import org.springframework.security.core.Authentication;

import util.AbstractH2Test;

/**
 * Account lockout end to end: Spring's authentication provider against the real user query, with
 * {@link SecurityListener} recording the outcomes in the database. A locked account refuses even the
 * right password, a failure while locked extends the lockout, the lockout expires, a successful sign in
 * clears the count and a disabled account is refused without being counted.
 */
class AccountLockoutH2Test extends AbstractH2Test {
	private static final String RIGHT = "Right0Password!";
	private static final long MINUTE = 60_000L;

	private int threshold;
	private int durationMultiple;
	@SuppressWarnings("resource") // closed in cleanUp
	private SecurityUserRows rows;
	private AuthenticationManager manager;
	private final SecurityListener listener = new SecurityListener();

	@BeforeEach
	@SuppressWarnings("resource") // the row set is closed in cleanUp
	void setup() {
		threshold = UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD;
		durationMultiple = UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS;
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = 3;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = 60;
		rows = new SecurityUserRows();

		DaoAuthenticationProvider provider = new DaoAuthenticationProvider(new SkyveSpringSecurity().jdbcUserDetailsManager());
		provider.setPasswordEncoder(SecurityUtil.createDelegatingPasswordEncoder());
		ProviderManager providerManager = new ProviderManager(provider);
		ApplicationEventPublisher publisher = event -> {
			if (event instanceof AbstractAuthenticationFailureEvent failure) {
				listener.onAuthenticationFailure(failure);
			} else if (event instanceof AuthenticationSuccessEvent success) {
				listener.onAuthenticationSuccess(success);
			}
		};
		providerManager.setAuthenticationEventPublisher(new DefaultAuthenticationEventPublisher(publisher));
		manager = providerManager;
	}

	@AfterEach
	void cleanUp() {
		rows.close();
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = threshold;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = durationMultiple;
	}

	@Test
	void failuresBelowTheThresholdDoNotLockTheAccount() throws Exception {
		String name = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT));

		signInWrong(name);
		signInWrong(name);

		assertNotNull(signIn(name, RIGHT));
	}

	@Test
	void theThresholdLocksTheAccountAndEvenTheRightPasswordIsRefused() throws Exception {
		String name = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT));

		signInWrong(name);
		signInWrong(name);
		signInWrong(name);

		assertEquals(Integer.valueOf(3), SecurityUserRows.failures(CUSTOMER, name));
		assertThrows(LockedException.class, () -> signIn(name, RIGHT));
		assertEquals(4, SecurityUserRows.failedLoginRecords(name),
				"the three bad passwords and the locked attempt are all recorded");
	}

	@Test
	void anAttemptWhileLockedCountsAndSoExtendsTheLockout() throws Exception {
		String name = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).failures(3, System.currentTimeMillis()));

		assertThrows(LockedException.class, () -> signIn(name, RIGHT));

		assertEquals(Integer.valueOf(4), SecurityUserRows.failures(CUSTOMER, name));
	}

	@Test
	void theLockoutExpiresAfterFailuresTimesTheDurationAndASignInClearsTheCount() throws Exception {
		// 3 failures lock for 3 minutes: 4 minutes after the last one the account is open again
		String name = newUser(
				new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).failures(3, System.currentTimeMillis() - (4 * MINUTE)));

		assertNotNull(signIn(name, RIGHT));

		assertEquals(Integer.valueOf(0), SecurityUserRows.failures(CUSTOMER, name));
	}

	@Test
	void theLockoutHoldsInsideItsDuration() throws Exception {
		String name = newUser(
				new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).failures(3, System.currentTimeMillis() - (2 * MINUTE)));

		assertThrows(LockedException.class, () -> signIn(name, RIGHT));
	}

	@Test
	void aSuccessfulSignInClearsEarlierFailures() throws Exception {
		String name = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).failures(2, System.currentTimeMillis()));

		signIn(name, RIGHT);

		assertEquals(Integer.valueOf(0), SecurityUserRows.failures(CUSTOMER, name));
	}

	@Test
	void aThresholdOfZeroTurnsLockoutOff() throws Exception {
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = 0;
		String name = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT));

		for (int i = 0; i < 6; i++) {
			signInWrong(name);
		}

		assertNotNull(signIn(name, RIGHT));
	}

	@Test
	void aDisabledAccountIsRefusedAndItsAttemptsAreNotCountedTowardLockout() throws Exception {
		String inactive = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).inactive());
		String notActivated = newUser(new SecurityUserRows.Spec(CUSTOMER, name(), RIGHT).notActivated());

		assertThrows(DisabledException.class, () -> signIn(inactive, RIGHT));
		assertThrows(DisabledException.class, () -> signIn(notActivated, RIGHT));

		assertEquals(0, SecurityUserRows.failedLoginRecords(inactive));
		assertEquals(0, SecurityUserRows.failedLoginRecords(notActivated));
	}

	@Test
	void anUnknownUserGetsTheSameAnswerAsAWrongPasswordAndLeavesAFailedLoginRecord() throws Exception {
		String ghost = name();
		// pass the lockout threshold to prove nothing about an unknown user leaks as a different exception
		for (int i = 0; i < 4; i++) {
			assertThrows(BadCredentialsException.class, () -> signIn(ghost, "whatever"));
		}

		assertEquals(4, SecurityUserRows.failedLoginRecords(ghost));
	}

	@Test
	void aWrongPasswordForOneCustomersUserDoesNotCountAgainstTheSameNameInAnotherCustomer() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		UtilImpl.CUSTOMER = null; // multi-tenant: the customer is part of the sign in name
		try {
			String name = name();
			newUser(new SecurityUserRows.Spec(CUSTOMER, name, RIGHT));
			newUser(new SecurityUserRows.Spec("othercustomer", name, RIGHT));

			signInWrong(name);
			signInWrong(name);
			signInWrong(name);

			assertEquals(Integer.valueOf(3), SecurityUserRows.failures(CUSTOMER, name));
			assertNull(SecurityUserRows.failures("othercustomer", name));
			assertNotNull(manager.authenticate(token("othercustomer", name, RIGHT)));
		} finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	private String newUser(SecurityUserRows.Spec spec) throws SQLException {
		rows.insert(spec);
		return spec.userName;
	}

	private static String name() {
		return "lockout." + UUID.randomUUID().toString().replace("-", "").substring(0, 12);
	}

	private Authentication signIn(String name, String password) {
		return manager.authenticate(token(CUSTOMER, name, password));
	}

	private void signInWrong(String name) {
		assertThrows(BadCredentialsException.class, () -> signIn(name, "wrong-password"));
	}

	private static UsernamePasswordAuthenticationToken token(String customer, String name, String password) {
		return UsernamePasswordAuthenticationToken.unauthenticated(customer + '/' + name, password);
	}
}

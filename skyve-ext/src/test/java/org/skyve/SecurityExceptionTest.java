package org.skyve;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.AccessException;
import org.skyve.domain.messages.SecurityException;

@SuppressWarnings("static-method")
class SecurityExceptionTest {
	@Test
	void securityExceptionConstructorTriggersSecurityLoggingPath() {
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
				() -> new SecurityException("this resource", "tester"));
		assertTrue(e.getMessage().contains("not a good choice"));
	}

	@Test
	void accessExceptionConstructorTriggersSpecialisedSecurityLoggingPath() {
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
				() -> new AccessException("this resource", "tester"));
		assertTrue(e.getMessage().contains("not a good choice"));
	}
}

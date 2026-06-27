package org.skyve.domain.messages;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class SkyveExceptionTest {

	@Test
	@SuppressWarnings("static-method")
	void domainExceptionFiveArgConstructorCoversI18nBranch() {
		// Exercises SkyveException(String, Throwable, boolean, boolean, boolean i18n)
		// with i18n=false so the message is used as-is (covers the non-i18n branch)
		DomainException e = new DomainException("raw message", new RuntimeException("cause"), true, true, false);
		assertNotNull(e.getMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	void domainExceptionFiveArgConstructorWithI18nTrue() {
		// Covers the i18n=true branch of the ternary in the 5-arg constructor
		DomainException e = new DomainException("raw message", new RuntimeException("cause"), true, true, true);
		assertNotNull(e.getMessage());
	}
}

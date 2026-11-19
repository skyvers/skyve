package org.skyve.impl.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

@SuppressWarnings("static-method")
class SkyveLegacyAuthenticationProviderTest {

	@Test
	void testSupportsUsernamePasswordAuthentication() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertTrue(provider.supports(UsernamePasswordAuthenticationToken.class));
	}

	@Test
	void testSupportsArbitraryClass() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertTrue(provider.supports(Object.class));
	}

	@Test
	void testGetHashedPasswordSqlReturnsNullByDefault() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertNull(provider.getHashedPasswordSql());
	}

	@Test
	void testSetHashedPasswordSqlRoundTrips() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		provider.setHashedPasswordSql("select password from ADM_SecurityUser where userName = ?");
		assertEquals("select password from ADM_SecurityUser where userName = ?", provider.getHashedPasswordSql());
	}

	@Test
	void testAuthenticateWithUnsupportedHashingAlgorithmThrowsInternalAuthenticationServiceException() {
		String originalAlgorithm = UtilImpl.PASSWORD_HASHING_ALGORITHM;
		try {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = "not-a-message-digest";
			SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
			UsernamePasswordAuthenticationToken token = new UsernamePasswordAuthenticationToken("user", "password");

			assertThrows(InternalAuthenticationServiceException.class, () -> provider.authenticate(token));
		}
		finally {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = originalAlgorithm;
		}
	}

	@Test
	void testNewInstanceIsNotNull() {
		assertNotNull(new SkyveLegacyAuthenticationProvider());
	}
}

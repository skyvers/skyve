package org.skyve.impl.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

@SuppressWarnings("static-method")
public class SkyveLegacyAuthenticationProviderTest {

	@Test
	public void testSupportsUsernamePasswordAuthentication() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertTrue(provider.supports(UsernamePasswordAuthenticationToken.class));
	}

	@Test
	public void testSupportsArbitraryClass() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertTrue(provider.supports(Object.class));
	}

	@Test
	public void testGetHashedPasswordSqlReturnsNullByDefault() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		assertNull(provider.getHashedPasswordSql());
	}

	@Test
	public void testSetHashedPasswordSqlRoundTrips() {
		SkyveLegacyAuthenticationProvider provider = new SkyveLegacyAuthenticationProvider();
		provider.setHashedPasswordSql("select password from ADM_SecurityUser where userName = ?");
		assertEquals("select password from ADM_SecurityUser where userName = ?", provider.getHashedPasswordSql());
	}

	@Test
	public void testAuthenticateWithUnsupportedHashingAlgorithmThrowsInternalAuthenticationServiceException() {
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
	public void testNewInstanceIsNotNull() {
		assertNotNull(new SkyveLegacyAuthenticationProvider());
	}
}

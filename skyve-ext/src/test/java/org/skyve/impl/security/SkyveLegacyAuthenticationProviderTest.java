package org.skyve.impl.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
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
	public void testNewInstanceIsNotNull() {
		assertNotNull(new SkyveLegacyAuthenticationProvider());
	}
}

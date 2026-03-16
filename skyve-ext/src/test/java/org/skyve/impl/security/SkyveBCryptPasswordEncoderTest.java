package org.skyve.impl.security;

import org.junit.Test;

public class SkyveBCryptPasswordEncoderTest {

	private final SkyveBCryptPasswordEncoder encoder = new SkyveBCryptPasswordEncoder();

	@Test(expected = IllegalArgumentException.class)
	public void testEncodeNullRawPassword() {
		encoder.encode(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMatchesNullRawPassword() {
		encoder.matches(null, "$2a$10$9u8BUQ4U8D6Nq3OGk7zSOu3jb0AmmPud95PZwQ0ZfYvFuG0j0rL9S");
	}
}

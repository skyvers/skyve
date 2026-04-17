package org.skyve.impl.security;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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

	@Test
	public void testEncodeAndMatchesWithPasswordLongerThan72Utf8Bytes() {
		// Create a password of 80 ASCII characters (80 UTF-8 bytes, >72-byte limit).
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < 80; i++) {
			sb.append('a');
		}
		String longPassword = sb.toString();
		String encoded = encoder.encode(longPassword);
		// The encoder should successfully match the original long password.
		assertTrue("Encoded password should match original long password",
				encoder.matches(longPassword, encoded));
		// Since the encoder only considers the first 72 bytes, the first 72 characters
		// (all ASCII) should also match the same encoded value.
		String first72Chars = longPassword.substring(0, 72);
		assertTrue("Encoded password should match first 72 characters of the password",
				encoder.matches(first72Chars, encoded));
	}

	@Test
	public void testTruncationAt72Utf8BytesWithMultiByteCharacter() {
		// Build a base password of 71 ASCII characters (71 UTF-8 bytes).
		StringBuilder baseBuilder = new StringBuilder();
		for (int i = 0; i < 71; i++) {
			baseBuilder.append('a');
		}
		String basePassword = baseBuilder.toString();
		// Append a multi-byte UTF-8 character ('€' is 3 bytes in UTF-8), so the total
		// length in bytes exceeds 72. The encoder should truncate at 72 bytes, so
		// the '€' character should be completely ignored.
		String multiBytePassword = basePassword + "€";
		String encoded = encoder.encode(multiBytePassword);
		// The encoded value should match when using only the first 71 ASCII characters,
		// demonstrating that the multi-byte character beyond the 72-byte boundary
		// does not affect the hash.
		assertTrue("Encoded password should match base password without multi-byte suffix",
				encoder.matches(basePassword, encoded));
		// Changing a character within the first 72 bytes must result in a non-matching
		// password.
		StringBuilder differentPrefixBuilder = new StringBuilder();
		differentPrefixBuilder.append('b');
		for (int i = 0; i < 70; i++) {
			differentPrefixBuilder.append('a');
		}
		String differentPrefixPassword = differentPrefixBuilder.toString();
		assertFalse("Password differing within the first 72 bytes should not match",
				encoder.matches(differentPrefixPassword, encoded));
	}

	@Test
	public void testEncodeAndMatchesForPasswordExceeding72Bytes() {
		// 71 ASCII chars + 'é' (2 UTF-8 bytes) = 73 bytes; exceeds the 72-byte bcrypt limit
		String password = "a".repeat(71) + "\u00e9";
		String encoded = encoder.encode(password);
		assertTrue(encoder.matches(password, encoded));
	}

	@Test
	public void testTruncationAtBoundary() {
		// Two passwords sharing the same first 72 bytes should produce matching hashes
		String prefix = "a".repeat(72); // exactly 72 bytes
		String passwordA = prefix + "extraA";
		String passwordB = prefix + "extraB";
		String encodedA = encoder.encode(passwordA);
		assertTrue("Passwords with identical 72-byte prefix must match after truncation",
				encoder.matches(passwordB, encodedA));
	}

	@Test
	public void testDistinctPasswordsUnder72BytesDoNotMatch() {
		// Ensure passwords shorter than 72 bytes are not conflated
		String encodedA = encoder.encode("passwordA");
		assertFalse(encoder.matches("passwordB", encodedA));
	}

	@Test
	public void testMultiByteCharacterNotSplitAtBoundary() {
		// 70 ASCII chars + 'é' (2 bytes) = exactly 72 bytes; any suffix is truncated away
		String base = "a".repeat(70) + "\u00e9";
		String extended = base + "extra";
		String encodedBase = encoder.encode(base);
		assertTrue("Password truncated to same 72-byte prefix should match",
				encoder.matches(extended, encodedBase));
	}
}
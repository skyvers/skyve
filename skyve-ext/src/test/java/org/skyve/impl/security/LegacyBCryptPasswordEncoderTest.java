package org.skyve.impl.security;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class LegacyBCryptPasswordEncoderTest {

	private final LegacyBCryptPasswordEncoder encoder = new LegacyBCryptPasswordEncoder();

	@Test(expected = IllegalArgumentException.class)
	public void testEncodeNullRawPassword() {
		encoder.encode(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testMatchesNullRawPassword() {
		encoder.matches(null, "$2a$10$9u8BUQ4U8D6Nq3OGk7zSOu3jb0AmmPud95PZwQ0ZfYvFuG0j0rL9S");
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

        // ---- upgradeEncoding() ----

        @Test
        public void testUpgradeEncodingDelegatesToBCrypt() {
                // BCryptPasswordEncoder.upgradeEncoding() returns false for any standard BCrypt hash
                String encoded = encoder.encode("testpass");
                assertFalse(encoder.upgradeEncoding(encoded));
        }

        // ---- utf8ByteLength() 3-byte and 4-byte paths ----

        @Test
        public void testPasswordWith3ByteUtf8CharTruncatesCorrectly() {
                // U+4E2D (中) = 3 UTF-8 bytes.
                // 24 ASCII chars + 16 CJK chars = 24 + 48 = 72 bytes exactly; any suffix is truncated
                String base = "a".repeat(24) + "\u4E2D".repeat(16);
                String extended = base + "extra";
                String encodedBase = encoder.encode(base);
                assertTrue("Password truncated to same 72-byte prefix must match",
                                encoder.matches(extended, encodedBase));
        }

        @Test
        public void testPasswordWith4ByteUtf8CharTruncatesCorrectly() {
                // U+1F600 (😀) = 4 UTF-8 bytes.
                // 16 ASCII chars + 14 emoji = 16 + 56 = 72 bytes exactly; any suffix is truncated
                String base = "a".repeat(16) + "\uD83D\uDE00".repeat(14);
                String extended = base + "extra";
                String encodedBase = encoder.encode(base);
                assertTrue("Password truncated to same 72-byte prefix must match",
                                encoder.matches(extended, encodedBase));
        }
}

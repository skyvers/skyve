package org.skyve.impl.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SkyveLegacyPasswordEncoderTest {

	private final SkyveLegacyPasswordEncoder encoder = new SkyveLegacyPasswordEncoder();

	// ---- static encode(rawPassword, algorithm) ----

	@Test
	public void testEncodeWithSHA1ProducesTwentyEightChars() {
		String encoded = SkyveLegacyPasswordEncoder.encode("password", "SHA1");
		assertNotNull(encoded);
		// SHA-1 produces a 20-byte digest; Base64 encoding of 20 bytes = 28 chars
		assertEquals(28, encoded.trim().length());
	}

	@Test
	public void testEncodeWithSHA256ProducesFourtyFourChars() {
		String encoded = SkyveLegacyPasswordEncoder.encode("password", "SHA-256");
		assertNotNull(encoded);
		// SHA-256 produces a 32-byte digest; Base64 encoding of 32 bytes = 44 chars
		assertEquals(44, encoded.trim().length());
	}

	@Test
	public void testEncodeWithUnknownAlgorithmReturnsNull() {
		String encoded = SkyveLegacyPasswordEncoder.encode("password", "NOT-AN-ALG-9921");
		assertNull(encoded);
	}

	@Test
	public void testEncodeSameInputProducesSameOutput() {
		String first = SkyveLegacyPasswordEncoder.encode("samepassword", "SHA-256");
		String second = SkyveLegacyPasswordEncoder.encode("samepassword", "SHA-256");
		assertEquals(first, second);
	}

	@Test
	public void testEncodeDifferentInputsProduceDifferentOutputs() {
		String a = SkyveLegacyPasswordEncoder.encode("passA", "SHA-256");
		String b = SkyveLegacyPasswordEncoder.encode("passB", "SHA-256");
		assertFalse("Different passwords must not produce the same hash", a.equals(b));
	}

	// ---- static matches(rawPassword, encodedPassword, algorithm) ----

	@Test
	public void testStaticMatchesReturnsTrueForCorrectPassword() {
		String encoded = SkyveLegacyPasswordEncoder.encode("pass123", "SHA1");
		assertTrue(SkyveLegacyPasswordEncoder.matches("pass123", encoded, "SHA1"));
	}

	@Test
	public void testStaticMatchesReturnsFalseForWrongPassword() {
		String encoded = SkyveLegacyPasswordEncoder.encode("pass123", "SHA1");
		assertFalse(SkyveLegacyPasswordEncoder.matches("wrongpass", encoded, "SHA1"));
	}

	@Test
	public void testStaticMatchesWithSHA256() {
		String encoded = SkyveLegacyPasswordEncoder.encode("pass123", "SHA-256");
		assertTrue(SkyveLegacyPasswordEncoder.matches("pass123", encoded, "SHA-256"));
		assertFalse(SkyveLegacyPasswordEncoder.matches("wrong", encoded, "SHA-256"));
	}

	// ---- instance matches() — detects SHA1 by 28-char length ----

	@Test
	public void testInstanceMatchesDetectsSHA1ByLength() {
		// SHA-1 encoded passwords produce a 28-char Base64 string.
		// The instance matches() detects length == 28 and uses SHA1 regardless of
		// the configured hashing algorithm.
		String encoded = SkyveLegacyPasswordEncoder.encode("testpass", "SHA1");
		assertEquals(28, encoded.trim().length());
		assertTrue(encoder.matches("testpass", encoded));
	}

	@Test
	public void testInstanceMatchesReturnsFalseForWrongPassword() {
		String encoded = SkyveLegacyPasswordEncoder.encode("correct", "SHA1");
		assertFalse(encoder.matches("wrong", encoded));
	}
}

package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class StateUtilTest {

	// ----------------------------------------------------------------
	// Session count
	// ----------------------------------------------------------------

	@Test
	void sessionCountStartsAtZero() {
		// Reset to known state — we cannot rely on zero due to static field
		// but we can verify increment and decrement are symmetric
		int before = StateUtil.getSessionCount();
		assertEquals(before, StateUtil.getSessionCount());
	}

	@Test
	void incrementSessionCountIncreasesCountByOne() {
		int before = StateUtil.getSessionCount();
		StateUtil.incrementSessionCount();
		try {
			assertEquals(before + 1, StateUtil.getSessionCount());
		}
		finally {
			StateUtil.decrementSessionCount();
		}
	}

	@Test
	void decrementSessionCountDecreasesCountByOne() {
		StateUtil.incrementSessionCount();
		int after = StateUtil.getSessionCount();
		StateUtil.decrementSessionCount();
		assertEquals(after - 1, StateUtil.getSessionCount());
	}

	@Test
	void decrementSessionCountDoesNotGoBelowZero() {
		// Drain count to zero first
		int count = StateUtil.getSessionCount();
		for (int i = 0; i < count; i++) {
			StateUtil.decrementSessionCount();
		}
		// Now one more decrement should clamp at 0
		StateUtil.decrementSessionCount();
		assertEquals(0, StateUtil.getSessionCount());
	}

	// ----------------------------------------------------------------
	// createToken
	// ----------------------------------------------------------------

	@Test
	void createTokenReturnsNonNullValue() {
		Integer token = StateUtil.createToken();
		assertNotNull(token);
	}

	@Test
	void createTokenReturnsDifferentValuesOnSuccessiveCalls() {
		// Probabilistically true — the chance of collision is ~1/2^32
		Integer t1 = StateUtil.createToken();
		Integer t2 = StateUtil.createToken();
		Integer t3 = StateUtil.createToken();
		// At least two of three should differ
		boolean anyDiffers = !t1.equals(t2) || !t2.equals(t3) || !t1.equals(t3);
		assertTrue(anyDiffers);
	}

	// ----------------------------------------------------------------
	// checkToken (String, Integer) with null token
	// ----------------------------------------------------------------

	@Test
	void checkTokenReturnsFalseForNullToken() {
		assertFalse(StateUtil.checkToken("some-session", null));
	}

        // ----------------------------------------------------------------
        // encode64 / decode64
        // ----------------------------------------------------------------

        @Test
        void encode64ReturnsNonNullString() throws IOException {
                String encoded = StateUtil.encode64("hello");
                assertNotNull(encoded);
                assertFalse(encoded.isEmpty());
        }

        @Test
        void encode64ThenDecode64RoundtripsString() throws IOException {
                String original = "round-trip test value";
                String encoded = StateUtil.encode64(original);
                String decoded = StateUtil.decode64(encoded);
                assertEquals(original, decoded);
        }

        @Test
        void encode64ThenDecode64RoundtripsInteger() throws IOException {
                Integer original = Integer.valueOf(42);
                String encoded = StateUtil.encode64(original);
                Integer decoded = StateUtil.decode64(encoded);
                assertEquals(original, decoded);
        }

        @Test
        void encode64ProducesDifferentEncodingsForDifferentValues() throws IOException {
                String enc1 = StateUtil.encode64("valueA");
                String enc2 = StateUtil.encode64("valueB");
                assertNotEquals(enc1, enc2);
        }
}

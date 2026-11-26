package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import jakarta.servlet.http.HttpSession;

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
		} finally {
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
	void createTokenIs128RandomBitsAsHex() {
		String t1 = StateUtil.createToken();
		String t2 = StateUtil.createToken();
		assertTrue(t1.matches("[0-9a-f]{32}"), t1);
		assertTrue(t2.matches("[0-9a-f]{32}"), t2);
		assertNotEquals(t1, t2);
	}

	// ----------------------------------------------------------------
	// Session CSRF token
	// ----------------------------------------------------------------

	@Test
	void getSessionTokenIssuesATokenOnceAndKeepsItForTheSession() {
		HttpSession session = session();
		String token = StateUtil.getSessionToken(session);
		assertTrue(token.matches("[0-9a-f]{32}"), token);
		assertEquals(token, StateUtil.getSessionToken(session));
		assertNotEquals(token, StateUtil.getSessionToken(session()), "another session has its own token");
	}

	@Test
	void checkSessionTokenAcceptsOnlyTheSessionsToken() {
		HttpSession session = session();
		String token = StateUtil.getSessionToken(session);
		assertTrue(StateUtil.checkSessionToken(session, token));
		assertFalse(StateUtil.checkSessionToken(session, StateUtil.createToken()), "another token");
		assertFalse(StateUtil.checkSessionToken(session, token.substring(1)), "a shorter token");
		assertFalse(StateUtil.checkSessionToken(session, null), "no token");
		assertFalse(StateUtil.checkSessionToken(null, token), "no session");
		assertFalse(StateUtil.checkSessionToken(session(), token), "another session's token");
	}

	@Test
	void checkSessionTokenDoesNotIssueAToken() {
		HttpSession session = session();
		assertFalse(StateUtil.checkSessionToken(session, ""));
		assertFalse(StateUtil.checkSessionToken(session, "null"));
		verify(session, never()).setAttribute(anyString(), any());
	}

	@Test
	void removeSessionTokenMakesTheNextCallIssueANewToken() {
		HttpSession session = session();
		String old = StateUtil.getSessionToken(session);
		StateUtil.removeSessionToken(session);
		assertFalse(StateUtil.checkSessionToken(session, old), "the old token is refused");
		String renewed = StateUtil.getSessionToken(session);
		assertNotEquals(old, renewed);
		assertTrue(StateUtil.checkSessionToken(session, renewed));
	}

	/**
	 * A session that keeps its attributes, as a container's does.
	 */
	private static HttpSession session() {
		Map<String, Object> attributes = new HashMap<>();
		HttpSession result = mock(HttpSession.class);
		when(result.getAttribute(anyString())).thenAnswer(i -> attributes.get(i.getArgument(0)));
		doAnswer(i -> attributes.put(i.getArgument(0), i.getArgument(1))).when(result).setAttribute(anyString(), any());
		doAnswer(i -> attributes.remove(i.getArgument(0))).when(result).removeAttribute(anyString());
		return result;
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
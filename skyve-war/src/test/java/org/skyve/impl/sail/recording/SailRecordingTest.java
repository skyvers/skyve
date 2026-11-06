package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;

import jakarta.servlet.http.HttpSession;

/**
 * Tests the recording gate (configuration, role, started) and the session's recording: steps in order, resent steps
 * added once, the step cap, ranges, and stopping.
 */
@SuppressWarnings("static-method")
class SailRecordingTest {
	private boolean originalRecording;
	private int originalMaximum;

	@BeforeEach
	void allowRecording() {
		originalRecording = UtilImpl.SAIL_RECORDING;
		originalMaximum = UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS;
		UtilImpl.SAIL_RECORDING = true;
	}

	@AfterEach
	void restore() {
		UtilImpl.SAIL_RECORDING = originalRecording;
		UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS = originalMaximum;
	}

	static User user(boolean recorder) {
		User result = mock(User.class);
		when(result.getName()).thenReturn("admin");
		when(result.getCustomerName()).thenReturn("demo");
		when(result.getCustomer()).thenReturn(mock(org.skyve.metadata.customer.Customer.class));
		when(Boolean.valueOf(result.isInRole(SailRecording.ROLE_MODULE_NAME, SailRecording.ROLE_NAME))).thenReturn(Boolean.valueOf(recorder));
		return result;
	}

	/**
	 * A session mock that keeps its attributes.
	 */
	static HttpSession session() {
		Map<String, Object> attributes = new HashMap<>();
		HttpSession result = mock(HttpSession.class);
		doAnswer(i -> attributes.put(i.getArgument(0), i.getArgument(1))).when(result).setAttribute(anyString(), any());
		doAnswer(i -> attributes.remove(i.getArgument(0))).when(result).removeAttribute(anyString());
		when(result.getAttribute(anyString())).thenAnswer(i -> attributes.get(i.getArgument(0)));
		when(result.getId()).thenReturn("sail-recording-session-" + System.nanoTime());
		return result;
	}

	private static Map<String, Object> step(String id, String op) {
		Map<String, Object> result = new HashMap<>();
		result.put("id", id);
		result.put("op", op);
		return result;
	}

	@Test
	void theGateNeedsTheConfigurationTheRoleAndAStartedRecording() {
		User recorder = user(true);
		HttpSession session = session();
		assertFalse(SailRecording.isRecording(session, recorder), "not started");
		SailRecording recording = SailRecording.start(session, recorder);
		assertAll(() -> assertTrue(SailRecording.isAllowed(recorder)),
					() -> assertFalse(SailRecording.isAllowed(user(false)), "no role"),
					() -> assertFalse(SailRecording.isAllowed(null), "nobody signed in"),
					() -> assertTrue(SailRecording.isRecording(session, recorder)),
					() -> assertFalse(SailRecording.isRecording(session, user(false))),
					() -> assertFalse(SailRecording.isRecording(null, recorder)),
					() -> assertSame(recording, SailRecording.get(session)));
		UtilImpl.SAIL_RECORDING = false;
		assertAll(() -> assertFalse(SailRecording.isAllowed(recorder), "configuration off"),
					() -> assertFalse(SailRecording.isRecording(session, recorder)));
		UtilImpl.SAIL_RECORDING = true;
		recording.stop();
		assertFalse(SailRecording.isRecording(session, recorder), "stopped");
	}

	@Test
	void startingIsRefusedWhenNotAllowed() {
		HttpSession session = session();
		User user = user(false);
		assertThrows(SecurityException.class, () -> SailRecording.start(session, user));
		assertNull(SailRecording.get(session));
	}

	@Test
	void startingAgainReplacesTheRecording() {
		HttpSession session = session();
		SailRecording first = SailRecording.start(session, user(true));
		first.add(List.of(step("a", "save")));
		SailRecording second = SailRecording.start(session, user(true));
		assertAll(() -> assertSame(second, SailRecording.get(session)),
					() -> assertEquals(0, second.getStepCount()),
					() -> assertEquals("demo", second.getCustomerName()),
					() -> assertEquals("admin", second.getUserName()));
	}

	/**
	 * The recorder resends steps it has not seen acknowledged, so a step id is added once; anything that is not a
	 * step is skipped, and values are kept as text.
	 */
	@Test
	void stepsAreAddedOnceInOrderAsText() {
		SailRecording recording = SailRecording.start(session(), user(true));
		Map<String, Object> numbered = step("c", "listGridZoom");
		numbered.put("row", Integer.valueOf(2));
		assertEquals(2, recording.add(List.of(step("a", "save"), "not a step", step("b", "ok"))));
		assertEquals(1, recording.add(List.of(step("a", "save"), step("b", "ok"), numbered)));
		List<Map<String, String>> steps = recording.getSteps();
		assertAll(() -> assertEquals(3, steps.size()),
					() -> assertEquals("save", steps.get(0).get("op")),
					() -> assertEquals("ok", steps.get(1).get("op")),
					() -> assertEquals("2", steps.get(2).get("row")));
	}

	/**
	 * Past the cap the recording refuses steps and says so; it keeps what it has, and can still be stopped.
	 */
	@Test
	void theCapRefusesStepsAndSaysSo() {
		UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS = 2;
		SailRecording recording = SailRecording.start(session(), user(true));
		assertEquals(1, recording.add(List.of(step("a", "save"))));
		assertFalse(recording.isLimitReached());
		assertEquals(1, recording.add(List.of(step("b", "save"), step("c", "save"))));
		assertAll(() -> assertTrue(recording.isLimitReached()),
					() -> assertEquals(2, recording.getStepCount()),
					() -> assertNotNull(recording.stop()));
	}

	@Test
	void aRangeOfStepsIsClampedToTheSteps() {
		SailRecording recording = SailRecording.start(session(), user(true));
		recording.add(List.of(step("a", "save"), step("b", "ok"), step("c", "cancel")));
		assertAll(() -> assertEquals(List.of("ok", "cancel"), recording.getSteps(1, 50).stream().map(s -> s.get("op")).toList()),
					() -> assertEquals(List.of("save"), recording.getSteps(-3, 1).stream().map(s -> s.get("op")).toList()),
					() -> assertTrue(recording.getSteps(7, 5).isEmpty()),
					() -> assertTrue(recording.getSteps(1, -1).isEmpty()));
	}

	@Test
	void stoppingBuildsTheScriptOnceAndTakesNoMoreSteps() {
		SailRecording recording = SailRecording.start(session(), user(true));
		recording.add(List.of(step("a", "save")));
		assertNull(recording.getXml());
		String xml = recording.stop();
		assertAll(() -> assertTrue(recording.isStopped()),
					() -> assertTrue(xml.contains("<save/>"), xml),
					() -> assertSame(xml, recording.stop()),
					() -> assertSame(xml, recording.getXml()),
					() -> assertThrows(IllegalStateException.class, () -> recording.add(List.of(step("b", "ok")))));
	}
}

package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.JSON;
import org.skyve.web.WebContext;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

/**
 * Tests the recording endpoint: it answers only a session that is recording, issues a CSRF token, takes steps only
 * with that token, adds resent steps once, says when the cap is reached, and serves ranges and the stopped script.
 */
@SuppressWarnings({ "resource", "java:S5960" }) // test-only mocks and assertions
class SailRecordingServletH2Test extends AbstractSkyveTest {
	private boolean originalRecording;
	private int originalMaximum;
	private HttpSession session;

	@BeforeEach
	void recordingSession() {
		originalRecording = UtilImpl.SAIL_RECORDING;
		originalMaximum = UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS;
		UtilImpl.SAIL_RECORDING = true;
		session = SailRecordingTest.session();
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		StateUtil.addSession(u.getId(), session);
	}

	@AfterEach
	void restore() {
		UtilImpl.SAIL_RECORDING = originalRecording;
		UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS = originalMaximum;
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void aSessionThatIsNotRecordingGetsNothing() throws Exception {
		Response get = get(Map.of());
		assertAll(() -> verify(get.response).setStatus(HttpServletResponse.SC_FORBIDDEN),
					() -> assertEquals("{\"recording\":false}", get.body()));
		SailRecording.start(session, u);
		UtilImpl.SAIL_RECORDING = false;
		Response off = get(Map.of());
		verify(off.response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	@Test
	void stepsNeedTheTokenAndAreAddedOnce() throws Exception {
		SailRecording recording = SailRecording.start(session, u);
		String token = token();
		String batch = "{\"_csrf\":" + token + ",\"steps\":[{\"id\":\"p-1\",\"op\":\"save\"},{\"id\":\"p-2\",\"op\":\"ok\"}]}";
		assertEquals("{\"received\":2,\"steps\":2}", post(batch).body());
		assertEquals("{\"received\":0,\"steps\":2}", post(batch).body());

		Response forged = post("{\"_csrf\":12345,\"steps\":[{\"id\":\"p-3\",\"op\":\"save\"}]}");
		Response none = post("{\"steps\":[{\"id\":\"p-3\",\"op\":\"save\"}]}");
		Response garbage = post("not json");
		assertAll(() -> verify(forged.response).sendError(HttpServletResponse.SC_FORBIDDEN),
					() -> verify(none.response).sendError(HttpServletResponse.SC_FORBIDDEN),
					() -> verify(garbage.response).sendError(HttpServletResponse.SC_FORBIDDEN),
					() -> assertEquals(2, recording.getStepCount()));
	}

	@Test
	void theCapIsReported() throws Exception {
		UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS = 1;
		SailRecording.start(session, u);
		String body = post("{\"_csrf\":" + token() + ",\"steps\":[{\"id\":\"p-1\",\"op\":\"save\"},{\"id\":\"p-2\",\"op\":\"ok\"}]}").body();
		assertEquals("{\"received\":1,\"steps\":1,\"limitReached\":true,\"maximumSteps\":1}", body);
	}

	@Test
	void aStoppedRecordingTakesNoStepsAndServesItsScript() throws Exception {
		SailRecording recording = SailRecording.start(session, u);
		String token = token();
		post("{\"_csrf\":" + token + ",\"steps\":[{\"id\":\"p-1\",\"op\":\"save\"}]}");
		Response early = get(Map.of("format", "xml"));
		verify(early.response).sendError(HttpServletResponse.SC_NOT_FOUND);

		String xml = recording.stop();
		Response late = post("{\"_csrf\":" + token + ",\"steps\":[{\"id\":\"p-2\",\"op\":\"ok\"}]}");
		Response script = get(Map.of("format", "xml"));
		assertAll(() -> verify(late.response).setStatus(HttpServletResponse.SC_FORBIDDEN),
					() -> assertEquals(xml, script.body()),
					() -> assertEquals(1, recording.getStepCount()));
	}

	@Test
	void aRangeOfStepsIsServed() throws Exception {
		SailRecording.start(session, u);
		post("{\"_csrf\":" + token() + ",\"steps\":[{\"id\":\"p-1\",\"op\":\"save\"},{\"id\":\"p-2\",\"op\":\"ok\"},{\"id\":\"p-3\",\"op\":\"cancel\"}]}");
		String body = get(Map.of("from", "1", "count", "1")).body();
		assertTrue(body.startsWith("{\"total\":3,\"from\":1,\"steps\":["), body);
		assertTrue(body.contains("\"op\":\"ok\"") && (! body.contains("\"op\":\"save\"")), body);
		Response bad = get(Map.of("from", "x"));
		verify(bad.response).sendError(HttpServletResponse.SC_BAD_REQUEST);
	}

	/**
	 * Asks for the page's token, as the recorder does when it loads.
	 */
	private String token() throws Exception {
		String body = get(Map.of()).body();
		Map<?, ?> state = (Map<?, ?>) JSON.unmarshall(body);
		assertEquals(Boolean.TRUE, state.get("recording"), body);
		return String.valueOf(state.get("_csrf"));
	}

	private Response get(Map<String, String> parameters) throws IOException {
		HttpServletRequest request = request(parameters, null);
		Response result = new Response();
		new TestableServlet().doGet(request, result.response);
		AbstractPersistence.get().setUser(u);
		return result;
	}

	private Response post(String body) throws IOException {
		HttpServletRequest request = request(Map.of(), body);
		Response result = new Response();
		new TestableServlet().doPost(request, result.response);
		AbstractPersistence.get().setUser(u);
		return result;
	}

	private HttpServletRequest request(Map<String, String> parameters, String body) throws IOException {
		HttpServletRequest result = mock(HttpServletRequest.class);
		when(result.getSession(false)).thenReturn(session);
		when(result.getSession()).thenReturn(session);
		when(result.getUserPrincipal()).thenReturn((Principal) null);
		when(result.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		for (Map.Entry<String, String> entry : parameters.entrySet()) {
			when(result.getParameter(entry.getKey())).thenReturn(entry.getValue());
		}
		if (body != null) {
			ByteArrayInputStream in = new ByteArrayInputStream(body.getBytes(StandardCharsets.UTF_8));
			when(result.getInputStream()).thenReturn(new ServletInputStream() {
				@Override
				public int read() {
					return in.read();
				}

				@Override
				public boolean isFinished() {
					return in.available() == 0;
				}

				@Override
				public boolean isReady() {
					return true;
				}

				@Override
				public void setReadListener(ReadListener listener) {
					// not asynchronous
				}
			});
		}
		return result;
	}

	private static final class Response {
		private final StringWriter sink = new StringWriter();
		private final HttpServletResponse response = mock(HttpServletResponse.class);

		private Response() throws IOException {
			when(response.getWriter()).thenReturn(new PrintWriter(sink, true));
		}

		private String body() {
			return sink.toString();
		}
	}

	private static final class TestableServlet extends SailRecordingServlet {
		private static final long serialVersionUID = 1L;

		@Override
		public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
			super.doGet(request, response);
		}

		@Override
		public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
			super.doPost(request, response);
		}
	}
}

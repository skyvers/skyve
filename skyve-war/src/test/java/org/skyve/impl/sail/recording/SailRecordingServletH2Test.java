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
	 * Every page of a recording gets the same token, so the session's tokens do not grow with the pages; a new
	 * recording replaces it.
	 */
	@Test
	void theTokenIsIssuedOncePerRecording() throws Exception {
		SailRecording.start(session, u);
		String first = token();
		assertEquals(first, token());
		SailRecording.start(session, u);
		String second = token();
		assertAll(() -> assertTrue(! first.equals(second), first),
					() -> assertTrue(! StateUtil.checkToken(session, Integer.valueOf(first)), "the old token is gone"),
					() -> assertTrue(StateUtil.checkToken(session, Integer.valueOf(second))));
	}

	/**
	 * Whenever recording is on, a signed-in user (no SAILRecorder role, no recording) gets their own effective roles,
	 * with the roles of their groups.
	 */
	@Test
	void theRolesAreTheSignedInUsersWithTheirGroups() throws Exception {
		org.skyve.metadata.user.User grouped = groupedUser();
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, grouped);
		StateUtil.addSession(grouped.getId(), session);
		String body = get(Map.of("roles", "")).body();
		Map<?, ?> answer = (Map<?, ?>) java.util.Objects.requireNonNull(JSON.unmarshall(body), body);
		java.util.List<?> roles = (java.util.List<?>) answer.get("roles");
		assertAll(() -> assertTrue(roles.contains("admin.BasicUser"), "from the group: " + body),
					() -> assertTrue(roles.contains("admin.AppUser"), "direct: " + body),
					() -> assertTrue(! roles.contains("admin.DevOps"), body),
					() -> assertTrue(! roles.contains("admin.SAILRecorder"), body),
					() -> assertEquals(grouped.getName(), answer.get("user")));

		UtilImpl.SAIL_RECORDING = false;
		Response off = get(Map.of("roles", ""));
		verify(off.response).sendError(HttpServletResponse.SC_FORBIDDEN);
	}

	/**
	 * A user with the role admin.AppUser directly and admin.BasicUser through a group.
	 */
	private static org.skyve.metadata.user.User groupedUser() {
		AbstractPersistence persistence = AbstractPersistence.get();
		long suffix = System.nanoTime();
		modules.admin.Group.GroupExtension group = new org.skyve.util.DataBuilder().fixture(org.skyve.util.test.SkyveFixture.FixtureType.crud)
																.build(modules.admin.domain.Group.MODULE_NAME, modules.admin.domain.Group.DOCUMENT_NAME);
		group.setName("SAIL group " + suffix);
		group.getRoles().clear();
		modules.admin.domain.GroupRole groupRole = modules.admin.domain.GroupRole.newInstance();
		groupRole.setRoleName("admin.BasicUser");
		group.addRolesElement(groupRole);
		group = persistence.save(group);

		modules.admin.User.UserExtension user = new org.skyve.util.DataBuilder().fixture(org.skyve.util.test.SkyveFixture.FixtureType.crud)
																.build(modules.admin.domain.User.MODULE_NAME, modules.admin.domain.User.DOCUMENT_NAME);
		user.setUserName("sail.grouped." + suffix);
		user.getGroups().clear();
		user.getRoles().clear();
		user.addGroupsElement(group);
		modules.admin.domain.UserRole userRole = modules.admin.domain.UserRole.newInstance();
		userRole.setRoleName("admin.AppUser");
		user.addRolesElement(userRole);
		user = persistence.save(user);
		return java.util.Objects.requireNonNull(user.toMetaDataUser());
	}

	/**
	 * Asks for the page's token, as the recorder does when it loads.
	 */
	private String token() throws Exception {
		String body = get(Map.of()).body();
		Map<?, ?> state = (Map<?, ?>) java.util.Objects.requireNonNull(JSON.unmarshall(body), body);
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

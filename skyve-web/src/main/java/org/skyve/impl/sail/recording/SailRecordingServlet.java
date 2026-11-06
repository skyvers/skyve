package org.skyve.impl.sail.recording;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.skyve.content.MimeType;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Receives the steps the browser recorder records, for the session's {@link SailRecording}.
 *
 * <ul>
 * <li>{@code GET} answers whether the session is recording: {@code {"recording":true,"_csrf":token,"steps":n}}, or
 * 		status 403 with {@code {"recording":false}}. The token is a Skyve CSRF token ({@link StateUtil#createToken()}),
 * 		issued once per recording ({@link SailRecording#token(HttpSession)}) and returned to every page that asks, so the
 * 		session's tokens do not grow with the pages.</li>
 * <li>{@code GET} with {@code roles} returns the signed-in user's effective roles, {@code {"user":name,"roles":[...]}},
 * 		whenever {@code sail.recording} is on: the SAIL harness builds its views with them. It needs no recording and no
 * 		SAILRecorder role; status 403 when recording is off or nobody is signed in.</li>
 * <li>{@code GET} with {@code from} (and optionally {@code count}, default {@value #DEFAULT_RANGE_COUNT}) returns a
 * 		range of the recorded entries as the recorder's panel shows them, recording or stopped:
 * 		{@code {"total":n,"gestures":g,"stopped":b,"from":i,"entries":[{"id","context","lines":[...]}]}}.</li>
 * <li>{@code GET} with {@code format=xml} returns the script: the one built when the recording stopped (the same XML
 * 		the Control Panel downloads), or while recording, one built now without the metadata checks. With
 * 		{@code download}, as an attachment. Status 404 if there is no recording.</li>
 * <li>{@code POST} with a JSON body {@code {"_csrf":token,"steps":[...]}} adds the steps (see
 * 		{@link SailRecording#add(List)}) and answers {@code {"received":added,"steps":total}}, with
 * 		{@code "limitReached":true,"maximumSteps":cap} once the recording is full. The batch can also carry
 * 		{@code "delete":id}, {@code "undo":true} (the last gesture) and {@code "stop":true} (stop, with the metadata
 * 		checks), applied in that order after its steps; the answer then also has {@code gestures} and {@code stopped}. A token the session did
 * 		not issue is refused with status 403. The token stays valid for the recording, so that the recorder can resend
 * 		steps from a beacon as the page unloads.</li>
 * </ul>
 * Every request but the roles is checked against the whole gate ({@link SailRecording#isRecording(HttpSession, User)}, or for the
 * script, {@link SailRecording#isAllowed(User)}): a session that is not recording gets status 403 and nothing else.
 *
 * <p>Threading: thread-safe; the servlet holds no state.
 */
public class SailRecordingServlet extends HttpServlet {
	private static final long serialVersionUID = 6436209818571040713L;

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SailRecordingServlet.class);

	/** The most a batch's body may hold. */
	static final int MAXIMUM_BODY_BYTES = 1024 * 1024;
	/** The parameter that asks a GET for the stopped recording's script. */
	static final String FORMAT_PARAMETER = "format";
	/** The {@value #FORMAT_PARAMETER} that asks for the script. */
	static final String XML_FORMAT = "xml";
	static final String STEPS_KEY = "steps";
	/** The parameter that asks a GET for a range of the recorded steps, from this zero-based index. */
	static final String FROM_PARAMETER = "from";
	/** The parameter that limits a range of steps; see {@link #FROM_PARAMETER}. */
	static final String COUNT_PARAMETER = "count";
	/** With {@value #FORMAT_PARAMETER}{@code =xml}, asks for the script as a file to save. */
	static final String DOWNLOAD_PARAMETER = "download";
	/** A batch key: the id of an entry to delete. */
	static final String DELETE_KEY = "delete";
	/** A batch key: true to undo the last gesture. */
	static final String UNDO_KEY = "undo";
	/** A batch key: true to stop recording (after adding the batch's steps). */
	static final String STOP_KEY = "stop";
	/** The parameter that asks a GET for the signed-in user's effective roles. */
	static final String ROLES_PARAMETER = "roles";
	/** The steps a range has when it does not say. */
	static final int DEFAULT_RANGE_COUNT = 50;
	private static final String NOT_RECORDING = "{\"recording\":false}";

	/**
	 * Answers whether the session is recording, with a CSRF token, or returns the stopped recording's script.
	 */
	@Override
	@SuppressWarnings("java:S1989") // there exists JavaEE error pages
	protected void doGet(@SuppressWarnings("null") @Nonnull HttpServletRequest request,
							@SuppressWarnings("null") @Nonnull HttpServletResponse response)
	throws IOException {
		noCache(response);
		HttpSession session = request.getSession(false);
		User user = user(request);
		if (request.getParameter(ROLES_PARAMETER) != null) {
			writeRoles(user, response);
			return;
		}
		String from = request.getParameter(FROM_PARAMETER);
		if (from != null) {
			writeRange(session, user, from, request.getParameter(COUNT_PARAMETER), response);
			return;
		}
		if (XML_FORMAT.equals(request.getParameter(FORMAT_PARAMETER))) {
			SailRecording recording = SailRecording.isAllowed(user) ? SailRecording.get(session) : null;
			if (recording == null) {
				response.sendError(HttpServletResponse.SC_NOT_FOUND);
				return;
			}
			String xml = recording.currentXml();
			response.setContentType(MimeType.xml.toString());
			response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			if (request.getParameter(DOWNLOAD_PARAMETER) != null) {
				response.setHeader("Content-Disposition", "attachment; filename=\"" + recording.fileName() + '"');
			}
			try (PrintWriter pw = response.getWriter()) {
				pw.print(xml);
			}
			return;
		}

		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		if ((session == null) || (! SailRecording.isRecording(session, user))) {
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			try (PrintWriter pw = response.getWriter()) {
				pw.print(NOT_RECORDING);
			}
			return;
		}
		SailRecording recording = SailRecording.get(session);
		if (recording == null) { // ended since the gate was checked
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			try (PrintWriter pw = response.getWriter()) {
				pw.print(NOT_RECORDING);
			}
			return;
		}
		Integer token = recording.token(session);
		try (PrintWriter pw = response.getWriter()) {
			pw.append("{\"recording\":true,\"").append(AbstractWebContext.CSRF_TOKEN_NAME).append("\":").append(token.toString());
			pw.append(",\"steps\":").append(String.valueOf(recording.getStepCount())).append('}');
		}
	}

	/**
	 * Adds a batch of recorded steps to the session's recording.
	 */
	@Override
	@SuppressWarnings("java:S1989") // there exists JavaEE error pages
	protected void doPost(@SuppressWarnings("null") @Nonnull HttpServletRequest request,
							@SuppressWarnings("null") @Nonnull HttpServletResponse response)
	throws IOException {
		noCache(response);
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());

		HttpSession session = request.getSession(false);
		User user = user(request);
		SailRecording recording = SailRecording.get(session);
		if ((session == null) || (recording == null) || (! SailRecording.isRecording(session, user))) {
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			try (PrintWriter pw = response.getWriter()) {
				pw.print(NOT_RECORDING);
			}
			return;
		}

		byte[] body;
		try (InputStream in = request.getInputStream()) {
			body = in.readNBytes(MAXIMUM_BODY_BYTES + 1);
		}
		if (body.length > MAXIMUM_BODY_BYTES) {
			response.sendError(HttpServletResponse.SC_REQUEST_ENTITY_TOO_LARGE);
			return;
		}
		Map<?, ?> batch = batch(new String(body, StandardCharsets.UTF_8));
		if ((batch == null) || (! StateUtil.checkToken(session, token(batch.get(AbstractWebContext.CSRF_TOKEN_NAME))))) {
			LOGGER.warn("SAIL recording batch refused for session {}: no valid CSRF token", session.getId());
			response.sendError(HttpServletResponse.SC_FORBIDDEN);
			return;
		}
		Object steps = batch.get(STEPS_KEY);
		int added;
		try {
			added = (steps instanceof List<?> list) ? recording.add(list) : 0;
			Object delete = batch.get(DELETE_KEY);
			if (delete != null) {
				recording.delete(delete.toString());
			}
			if (Boolean.TRUE.equals(batch.get(UNDO_KEY))) {
				recording.undo();
			}
			if (Boolean.TRUE.equals(batch.get(STOP_KEY))) {
				stop(recording, user);
			}
		}
		catch (@SuppressWarnings("unused") IllegalStateException e) { // stopped since the gate was checked
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			try (PrintWriter pw = response.getWriter()) {
				pw.print(NOT_RECORDING);
			}
			return;
		}
		try (PrintWriter pw = response.getWriter()) {
			pw.append("{\"received\":").append(String.valueOf(added));
			pw.append(",\"steps\":").append(String.valueOf(recording.getStepCount()));
			pw.append(",\"gestures\":").append(String.valueOf(recording.getGestureCount()));
			pw.append(",\"stopped\":").append(String.valueOf(recording.isStopped()));
			if (recording.isLimitReached()) {
				pw.append(",\"limitReached\":true,\"maximumSteps\":").append(String.valueOf(UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS));
			}
			pw.append('}');
		}
	}

	/**
	 * Writes a range of the session's recorded steps as {@code {"total":n,"from":i,"steps":[...]}}, recording or
	 * stopped; status 404 if there is no recording, 400 if the range is not numbers.
	 */
	private static void writeRange(@Nullable HttpSession session,
									@Nullable User user,
									@Nonnull String from,
									@Nullable String count,
									@Nonnull HttpServletResponse response)
	throws IOException {
		SailRecording recording = SailRecording.isAllowed(user) ? SailRecording.get(session) : null;
		if (recording == null) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}
		int start;
		int length;
		try {
			start = Integer.parseInt(from.trim());
			length = (count == null) ? DEFAULT_RANGE_COUNT : Integer.parseInt(count.trim());
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			response.sendError(HttpServletResponse.SC_BAD_REQUEST);
			return;
		}
		List<Map<String, Object>> entries = recording.preview(start, length);
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		try (PrintWriter pw = response.getWriter()) {
			pw.append("{\"total\":").append(String.valueOf(recording.getStepCount()));
			pw.append(",\"gestures\":").append(String.valueOf(recording.getGestureCount()));
			pw.append(",\"stopped\":").append(String.valueOf(recording.isStopped()));
			pw.append(",\"from\":").append(String.valueOf(Math.max(0, Math.min(start, recording.getStepCount()))));
			pw.append(",\"entries\":").append(JSON.marshall(entries)).append('}');
		}
	}

	/**
	 * Stops the recording from the recorder's panel, checking the steps as the Control Panel's Stop does: as the
	 * signed-in user, in a transaction of its own.
	 */
	private static void stop(@Nonnull SailRecording recording, @Nullable User user) {
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			persistence.begin();
			if (user != null) {
				persistence.setUser(user);
			}
			recording.stop(new SailRecordingChecks(recording.getUxui(), recording.getUserAgentType()));
		}
		finally {
			persistence.commit(true);
		}
	}

	/**
	 * Writes the signed-in user's effective roles as {@code {"user":name,"roles":["module.Role",...]}}, for the SAIL
	 * harness to build its views with (see docs/sail.md, The test user). Allowed whenever {@code sail.recording} is on
	 * and a user is signed in: the user needs no SAILRecorder role, and sees only their own roles. Status 403 otherwise.
	 */
	private static void writeRoles(@Nullable User user, @Nonnull HttpServletResponse response) throws IOException {
		if ((! UtilImpl.SAIL_RECORDING) || (user == null)) {
			response.sendError(HttpServletResponse.SC_FORBIDDEN);
			return;
		}
		Map<String, Object> roles = new java.util.LinkedHashMap<>();
		roles.put("user", user.getName());
		roles.put(ROLES_PARAMETER, SailRecording.effectiveRoleNames(user)); // the JSON key is named as the parameter
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		try (PrintWriter pw = response.getWriter()) {
			pw.print(JSON.marshall(roles));
		}
	}

	private static void noCache(@Nonnull HttpServletResponse response) {
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
	}

	/**
	 * Returns the signed-in user, as Skyve's other servlets find it.
	 *
	 * @return the user, or {@code null} if nobody is signed in or the session has ended
	 */
	private static @Nullable User user(@Nonnull HttpServletRequest request) {
		Principal principal = request.getUserPrincipal();
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			persistence.begin();
			return WebUtil.processUserPrincipalForRequest(request, (principal == null) ? null : principal.getName());
		}
		catch (Exception e) {
			LOGGER.warn("SAIL recording could not read the signed in user", e);
			persistence.rollback();
			return null;
		}
		finally {
			persistence.commit(true);
		}
	}

	/**
	 * Parses a batch's JSON body.
	 *
	 * @return the batch, or {@code null} if the body is not a JSON object
	 */
	private static @Nullable Map<?, ?> batch(@Nonnull String body) {
		try {
			return (JSON.unmarshall(body) instanceof Map<?, ?> map) ? map : null;
		}
		catch (Exception e) {
			LOGGER.warn("SAIL recording batch is not JSON: {}", e.toString());
			return null;
		}
	}

	/**
	 * Returns a CSRF token sent as a JSON number or string.
	 */
	private static @Nullable Integer token(@Nullable Object value) {
		if (value instanceof Number number) {
			return Integer.valueOf(number.intValue());
		}
		if (value instanceof String string) {
			try {
				return Integer.valueOf(string.trim());
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				return null;
			}
		}
		return null;
	}
}

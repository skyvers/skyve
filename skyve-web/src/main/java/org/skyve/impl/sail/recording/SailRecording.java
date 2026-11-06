package org.skyve.impl.sail.recording;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpSession;

/**
 * A SAIL recording of one HTTP session, and the gate that decides whether a session records.
 *
 * <p>A session records when all three parts of the gate pass: the configuration allows recording
 * ({@code sail.recording}, {@link UtilImpl#SAIL_RECORDING}), the user has the {@code admin.SAILRecorder} role, and
 * recording was started for the session (the Control Panel's Start, {@link #start(HttpSession, User)}). When any part
 * fails, the server sends no recorder code: see {@link #isRecording(HttpSession, User)}.
 *
 * <p>The recording lives only in the session, under {@link #SESSION_ATTRIBUTE_NAME}. It is never written to the
 * database, and it ends with the session. The browser recorder sends its steps to {@link SailRecordingServlet}, which
 * adds them with {@link #add(List)}. Stopping builds the script once, with {@link SailRecordingBuilder}; the Control
 * Panel then offers it for download.
 *
 * <p>Threading: thread-safe. The recorder's requests and the Control Panel's actions can arrive at the same time;
 * the steps, the step ids and the built script are guarded by this object's monitor.
 */
public final class SailRecording implements Serializable {
	private static final long serialVersionUID = -2381796113574580742L;

	/** The session attribute that holds the session's recording. */
	public static final String SESSION_ATTRIBUTE_NAME = "skyve.sail.recording";
	/** The module of the role that may record. */
	public static final String ROLE_MODULE_NAME = "admin";
	/** The role that may record, in {@link #ROLE_MODULE_NAME}. */
	public static final String ROLE_NAME = "SAILRecorder";
	/** The SmartClient recorder bundle, relative to the web context. */
	public static final String SMART_CLIENT_SCRIPT = "skyve/sail/recorder-sc-min.js";
	/** The key of a recorded step's unique id; see {@link #add(List)}. */
	public static final String STEP_ID_KEY = "id";

	private final String customerName;
	private final String userName;
	private final long startedMillis;
	/** The recorded steps in the order received: each an op-code and its attributes. Guarded by this. */
	private final ArrayList<LinkedHashMap<String, String>> steps = new ArrayList<>();
	/** The ids of the steps received, so that a step resent by the recorder is added once. Guarded by this. */
	private final HashSet<String> stepIds = new HashSet<>();
	/** True once a batch had more steps than the cap ({@link UtilImpl#SAIL_RECORDING_MAXIMUM_STEPS}) allowed. Guarded by this. */
	private boolean limitReached = false;
	/** When recording stopped, or 0 while it records. Guarded by this. */
	private long stoppedMillis = 0L;
	/** The script built when recording stopped. Guarded by this. */
	private String xml = null;

	private SailRecording(@Nullable String customerName, @Nonnull String userName, long startedMillis) {
		this.customerName = customerName;
		this.userName = userName;
		this.startedMillis = startedMillis;
	}

	/**
	 * Indicates whether the configuration and the user's roles allow recording: the first two parts of the gate.
	 *
	 * @param user	the user, or {@code null} when nobody is signed in
	 * @return true if {@code sail.recording} is on and the user has the {@code admin.SAILRecorder} role
	 */
	public static boolean isAllowed(@Nullable User user) {
		return UtilImpl.SAIL_RECORDING && (user != null) && user.isInRole(ROLE_MODULE_NAME, ROLE_NAME);
	}

	/**
	 * Returns the session's recording, recording or stopped.
	 *
	 * <p>Side effects: none; it does not create a session.
	 *
	 * @param session	the session, or {@code null}
	 * @return the recording, or {@code null} if the session has none
	 */
	public static @Nullable SailRecording get(@Nullable HttpSession session) {
		if (session == null) {
			return null;
		}
		Object result = session.getAttribute(SESSION_ATTRIBUTE_NAME);
		return (result instanceof SailRecording recording) ? recording : null;
	}

	/**
	 * Indicates whether the whole gate passes: recording is allowed for the user and has been started, and not
	 * stopped, in the session. Pages add the recorder's script only when this is true.
	 *
	 * @param session	the request's session, or {@code null}
	 * @param user	the signed-in user, or {@code null}
	 * @return true if the session is recording
	 */
	public static boolean isRecording(@Nullable HttpSession session, @Nullable User user) {
		if (! isAllowed(user)) {
			return false;
		}
		SailRecording recording = get(session);
		return (recording != null) && (! recording.isStopped());
	}

	/**
	 * Starts a new recording in the session, replacing any earlier one (and the script it built).
	 *
	 * <p>Side effects: sets the session attribute {@link #SESSION_ATTRIBUTE_NAME}.
	 *
	 * @param session	the session to record
	 * @param user	the signed-in user
	 * @return the new recording
	 * @throws SecurityException if the configuration or the user's roles do not allow recording
	 */
	public static @Nonnull SailRecording start(@Nonnull HttpSession session, @Nonnull User user) {
		if (! isAllowed(user)) {
			throw new SecurityException("SAIL recording is not allowed for " + user.getName());
		}
		String name = user.getName();
		SailRecording result = new SailRecording(user.getCustomerName(), (name == null) ? "" : name, System.currentTimeMillis());
		session.setAttribute(SESSION_ATTRIBUTE_NAME, result);
		return result;
	}

	/**
	 * Returns the customer of the user who started the recording.
	 *
	 * @return the customer name, or {@code null}
	 */
	public @Nullable String getCustomerName() {
		return customerName;
	}

	/**
	 * Returns the name of the user who started the recording.
	 *
	 * @return the user name
	 */
	public @Nonnull String getUserName() {
		return userName;
	}

	/**
	 * Returns when the recording started.
	 *
	 * @return the time in milliseconds since the epoch
	 */
	public long getStartedMillis() {
		return startedMillis;
	}

	/**
	 * Indicates whether the recording has stopped.
	 *
	 * @return true once {@link #stop()} has been called
	 */
	public synchronized boolean isStopped() {
		return (stoppedMillis != 0L);
	}

	/**
	 * Returns the number of steps recorded so far.
	 *
	 * @return the step count
	 */
	public synchronized int getStepCount() {
		return steps.size();
	}

	/**
	 * Returns a copy of the recorded steps, in the order received.
	 *
	 * @return a new list of new maps, each an op-code ({@code op}) and its attributes as text
	 */
	public synchronized @Nonnull List<Map<String, String>> getSteps() {
		return getSteps(0, steps.size());
	}

	/**
	 * Returns a copy of a range of the recorded steps, in the order received, so that a page can show the latest steps
	 * and fetch earlier ones when it needs them.
	 *
	 * <p>Complexity: O(count).
	 *
	 * @param from	the zero-based index of the first step; clamped to the steps there are
	 * @param count	the most steps to return
	 * @return a new list of new maps, each an op-code ({@code op}) and its attributes as text
	 */
	public synchronized @Nonnull List<Map<String, String>> getSteps(int from, int count) {
		int start = Math.max(0, Math.min(from, steps.size()));
		int end = Math.max(start, Math.min(steps.size(), start + Math.max(0, count)));
		List<Map<String, String>> result = new ArrayList<>(end - start);
		for (int i = start; i < end; i++) {
			result.add(new LinkedHashMap<>(steps.get(i)));
		}
		return result;
	}

	/**
	 * Indicates whether the recording has reached its cap ({@code sail.maximumSteps}) and refused steps. The recording
	 * keeps the steps it has, and can still be stopped and downloaded.
	 *
	 * @return true once steps were refused
	 */
	public synchronized boolean isLimitReached() {
		return limitReached;
	}

	/**
	 * Adds a batch of steps sent by the browser recorder, in order. Each step is a map of attribute names to values;
	 * values are kept as text. A step whose {@value #STEP_ID_KEY} was received before is skipped, because the recorder
	 * resends the steps it has not seen acknowledged (for example when the page unloaded during a request). Once the
	 * recording holds {@link UtilImpl#SAIL_RECORDING_MAXIMUM_STEPS} steps, further steps are refused and
	 * {@link #isLimitReached()} becomes true, so that the recorder can tell the user to stop and download.
	 *
	 * <p>Complexity: O(n) in the batch's size.
	 *
	 * @param batch	the steps, each a {@link Map}; other elements are skipped
	 * @return the number of steps added
	 * @throws IllegalStateException if the recording has stopped
	 */
	public synchronized int add(@Nonnull List<?> batch) {
		if (stoppedMillis != 0L) {
			throw new IllegalStateException("The SAIL recording has stopped");
		}
		int result = 0;
		for (Object element : batch) {
			if (element instanceof Map<?, ?> map) {
				LinkedHashMap<String, String> step = textMap(map);
				String id = step.get(STEP_ID_KEY);
				if ((id == null) || (! stepIds.contains(id))) {
					if (steps.size() >= UtilImpl.SAIL_RECORDING_MAXIMUM_STEPS) {
						limitReached = true;
						break;
					}
					if (id != null) {
						stepIds.add(id);
					}
					steps.add(step);
					result++;
				}
			}
		}
		return result;
	}

	/**
	 * Returns a map's non-null entries as text, in its order.
	 */
	private static @Nonnull LinkedHashMap<String, String> textMap(@Nonnull Map<?, ?> map) {
		LinkedHashMap<String, String> result = new LinkedHashMap<>(map.size() * 4 / 3 + 1);
		for (Entry<?, ?> entry : map.entrySet()) {
			Object key = entry.getKey();
			Object value = entry.getValue();
			if ((key != null) && (value != null)) {
				result.put(key.toString(), value.toString());
			}
		}
		return result;
	}

	/**
	 * Stops recording and builds the script from the steps. Stopping again returns the script built the first time.
	 *
	 * <p>Side effects: the recording no longer takes steps, and {@link #isRecording(HttpSession, User)} is false.
	 *
	 * @return the script as SAIL XML
	 */
	public synchronized @Nonnull String stop() {
		if (xml == null) {
			stoppedMillis = System.currentTimeMillis();
			// The sign in page asks for the customer only when the configuration does not fix one
			String loginCustomerName = (UtilImpl.CUSTOMER == null) ? customerName : null;
			SailRecordingBuilder builder = new SailRecordingBuilder(loginCustomerName, userName, startedMillis, stoppedMillis);
			xml = SailRecordingBuilder.toXml(builder.build(steps));
		}
		return xml;
	}

	/**
	 * Returns the script built when recording stopped.
	 *
	 * @return the SAIL XML, or {@code null} while recording
	 */
	public synchronized @Nullable String getXml() {
		return xml;
	}
}

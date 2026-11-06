package org.skyve.impl.sail.recording;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

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
	private static final org.slf4j.Logger LOGGER = org.skyve.util.logging.SkyveLoggerFactory.getLogger(SailRecording.class);

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
	/** The user's effective roles when recording started, {@code module.Role}, sorted. */
	private final ArrayList<String> roleNames;
	/**
	 * The PrimeFaces recorder's client-id maps built for this recording, by the page and user they describe (see
	 * {@link #pageMap(String, java.util.function.Supplier)}). Not kept when the session is serialised.
	 */
	private transient Map<String, String> pageMaps = null;
	/** The UX/UI the user got when recording started, or null if not known. */
	private final String uxui;
	/** The user agent type the user got when recording started, or null if not known. */
	private final UserAgentType userAgentType;
	/** The CSRF token issued for this recording, or null before the recorder first asks. Guarded by this. */
	private Integer token = null;
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

	private SailRecording(@Nullable String customerName,
							@Nonnull String userName,
							long startedMillis,
							@Nonnull List<String> roleNames,
							@Nullable String uxui,
							@Nullable UserAgentType userAgentType) {
		this.customerName = customerName;
		this.userName = userName;
		this.startedMillis = startedMillis;
		this.roleNames = new ArrayList<>(roleNames);
		this.uxui = uxui;
		this.userAgentType = userAgentType;
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
		return start(session, user, null);
	}

	/**
	 * Starts a new recording in the session, replacing any earlier one (and the script it built), and keeps the user's
	 * effective roles and UX/UI for the script's header.
	 *
	 * <p>Side effects: sets the session attribute {@link #SESSION_ATTRIBUTE_NAME}; removes the earlier recording's
	 * CSRF token from the session's tokens.
	 *
	 * @param session	the session to record
	 * @param user	the signed-in user
	 * @param uxui	the UX/UI the user got, or {@code null} if not known
	 * @return the new recording
	 * @throws SecurityException if the configuration or the user's roles do not allow recording
	 */
	public static @Nonnull SailRecording start(@Nonnull HttpSession session, @Nonnull User user, @Nullable String uxui) {
		return start(session, user, uxui, null);
	}

	/**
	 * Starts a new recording in the session, replacing any earlier one (and the script it built), for the UX/UI and user
	 * agent type the user got: the script targets them, so that a PrimeFaces recording replays in PrimeFaces.
	 *
	 * <p>Side effects: as {@link #start(HttpSession, User, String)}.
	 *
	 * @param session	the session to record
	 * @param user	the signed-in user
	 * @param uxui	the UX/UI the user got, or {@code null} if not known
	 * @param userAgentType	the user's agent type, or {@code null} if not known (the script then targets desktop)
	 * @return the new recording
	 * @throws SecurityException if the configuration or the user's roles do not allow recording
	 */
	public static @Nonnull SailRecording start(@Nonnull HttpSession session,
												@Nonnull User user,
												@Nullable String uxui,
												@Nullable UserAgentType userAgentType) {
		if (! isAllowed(user)) {
			throw new SecurityException("SAIL recording is not allowed for " + user.getName());
		}
		SailRecording earlier = get(session);
		if (earlier != null) {
			earlier.forgetToken(session);
		}
		String name = user.getName();
		SailRecording result = new SailRecording(user.getCustomerName(),
													(name == null) ? "" : name,
													System.currentTimeMillis(),
													effectiveRoleNames(user),
													uxui,
													userAgentType);
		session.setAttribute(SESSION_ATTRIBUTE_NAME, result);
		return result;
	}

	/**
	 * Returns a user's effective roles: each role of each of the customer's modules that the user is in, whether
	 * directly or through a group, as {@code module.Role}.
	 *
	 * <p>Complexity: O(r) in the customer's module roles.
	 *
	 * @param user	the user
	 * @return the role names, sorted
	 */
	public static @Nonnull List<String> effectiveRoleNames(@Nonnull User user) {
		Customer customer = user.getCustomer();
		List<String> result = new ArrayList<>();
		for (Module module : customer.getModules()) {
			String moduleName = module.getName();
			for (Role role : module.getRoles()) {
				String roleName = role.getName();
				if (user.isInRole(moduleName, roleName)) {
					result.add(moduleName + '.' + roleName);
				}
			}
		}
		Collections.sort(result);
		return result;
	}

	/**
	 * Returns the CSRF token for this recording's batches, issuing it the first time. The same token serves every page
	 * of the recording, so the session's tokens do not grow with each page the recorder loads into.
	 *
	 * <p>Side effects: the first call adds the token to the session's tokens ({@link StateUtil}).
	 *
	 * @param session	the recording's session
	 * @return the token
	 */
	public synchronized @Nonnull Integer token(@Nonnull HttpSession session) {
		Integer result = token;
		if (result == null) {
			result = StateUtil.createToken();
			StateUtil.replaceToken(session, null, result);
			token = result;
		}
		return result;
	}

	/**
	 * Removes this recording's token from the session's tokens, as a new recording replaces it.
	 */
	private synchronized void forgetToken(@Nonnull HttpSession session) {
		Integer old = token;
		if (old != null) {
			StateUtil.removeToken(session, old);
			token = null;
		}
	}

	/**
	 * Returns the user's effective roles when recording started.
	 *
	 * @return the role names, {@code module.Role}, sorted
	 */
	public @Nonnull List<String> getRoleNames() {
		return Collections.unmodifiableList(roleNames);
	}

	/**
	 * Returns the UX/UI the user got when recording started.
	 *
	 * @return the UX/UI name, or {@code null} if not known
	 */
	public @Nullable String getUxui() {
		return uxui;
	}

	/**
	 * Returns a PrimeFaces client-id map, built the first time a page asks for it and kept for the rest of the recording,
	 * as an ajax update of a page's form renders the map again. At most {@value #PAGE_MAPS} maps are kept.
	 *
	 * <p>Side effects: builds the map if it is not kept (a view's component tree build), under this recording's lock.
	 *
	 * @param key	what the map depends on: the page, its UX/UI and user agent type, and the user and their roles
	 * @param build	builds the map
	 * @return the map
	 */
	public synchronized @Nonnull String pageMap(@Nonnull String key, @Nonnull java.util.function.Supplier<String> build) {
		if (pageMaps == null) {
			pageMaps = new java.util.HashMap<>();
		}
		String result = pageMaps.get(key);
		if (result == null) {
			result = build.get();
			if (pageMaps.size() >= PAGE_MAPS) {
				pageMaps.clear();
			}
			pageMaps.put(key, result);
		}
		return result;
	}

	/** How many client-id maps a recording keeps. */
	private static final int PAGE_MAPS = 100;

	/**
	 * Returns the user agent type the user got when recording started.
	 *
	 * @return the user agent type, or {@code null} if not known (the script then targets desktop)
	 */
	public @Nullable UserAgentType getUserAgentType() {
		return userAgentType;
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
		// in long arithmetic: start + count overflows for a large count
		int end = (int) Math.max(start, Math.min((long) start + Math.max(0, count), steps.size()));
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
		checkRecording();
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
		return stop(null);
	}

	/**
	 * Stops recording and builds the script from the steps, checking each step with {@code check}: a step it cannot
	 * confirm becomes a comment that says why. Stopping again returns the script built the first time.
	 *
	 * <p>Side effects: the recording no longer takes steps, and {@link #isRecording(HttpSession, User)} is false.
	 *
	 * @param check	checks the steps against the metadata, or {@code null} for no checks
	 * @return the script as SAIL XML
	 */
	public synchronized @Nonnull String stop(@Nullable SailRecordingBuilder.StepCheck check) {
		if (xml == null) {
			// Build first, and stop only once there is a script: a recording must never be left stopped with nothing to
			// download. If the checks fail, the script is built without them (it says so); if even that fails, the
			// recording carries on and the error goes to the caller.
			long stopped = System.currentTimeMillis();
			String built;
			try {
				built = SailRecordingBuilder.toXml(builder(stopped).check(check).build(steps));
			}
			catch (RuntimeException e) {
				LOGGER.warn("SAIL recording could not check its steps; building the script without the checks", e);
				built = SailRecordingBuilder.toXml(builder(stopped).note("The steps could not be checked against the metadata: " + e).build(steps));
			}
			stoppedMillis = stopped;
			xml = built;
		}
		return xml;
	}

	/**
	 * Returns the name to download the script as: {@code sail-recording-yyyyMMdd-HHmm.xml}, from when recording started.
	 *
	 * @return the file name
	 */
	public @Nonnull String fileName() {
		return "sail-recording-" + java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmm").withZone(java.time.ZoneId.systemDefault())
																		.format(java.time.Instant.ofEpochMilli(startedMillis)) + ".xml";
	}

	/**
	 * Returns the number of gestures recorded so far: the steps that are not context.
	 *
	 * <p>Complexity: O(n) in the steps.
	 *
	 * @return the gesture count
	 */
	public synchronized int getGestureCount() {
		int result = 0;
		for (Map<String, String> step : steps) {
			if (! SailRecordingBuilder.CONTEXT.equals(step.get(SailRecordingBuilder.OP))) {
				result++;
			}
		}
		return result;
	}

	/**
	 * Deletes a recorded entry. Its id stays known, so that a resent copy is not added again.
	 *
	 * @param id	the entry's {@value #STEP_ID_KEY}
	 * @return true if it was deleted
	 * @throws IllegalStateException if the recording has stopped
	 */
	public synchronized boolean delete(@Nonnull String id) {
		checkRecording();
		for (int i = steps.size() - 1; i >= 0; i--) {
			if (id.equals(steps.get(i).get(STEP_ID_KEY))) {
				steps.remove(i);
				return true;
			}
		}
		return false;
	}

	/**
	 * Undoes the last gesture: removes the last entry that is not context, with the context recorded after it.
	 *
	 * @return the number of entries removed
	 * @throws IllegalStateException if the recording has stopped
	 */
	public synchronized int undo() {
		checkRecording();
		int last = steps.size() - 1;
		while ((last >= 0) && SailRecordingBuilder.CONTEXT.equals(steps.get(last).get(SailRecordingBuilder.OP))) {
			last--;
		}
		if (last < 0) {
			return 0;
		}
		int result = steps.size() - last;
		steps.subList(last, steps.size()).clear();
		return result;
	}

	private void checkRecording() {
		if (stoppedMillis != 0L) {
			throw new IllegalStateException("The SAIL recording has stopped");
		}
	}

	/**
	 * Returns what the script shows for a range of the entries, for the recorder's panel: each entry's id and lines
	 * (see {@link SailRecordingBuilder#preview(List, int, int)}).
	 *
	 * @param from	the zero-based index of the first entry
	 * @param count	the most entries
	 * @return for each entry, a map with {@code id} and {@code lines}
	 */
	public synchronized @Nonnull List<Map<String, Object>> preview(int from, int count) {
		List<List<String>> lines = builder().preview(steps, from, count);
		int start = Math.max(0, Math.min(from, steps.size()));
		List<Map<String, Object>> result = new ArrayList<>(lines.size());
		for (int i = 0, l = lines.size(); i < l; i++) {
			Map<String, Object> entry = new LinkedHashMap<>(4);
			Map<String, String> step = steps.get(start + i);
			entry.put(STEP_ID_KEY, step.get(STEP_ID_KEY));
			entry.put("context", Boolean.valueOf(SailRecordingBuilder.CONTEXT.equals(step.get(SailRecordingBuilder.OP))));
			entry.put("lines", lines.get(i));
			result.add(entry);
		}
		return result;
	}

	/**
	 * Returns what a page that loads the recorder needs to start at once, as JSON: the recording's CSRF token, its
	 * counts, and the last entries as the panel shows them, so that the panel needs no request to appear. A renderer
	 * that does a full page load per view (PrimeFaces) renders it into each page while recording.
	 *
	 * <p>Side effects: issues the recording's token if it has none ({@link #token(HttpSession)}).
	 *
	 * <p>Complexity: O(n) in the entries, for the last {@code count} entries' rendering.
	 *
	 * @param session	the recording's session
	 * @param count	how many of the last entries to include
	 * @return {@code {"recording":true,"_csrf":t,"steps":n,"gestures":g,"from":i,"entries":[...]}}
	 */
	public synchronized @Nonnull String pageState(@Nonnull HttpSession session, int count) {
		int from = Math.max(0, steps.size() - count);
		Map<String, Object> result = new LinkedHashMap<>(8);
		result.put("recording", Boolean.TRUE);
		result.put("_csrf", token(session));
		result.put("steps", Integer.valueOf(steps.size()));
		result.put("gestures", Integer.valueOf(getGestureCount()));
		result.put("from", Integer.valueOf(from));
		result.put("entries", preview(from, count));
		return org.skyve.util.JSON.marshall(result);
	}

	/**
	 * Returns the script as it stands: the one built on Stop, or while recording one built now without the metadata
	 * checks, for the panel's Copy and Download.
	 *
	 * @return the SAIL XML
	 */
	public synchronized @Nonnull String currentXml() {
		return (xml == null) ? SailRecordingBuilder.toXml(builder().build(steps)) : xml;
	}

	private @Nonnull SailRecordingBuilder builder() {
		return builder((stoppedMillis == 0L) ? System.currentTimeMillis() : stoppedMillis);
	}

	private @Nonnull SailRecordingBuilder builder(long stopped) {
		// The sign in page asks for the customer only when the configuration does not fix one
		String loginCustomerName = (UtilImpl.CUSTOMER == null) ? customerName : null;
		SailRecordingBuilder result = new SailRecordingBuilder(loginCustomerName, userName, startedMillis, stopped).roles(roleNames);
		return (userAgentType == null) ? result.uxui(uxui) : result.targeting(uxui, userAgentType);
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

package util.sail.browser;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.Connection;
import org.openqa.selenium.devtools.Event;
import org.openqa.selenium.devtools.SeleniumCdpConnection;
import org.openqa.selenium.devtools.idealized.target.model.SessionID;
import org.openqa.selenium.json.Json;
import org.openqa.selenium.json.JsonInput;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestRule.ErrorReason;
import util.sail.browser.RequestRule.Fail;
import util.sail.browser.RequestRule.Fulfil;
import util.sail.browser.RequestRule.Headers;
import util.sail.browser.RequestRule.Hold;
import util.sail.browser.RequestRule.Stage;

/**
 * Controls a Chromium browser (Chrome or Edge): every capability. Request interception, the request log, network
 * conditions, init scripts, dialogs, the storage quota, page lifecycle, touch and the screencast go through the Chrome DevTools
 * Protocol; cookies, tabs, keys and the window through plain WebDriver, as {@link WebDriverBrowserControl} does.
 * <p>
 * <b>The whole browser, not one page.</b> It attaches to every tab, every page in them, every service worker and
 * shared worker, and every dedicated worker and out-of-process frame, as they appear (DevTools
 * {@code Target.setAutoAttach}), and applies its settings to each one. This matters because the page's network
 * settings don't reach its service worker: a page set offline still gets the network through a worker's
 * {@code fetch}. A target that starts later (a new tab, a service worker Chrome starts again) waits until its settings
 * are applied. A service worker that Chrome stops and starts again keeps its DevTools session and so its settings.
 * <p>
 * <b>Lifetime.</b> It connects to DevTools as {@link SailBrowserControl#of(WebDriver)} makes it, which is the first time
 * a step asks for the browser's control, not when the browser starts: the request log and the dialogs record from then
 * on ({@link #watchingSince()}), and a dialog already open when it attaches is never reported. So a script that checks
 * them starts watching with an earlier call, such as {@code RequestLog} with {@code clear=true}, before the steps it
 * checks. If it can't connect then, it tries again when a DevTools capability is used, and records from then. Its
 * settings last until the browser stops; a browser started again (on the same profile, say) gets a new control, with
 * none of them.
 * <p>
 * <b>Raw DevTools.</b> Chrome's own protocol is spoken raw over its own connection ({@link SeleniumCdpConnection}),
 * as Selenium's typed {@code selenium-devtools-vNNN} bindings lag Chrome: Chrome 158 against devtools v142 printed
 * "Unable to find CDP implementation matching 158". Nothing of the protocol leaves this class.
 * <p>
 * <b>One DevTools session per page.</b> Nothing else may attach to a page: a second session on it (such as a screencast
 * opened with its own connection and {@code Target.attachToTarget}) makes a document that loads after the browser
 * goes offline report {@code navigator.onLine === true}, though its requests fail. So the screencast
 * ({@link PageScreencast}) runs on this control's own session for the window, and the control, which connects as
 * soon as it is made, attaches to every page of a run that records video.
 * <p>
 * Ported from basix-cloud's {@code SailDevTools}.
 * <p>
 * Threading: methods may be called from any thread. Every DevTools command and event is handled in turn on one thread
 * of its own, so a target that attaches while a setting changes still ends up with the new setting. Selenium hands
 * each event to a pool of threads, though, so events can reach that thread in another order than Chrome sent them.
 * What depends on the order copes with that: a request's log entry is filled in as its events come, its method and URL
 * from the last redirect (Selenium numbers the events as they arrive); a target that detaches before its attach is
 * handled is not added; a setting sent to a session that has gone drops it; and a held request belongs to the document
 * that sent it (its loader id), not to a place in the order of events, so a navigation drops only the held requests of
 * other documents, and a request of a document its frame has already left isn't held.
 */
public final class ChromiumBrowserControl extends WebDriverBrowserControl
		implements RequestInterception, RequestLog, NetworkConditions, InitScripts, Dialogs, StorageQuota, PageLifecycle, TouchEmulation, PageScreencast {
	/** A DevTools session on one of the browser's targets. */
	private record Target(@Nonnull String targetId, @Nonnull String type) {
		boolean isPage() {
			return "page".equals(type);
		}

		/** Pages and workers have network and fetch domains; a tab only holds its pages. */
		boolean hasNetwork() {
			return ! "tab".equals(type);
		}
	}

	/** A request in the log, filled in as its events arrive (in any order). Touched only on the DevTools thread. */
	private static final class RequestEntry {
		private String method = "";
		private String url = "";
		/** Where the {@code Network.requestWillBeSent} that set the method and URL came in the order of events. */
		private long sent = -1;
		/** The document that sent it (its loader id), for a request held later; not logged. Null if not known. */
		private String loaderId;
		private Integer status;
		private String failure;
		private boolean fromServiceWorker;

		@Nonnull LoggedRequest snapshot() {
			return new LoggedRequest(method, url, status, failure, fromServiceWorker);
		}
	}

	/**
	 * A request a rule holds: the frame that sent it, its network request id, and the document that sent it (its loader
	 * id, from {@code Network.requestWillBeSent}), or null if that event hadn't been handled when the request was held.
	 */
	private record HeldRequest(@Nonnull String frameId, @Nonnull String networkId, @Nullable String loaderId) {
	}

	/** Sends one DevTools command to one session. */
	private interface SessionCommand {
		void send(@Nonnull String sessionId);
	}

	private static final Duration COMMAND_TIMEOUT = Duration.ofSeconds(10);
	private static final Duration ATTACH_TIMEOUT = Duration.ofSeconds(10);
	/**
	 * The targets that must be attached before the first setting is applied. Chrome also has {@code browser_ui} targets
	 * (the omnibox popup, {@code chrome://omnibox-popup.top-chrome/}), which aren't the application's: the attach
	 * filters leave them out, and so does this.
	 */
	private static final Set<String> TOP_LEVEL_TYPES = Set.of("page", "service_worker", "shared_worker");
	private static final String PREFIX = "SAIL browser control: ";
	private static final String CONTINUE_REQUEST = "Fetch.continueRequest";
	private static final String SET_AUTO_ATTACH = "Target.setAutoAttach";
	private static final String REQUEST_ID = "requestId";
	private static final String REQUEST = "request";
	private static final String METHOD = "method";
	private static final String RESPONSE_HEADERS = "responseHeaders";
	/** What Chrome says when a command names a session that has gone. */
	private static final String SESSION_NOT_FOUND = "Session with given id not found";
	/**
	 * What Chrome says when a command about a paused request goes to a session that didn't pause it: the request id is
	 * another session's, or the session intercepts nothing.
	 */
	private static final List<String> NOT_THIS_SESSIONS_REQUEST = List.of("Invalid InterceptionId", "Fetch domain is not enabled", SESSION_NOT_FOUND);

	/** The DevTools connection; guarded by {@code this}. */
	@SuppressWarnings("resource") // closed by release()
	private Connection connection;
	/** The thread every DevTools command and event runs on, made with the connection; guarded by {@code this}. */
	private ExecutorService thread;
	/** When the control began watching the browser: when it connected to DevTools; guarded by {@code this}. */
	private Instant watchingSince;
	/** The sessions by session id; changed only on the DevTools thread, read anywhere. */
	private final Map<String, Target> sessions = new ConcurrentHashMap<>();
	/**
	 * The sessions that have gone with an event still to come, which is ignored: the attach of one whose detach was
	 * handled first, or the detach of one dropped as Chrome said it had gone. Only on the DevTools thread.
	 */
	private final Set<String> gone = new HashSet<>();
	/** The screencast's sink, DevTools session and page target; null when none is on. Touched only on the DevTools thread. */
	private Consumer<Image> screencastSink;
	private String screencastSession;
	private String screencastTarget;

	// The settings, applied to each target as it attaches. Touched only on the DevTools thread.
	private Map<String, Object> network = networkConditions(false, 0, -1, -1);
	private int touchPoints;
	private final List<String> initScripts = new ArrayList<>();
	private final List<RequestRule> rules = new ArrayList<>();
	private final List<int[]> ruleUses = new ArrayList<>(); // how many times each rule has applied
	/** The requests held, by paused request id. */
	private final Map<String, HeldRequest> held = new LinkedHashMap<>();
	/** Each frame's document now, by frame id: its loader id, from {@code Page.frameNavigated}. */
	private final Map<String, String> documents = new HashMap<>();
	/**
	 * The documents (loader ids) that frames have left, by navigating or closing: a request of one isn't held. One id per
	 * navigation, for the browser's life.
	 */
	private final Set<String> leftDocuments = new HashSet<>();
	private final List<Dialog> dialogs = new ArrayList<>();
	private final Map<String, RequestEntry> requests = new LinkedHashMap<>();

	/** Signalled when a target attaches; guards {@link #attaches}. */
	private final Object attachSignal = new Object();
	/** How many targets have attached, so a wait can tell that another has. */
	private long attaches;
	/** Signalled when the held requests change; guards {@link #heldNow}. */
	private final Object heldSignal = new Object();
	/** How many requests are held: {@link #held}'s size, readable off the DevTools thread. */
	private int heldNow;

	/**
	 * Controls a Chromium browser through its driver, and starts watching it; {@link SailBrowserControl#of(WebDriver)}
	 * makes it.
	 *
	 * @param driver	a Chromium browser's driver
	 */
	ChromiumBrowserControl(@Nonnull WebDriver driver) {
		super(driver);
		// Watch from now, as DevTools reports neither the requests before it attached nor a dialog already open.
		try {
			devTools();
		}
		catch (RuntimeException e) {
			System.out.println(PREFIX + "could not connect to DevTools (" + firstLine(e) + "); trying again when a DevTools capability is used");
		}
	}

	// ---- network

	@Override
	public void offline() {
		network(networkConditions(true, 0, -1, -1));
	}

	@Override
	public void online() {
		network(networkConditions(false, 0, -1, -1));
	}

	@Override
	public void throttle(@Nonnull Duration latency, long downloadBytesPerSecond, long uploadBytesPerSecond) {
		network(networkConditions(false, latency.toMillis(), downloadBytesPerSecond, uploadBytesPerSecond));
	}

	/** Sets the network conditions of every page and worker, now and as they start. */
	private void network(@Nonnull Map<String, Object> conditions) {
		call(() -> {
			network = conditions;
			forEachSession(Target::hasNetwork, sessionId -> send(sessionId, "Network.emulateNetworkConditions", conditions));
			return null;
		});
	}

	/** Returns the parameters of {@code Network.emulateNetworkConditions}. */
	private static @Nonnull Map<String, Object> networkConditions(boolean offline, long latencyMillis, long downloadBytesPerSecond, long uploadBytesPerSecond) {
		return Map.of("offline", Boolean.valueOf(offline),
						"latency", Long.valueOf(latencyMillis),
						"downloadThroughput", Long.valueOf(downloadBytesPerSecond),
						"uploadThroughput", Long.valueOf(uploadBytesPerSecond));
	}

	// ---- touch

	@Override
	public void emulateTouch(int maxTouchPoints) {
		call(() -> {
			touchPoints = maxTouchPoints;
			forEachSession(Target::isPage, sessionId -> send(sessionId, "Emulation.setTouchEmulationEnabled", touch()));
			return null;
		});
	}

	private @Nonnull Map<String, Object> touch() {
		return (touchPoints > 0) ? Map.of("enabled", Boolean.TRUE, "maxTouchPoints", Integer.valueOf(touchPoints)) : Map.of("enabled", Boolean.FALSE);
	}

	// ---- page lifecycle

	/**
	 * {@inheritDoc}
	 *
	 * <p>It is Chrome's web lifecycle state (DevTools {@code Page.setWebLifecycleState}), as when it freezes a tab in
	 * the background.
	 */
	@Override
	public void freeze(@Nonnull String windowHandle) {
		lifecycle(windowHandle, "frozen");
	}

	@Override
	public void resume(@Nonnull String windowHandle) {
		lifecycle(windowHandle, "active");
	}

	/** Sets a tab's web lifecycle state. A window handle is Chrome's target id, with or without the CDwindow- prefix. */
	private void lifecycle(@Nonnull String windowHandle, @Nonnull String state) {
		String targetId = windowHandle.replaceFirst("^CDwindow-", "");
		call(() -> {
			for (Entry<String, Target> session : sessions.entrySet()) {
				if (session.getValue().isPage() && session.getValue().targetId().equals(targetId)) {
					send(session.getKey(), "Page.setWebLifecycleState", Map.of("state", state));
					return null;
				}
			}
			throw new IllegalStateException("The browser has no attached page " + windowHandle);
		});
	}

	// ---- init scripts

	/**
	 * {@inheritDoc}
	 *
	 * <p>A script already added is not added again, as DevTools would run each copy.
	 */
	@Override
	public void addInitScript(@Nonnull String source) {
		call(() -> {
			if (initScripts.contains(source)) {
				return null;
			}
			initScripts.add(source);
			forEachSession(Target::isPage, sessionId -> send(sessionId, "Page.addScriptToEvaluateOnNewDocument", Map.of("source", source)));
			return null;
		});
	}

	// ---- dialogs

	/**
	 * {@inheritDoc}
	 *
	 * <p>This control records the dialogs from when it was made, or else from when it first connected to DevTools
	 * ({@link #watchingSince()}).
	 */
	@Override
	public @Nonnull List<Dialog> dialogs() {
		return call(() -> List.copyOf(dialogs));
	}

	@Override
	public void clearDialogs() {
		call(() -> {
			dialogs.clear();
			return null;
		});
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>The dialog is answered in whichever page has it open, as DevTools events don't say which page that is: each
	 * page is asked in turn, and the others refuse.
	 */
	@Override
	public void answerDialog(boolean accept, @Nullable String promptText) {
		Map<String, Object> params = new LinkedHashMap<>();
		params.put("accept", Boolean.valueOf(accept));
		if (promptText != null) {
			params.put("promptText", promptText);
		}
		boolean answered = call(() -> {
			for (Entry<String, Target> session : sessions.entrySet()) {
				if (session.getValue().isPage()) {
					try {
						send(session.getKey(), "Page.handleJavaScriptDialog", params);
						return Boolean.TRUE;
					}
					catch (@SuppressWarnings("unused") RuntimeException e) {
						// no dialog is open in this page
					}
				}
			}
			return Boolean.FALSE;
		}).booleanValue();
		if (! answered) {
			throw new IllegalStateException("No dialog is open to answer");
		}
	}

	/** Returns a dialog's type from its DevTools name, such as {@code beforeunload}. */
	static @Nonnull Dialog.Type dialogType(@Nonnull String devTools) {
		return Dialog.Type.valueOf(devTools.toUpperCase(Locale.ROOT));
	}

	// ---- request interception

	/**
	 * {@inheritDoc}
	 *
	 * <p>Every page and worker gets the rule, because a request that a service worker forwards with its own
	 * {@code fetch} is stopped only in the worker's session, not the page's.
	 */
	@Override
	public void addRule(@Nonnull RequestRule rule) {
		call(() -> {
			rules.add(rule);
			ruleUses.add(new int[1]);
			Map<String, Object> patterns = fetchPatterns();
			forEachSession(Target::hasNetwork, sessionId -> send(sessionId, "Fetch.enable", patterns));
			return null;
		});
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>The held requests are let go and the rules cleared in one turn of the DevTools thread, so no request is held
	 * in between and left counted.
	 */
	@Override
	public void clearRules() {
		call(() -> {
			continueHeld(CONTINUE_REQUEST, Map.of());
			rules.clear();
			ruleUses.clear();
			forEachSession(Target::hasNetwork, sessionId -> send(sessionId, "Fetch.disable", Map.of()));
			return null;
		});
	}

	@Override
	public int heldCount() {
		devTools(); // so that requests can be held
		synchronized (heldSignal) {
			return heldNow;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>The wait is on the event, not a poll: each request this control holds wakes it.
	 */
	@Override
	public void awaitHeld(int count, @Nonnull Duration timeout) {
		if (count < 1) {
			throw new IllegalArgumentException("Await at least 1 held request, not " + count);
		}
		devTools();
		long deadline = System.nanoTime() + timeout.toNanos();
		synchronized (heldSignal) {
			while (heldNow < count) {
				long left = deadline - System.nanoTime();
				if (left <= 0) {
					throw new AssertionError(String.format("Expected %d held request(s) within %s, but %d were held",
															Integer.valueOf(count), timeout, Integer.valueOf(heldNow)));
				}
				try {
					TimeUnit.NANOSECONDS.timedWait(heldSignal, left);
				}
				catch (InterruptedException e) {
					Thread.currentThread().interrupt();
					throw new IllegalStateException("Interrupted waiting for held requests", e);
				}
			}
		}
	}

	@Override
	public void releaseHeld() {
		finishHeld(CONTINUE_REQUEST, Map.of());
	}

	@Override
	public void failHeld(@Nonnull ErrorReason reason) {
		finishHeld("Fetch.failRequest", Map.of("errorReason", devToolsName(reason)));
	}

	private void finishHeld(@Nonnull String method, @Nonnull Map<String, Object> params) {
		call(() -> {
			continueHeld(method, params);
			return null;
		});
	}

	/** Sends each held request a command, and holds none. Runs on the DevTools thread. */
	private void continueHeld(@Nonnull String method, @Nonnull Map<String, Object> params) {
		for (String requestId : held.keySet()) {
			Map<String, Object> p = new LinkedHashMap<>(params);
			p.put(REQUEST_ID, requestId);
			toOwner(requestId, method, p);
		}
		held.clear();
		heldChanged();
	}

	/**
	 * Takes a frame's new document ({@code Page.frameNavigated}): the document it showed before is left, and the frame's
	 * held requests from any document but the new one are forgotten. They went with their document, and DevTools reports
	 * nothing else of them (no {@code Network.loadingFailed}). A held request whose document isn't known is kept, as it
	 * may be the new document's (whose requests can be handled before its navigation). Only the frame's own requests go:
	 * those of the iframes in it don't. Runs on the DevTools thread.
	 */
	private void navigated(@Nonnull Map<String, Object> params) {
		Map<String, Object> frame = map(params, "frame");
		String frameId = string(frame, "id");
		String loaderId = string(frame, "loaderId");
		String before = documents.put(frameId, loaderId);
		if ((before != null) && (! before.equals(loaderId))) {
			leftDocuments.add(before);
		}
		dropHeld(h -> {
			String document = (h.loaderId() == null) ? loaderId(h.networkId()) : h.loaderId();
			return h.frameId().equals(frameId) && (document != null) && (! document.equals(loaderId));
		});
	}

	/**
	 * Forgets the held requests of a page that has closed (or a worker that has stopped), the document it showed and its screencast.
	 * Only the page's own requests go: those of the iframes in it don't. Runs on the DevTools thread.
	 *
	 * @param targetId	the target's id, which for a page is its main frame's id
	 */
	private void closed(@Nonnull String targetId) {
		if (targetId.equals(screencastTarget)) {
			screencastSink = null; // the page has closed, and its recording with it
			screencastSession = null;
			screencastTarget = null;
		}
		String document = documents.remove(targetId);
		if (document != null) {
			leftDocuments.add(document);
		}
		dropHeld(h -> h.frameId().equals(targetId));
	}

	/** Forgets some held requests. Runs on the DevTools thread. */
	private void dropHeld(@Nonnull Predicate<HeldRequest> which) {
		if (held.values().removeIf(which)) {
			heldChanged();
		}
	}

	/** Returns the document (loader id) that sent a request, or null if it isn't known: the log was cleared, say. */
	private @Nullable String loaderId(@Nonnull String networkId) {
		RequestEntry logged = requests.get(networkId);
		return (logged == null) ? null : logged.loaderId;
	}

	/** Publishes how many requests are held, and wakes a wait for them. Called on the DevTools thread. */
	private void heldChanged() {
		synchronized (heldSignal) {
			heldNow = held.size();
			heldSignal.notifyAll();
		}
	}

	/** Returns the {@code Fetch.enable} parameters: a pattern per distinct URL glob and stage of the rules. */
	private @Nonnull Map<String, Object> fetchPatterns() {
		List<Map<String, String>> patterns = new ArrayList<>(rules.size());
		for (RequestRule rule : rules) {
			// DevTools' URL patterns are globs as RequestRule's are, so DevTools stops only the requests a rule may take
			Map<String, String> pattern = Map.of("urlPattern", rule.urlPattern(), "requestStage", devToolsName(rule.stage()));
			if (! patterns.contains(pattern)) {
				patterns.add(pattern);
			}
		}
		return Map.of("patterns", patterns);
	}

	/**
	 * Takes a paused request: applies the first rule that matches and still has uses, or lets it carry on.
	 *
	 * @param params	the {@code Fetch.requestPaused} event
	 */
	private void paused(@Nonnull Map<String, Object> params) {
		String requestId = string(params, REQUEST_ID);
		Map<String, Object> request = map(params, REQUEST);
		Stage stage = (params.containsKey("responseStatusCode") || params.containsKey("responseErrorReason")) ? Stage.RESPONSE : Stage.REQUEST;
		String networkId = string(params, "networkId");
		String loaderId = loaderId(networkId);
		RequestRule rule = null;
		// A request of a document its frame has left was paused before the navigation, but handled after it: it went
		// with its document, so no rule takes it, and it carries on rather than being held for nothing
		if ((loaderId == null) || (! leftDocuments.contains(loaderId))) {
			for (int i = 0, l = rules.size(); i < l; i++) {
				RequestRule candidate = rules.get(i);
				int[] uses = ruleUses.get(i);
				if (candidate.matches(string(request, "url"), string(request, METHOD), stage) &&
						((candidate.times() == 0) || (uses[0] < candidate.times()))) {
					uses[0]++;
					rule = candidate;
					break;
				}
			}
		}
		if ((rule != null) && (rule.action() instanceof Hold)) {
			held.put(requestId, new HeldRequest(string(params, "frameId"), networkId, loaderId));
			heldChanged();
			return;
		}
		Map<String, Object> answer = answer(requestId, params, rule, stage);
		toOwner(requestId, string(answer, METHOD), map(answer, "params"));
	}

	/**
	 * Sends a command about a paused request to the session that paused it. The event doesn't say which session that
	 * is, so each is tried until one takes it: the others refuse the request id. If none takes it and one refused the
	 * command itself (a status or header Chrome won't take, which Chrome checks before the request id, so in every
	 * session), the request is failed, so the page isn't left waiting for it, and the first such refusal is printed.
	 */
	private void toOwner(@Nonnull String requestId, @Nonnull String method, @Nonnull Map<String, Object> params) {
		RuntimeException refusal = null;
		for (Entry<String, Target> session : sessions.entrySet()) {
			if (session.getValue().hasNetwork()) {
				try {
					send(session.getKey(), method, params);
					return;
				}
				catch (RuntimeException e) {
					// another session's request, or this session couldn't answer (it went, or timed out): try the rest
					if ((refusal == null) && (! says(e, NOT_THIS_SESSIONS_REQUEST))) {
						refusal = e;
					}
				}
			}
		}
		if (refusal == null) {
			// none does if the request has gone, such as a page that gave up on it
			System.out.println(PREFIX + "no session took " + method + " for request " + requestId);
			return;
		}
		System.out.println(PREFIX + "Chrome refused " + method + " for request " + requestId + ", so it fails: " + firstLine(refusal));
		Map<String, Object> fail = Map.of(REQUEST_ID, requestId, "errorReason", devToolsName(ErrorReason.FAILED));
		// the session that refused may not be the request's, so each is told; the others refuse the request id
		forEachSession(Target::hasNetwork, sessionId -> sendQuietly(sessionId, "Fetch.failRequest", fail));
	}

	/**
	 * Returns the command that does what a rule says to a paused request, or continues it.
	 *
	 * @param requestId	the paused request's id
	 * @param params	the {@code Fetch.requestPaused} event
	 * @param rule	the rule that applies, or null to let the request carry on
	 * @param stage	the stage it is paused at
	 * @return {@code {method, params}}
	 */
	static @Nonnull Map<String, Object> answer(@Nonnull String requestId, @Nonnull Map<String, Object> params,
												@Nullable RequestRule rule, @Nonnull Stage stage) {
		Object status = params.get("responseStatusCode");
		if ((rule == null) || ((rule.action() instanceof Headers) && (stage == Stage.RESPONSE) && (status == null))) {
			// no rule, or response headers to change on a response that failed and so has none
			// a response that failed has no status for continueResponse, so it carries on as continueRequest has it
			return command(((stage == Stage.REQUEST) || (status == null)) ? CONTINUE_REQUEST : "Fetch.continueResponse",
							Map.of(REQUEST_ID, requestId));
		}
		if (rule.action() instanceof Fail fail) {
			return command("Fetch.failRequest", Map.of(REQUEST_ID, requestId, "errorReason", devToolsName(fail.reason())));
		}
		if (rule.action() instanceof Fulfil fulfil) {
			return command("Fetch.fulfillRequest", Map.of(REQUEST_ID, requestId,
															"responseCode", Integer.valueOf(fulfil.status()),
															RESPONSE_HEADERS, headerList(fulfil.headers()),
															"body", Base64.getEncoder().encodeToString(fulfil.body().getBytes(StandardCharsets.UTF_8))));
		}
		Headers headers = (Headers) rule.action();
		if (stage == Stage.REQUEST) {
			Map<String, String> original = stringMap(map(map(params, REQUEST), "headers"));
			return command(CONTINUE_REQUEST, Map.of(REQUEST_ID, requestId, "headers", headerList(mergeHeaders(original, headers))));
		}
		Map<String, String> original = new LinkedHashMap<>();
		for (Object header : (List<?>) params.getOrDefault(RESPONSE_HEADERS, List.of())) {
			if (header instanceof Map<?, ?> h) {
				original.put(String.valueOf(h.get("name")), String.valueOf(h.get("value")));
			}
		}
		return command("Fetch.continueResponse", Map.of(REQUEST_ID, requestId,
														"responseCode", status,
														RESPONSE_HEADERS, headerList(mergeHeaders(original, headers))));
	}

	private static @Nonnull Map<String, Object> command(@Nonnull String method, @Nonnull Map<String, Object> params) {
		return Map.of(METHOD, method, "params", params);
	}

	/**
	 * Returns headers with some removed and some added or replaced; names are compared in any case.
	 *
	 * @param original	the request's or response's headers
	 * @param change	the change
	 * @return the new headers, in their original order, then the added ones
	 */
	static @Nonnull Map<String, String> mergeHeaders(@Nonnull Map<String, String> original, @Nonnull Headers change) {
		Set<String> dropped = new HashSet<>();
		for (String name : change.remove()) {
			dropped.add(name.toLowerCase(Locale.ROOT));
		}
		for (String name : change.set().keySet()) {
			dropped.add(name.toLowerCase(Locale.ROOT));
		}
		Map<String, String> result = new LinkedHashMap<>();
		for (Entry<String, String> header : original.entrySet()) {
			if (! dropped.contains(header.getKey().toLowerCase(Locale.ROOT))) {
				result.put(header.getKey(), header.getValue());
			}
		}
		result.putAll(change.set());
		return result;
	}

	private static @Nonnull List<Map<String, String>> headerList(@Nonnull Map<String, String> headers) {
		List<Map<String, String>> result = new ArrayList<>(headers.size());
		for (Entry<String, String> header : headers.entrySet()) {
			result.add(Map.of("name", header.getKey(), "value", header.getValue()));
		}
		return result;
	}

	/**
	 * Returns a stage's DevTools name, for {@code Fetch.enable}.
	 *
	 * @param stage	the stage
	 * @return {@code Request} or {@code Response}
	 */
	static @Nonnull String devToolsName(@Nonnull Stage stage) {
		return switch (stage) {
			case REQUEST -> "Request";
			case RESPONSE -> "Response";
		};
	}

	/**
	 * Returns an error's DevTools name ({@code Network.ErrorReason}), for {@code Fetch.failRequest}.
	 *
	 * @param reason	the error
	 * @return the name, such as {@code InternetDisconnected}
	 */
	static @Nonnull String devToolsName(@Nonnull ErrorReason reason) {
		return switch (reason) {
			case FAILED -> "Failed";
			case ABORTED -> "Aborted";
			case TIMED_OUT -> "TimedOut";
			case ACCESS_DENIED -> "AccessDenied";
			case CONNECTION_CLOSED -> "ConnectionClosed";
			case CONNECTION_RESET -> "ConnectionReset";
			case CONNECTION_REFUSED -> "ConnectionRefused";
			case CONNECTION_ABORTED -> "ConnectionAborted";
			case CONNECTION_FAILED -> "ConnectionFailed";
			case NAME_NOT_RESOLVED -> "NameNotResolved";
			case INTERNET_DISCONNECTED -> "InternetDisconnected";
			case ADDRESS_UNREACHABLE -> "AddressUnreachable";
			case BLOCKED_BY_CLIENT -> "BlockedByClient";
			case BLOCKED_BY_RESPONSE -> "BlockedByResponse";
		};
	}

	// ---- request log

	/**
	 * {@inheritDoc}
	 *
	 * <p>This control logs the requests from when it was made, or else from when it first connected to DevTools
	 * ({@link #watchingSince()}).
	 */
	@Override
	public @Nonnull List<LoggedRequest> requests() {
		return call(() -> requests.values().stream().map(RequestEntry::snapshot).toList());
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>It is when this control connected to DevTools, connecting now if it hasn't: as {@link SailBrowserControl#of}
	 * made it, or else the first time a DevTools capability was used.
	 */
	@Override
	public synchronized @Nonnull Instant watchingSince() {
		devTools();
		return watchingSince;
	}

	@Override
	public void clearRequests() {
		call(() -> {
			requests.clear();
			return null;
		});
	}

	private @Nonnull RequestEntry logged(@Nonnull Map<String, Object> params) {
		return requests.computeIfAbsent(string(params, REQUEST_ID), id -> new RequestEntry());
	}

	// ---- storage quota

	/**
	 * {@inheritDoc}
	 *
	 * <p>The override is set in a page's session ({@code Storage.overrideQuotaForOrigin}), where Chrome enforces it on
	 * writes; Chrome 158's {@code navigator.storage.estimate()} still reports the real quota.
	 */
	@Override
	public void overrideQuota(@Nonnull String origin, long bytes) {
		quota(Map.of("origin", origin, "quotaSize", Long.valueOf(bytes)));
	}

	@Override
	public void clearQuotaOverride(@Nonnull String origin) {
		quota(Map.of("origin", origin));
	}

	private void quota(@Nonnull Map<String, Object> params) {
		call(() -> {
			// a page's session: the browser's session refuses it
			send(firstSession(Target::isPage), "Storage.overrideQuotaForOrigin", params);
			return null;
		});
	}

	// ---- screencast

	/**
	 * {@inheritDoc}
	 *
	 * <p>It is DevTools' {@code Page.startScreencast} on this control's session for the window's page, never on a
	 * session of its own: see the class comment for why a second session on a page is not opened. Frames are
	 * acknowledged here, as they arrive, so the browser sends the next.
	 */
	@Override
	public void startScreencast(@Nonnull String windowHandle, @Nonnull Options options, @Nonnull Consumer<Image> sink) {
		String targetId = windowHandle.replaceFirst("^CDwindow-", ""); // a window handle is a page's target id
		Map<String, Object> params = Map.of("format", options.format(),
											"quality", Integer.valueOf(options.quality()),
											"maxWidth", Integer.valueOf(options.maxWidth()),
											"maxHeight", Integer.valueOf(options.maxHeight()));
		call(() -> {
			String sessionId = null;
			for (Entry<String, Target> session : sessions.entrySet()) {
				if (session.getValue().isPage() && session.getValue().targetId().equals(targetId)) {
					sessionId = session.getKey();
				}
			}
			if (sessionId == null) {
				throw new IllegalStateException("The browser has no attached page " + windowHandle);
			}
			stopScreencastNow();
			screencastSink = sink;
			screencastSession = sessionId;
			screencastTarget = targetId;
			try {
				send(sessionId, "Page.startScreencast", params);
			}
			catch (RuntimeException e) {
				screencastSink = null;
				screencastSession = null;
				screencastTarget = null;
				throw e;
			}
			return null;
		});
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>It does nothing, and does not connect to DevTools, if no screencast was started (or the control has been
	 * released), so stopping the recording of a browser that has quit does not try to reach it again.
	 */
	@Override
	public void stopScreencast() {
		synchronized (this) {
			if (thread == null) {
				return;
			}
		}
		call(() -> {
			stopScreencastNow();
			return null;
		});
	}

	/** Stops the screencast, if one is on, and forgets it. Runs on the DevTools thread. */
	private void stopScreencastNow() {
		String sessionId = screencastSession;
		screencastSink = null;
		screencastSession = null;
		screencastTarget = null;
		if (sessionId != null) {
			sendQuietly(sessionId, "Page.stopScreencast", Map.of()); // the page may have gone
		}
	}

	/** Acknowledges a screencast frame, so that the browser sends the next, and hands it to the sink. Runs on the DevTools thread. */
	private void screencastFrame(@Nonnull Map<String, Object> frame) {
		String sessionId = screencastSession;
		Consumer<Image> sink = screencastSink;
		if ((sessionId == null) || (sink == null)) {
			return; // stopped, or its page has closed: a frame already on its way is dropped
		}
		Object ack = frame.get("sessionId");
		if (ack != null) {
			acknowledge(sessionId, ack);
		}
		if (frame.get("data") instanceof String base64) {
			Object timestamp = map(frame, "metadata").get("timestamp"); // seconds
			long millis = (timestamp instanceof Number n) ? Math.round(n.doubleValue() * 1000.0) : System.currentTimeMillis();
			sink.accept(new Image(base64, millis));
		}
	}

	/** Acknowledges a frame without waiting for the answer, as a frame's turn on the DevTools thread should be short. */
	private void acknowledge(@Nonnull String sessionId, @Nonnull Object frameId) {
		Connection c = connection;
		if (c != null) {
			try {
				c.send(new SessionID(sessionId), new Command<>("Page.screencastFrameAck", Map.of("sessionId", frameId)));
			}
			catch (RuntimeException e) {
				System.out.println(PREFIX + "could not acknowledge a screencast frame: " + firstLine(e)); // the browser has gone
			}
		}
	}

	// ---- lifetime

	/** Connects to DevTools and starts watching the browser, the first time. */
	@SuppressWarnings("resource") // the connection and thread are this control's, closed by release(), or below if it can't start
	private synchronized void devTools() {
		if (thread != null) {
			return;
		}
		connection = SeleniumCdpConnection.create(driver())
								.orElseThrow(() -> new IllegalStateException(browser() + " offers no DevTools connection"));
		thread = Executors.newSingleThreadExecutor(r -> {
			Thread t = new Thread(r, "sail-browser-control");
			t.setDaemon(true);
			return t;
		});
		watchingSince = Instant.now(); // before the first event can arrive
		try {
			start();
		}
		catch (RuntimeException e) {
			release();
			throw e;
		}
	}

	/** Queues some work on the DevTools thread, connecting first if need be. */
	private synchronized @Nonnull <T> Future<T> submit(@Nonnull Supplier<T> work) {
		devTools();
		return thread.submit(work::get);
	}

	/** Listens for the browser's events, then attaches to its current and future targets. */
	private void start() {
		listen("Target.attachedToTarget", this::attached);
		listen("Target.detachedFromTarget", params -> {
			String sessionId = string(params, "sessionId");
			Target target = sessions.remove(sessionId);
			if (target != null) {
				closed(target.targetId());
			}
			else if (! gone.remove(sessionId)) {
				gone.add(sessionId); // not dropped as gone already, so its attach comes later
			}
		});
		listen("Page.screencastFrame", this::screencastFrame);
		listen("Fetch.requestPaused", this::paused);
		listen("Page.frameNavigated", this::navigated);
		listen("Page.javascriptDialogOpening", params -> dialogs.add(new Dialog(dialogType(string(params, "type")),
																				string(params, "message"),
																				string(params, "url"))));
		listen("Network.requestWillBeSent", (sequence, params) -> {
			Map<String, Object> request = map(params, REQUEST);
			RequestEntry logged = logged(params);
			// a redirect sends the event again for the same request: the later one has the URL it went to
			if (sequence.longValue() > logged.sent) {
				logged.sent = sequence.longValue();
				logged.method = string(request, METHOD);
				logged.url = string(request, "url");
			}
			String loaderId = string(params, "loaderId");
			if (! loaderId.isEmpty()) {
				logged.loaderId = loaderId;
			}
		});
		listen("Network.responseReceived", params -> {
			Map<String, Object> response = map(params, "response");
			RequestEntry logged = logged(params);
			if (response.get("status") instanceof Number status) {
				logged.status = Integer.valueOf(status.intValue());
			}
			logged.fromServiceWorker = Boolean.TRUE.equals(response.get("fromServiceWorker"));
		});
		listen("Network.loadingFailed", params -> logged(params).failure = string(params, "errorText"));

		call(() -> {
			send(null, SET_AUTO_ATTACH, autoAttach(List.of("tab", "service_worker", "shared_worker")));
			return null;
		});
		awaitCurrentTargets();
	}

	/**
	 * Waits until each page and worker the browser has now is attached, so the first setting reaches them. The wait is
	 * on the event, not a poll: each target attaching wakes it to check again.
	 */
	private void awaitCurrentTargets() {
		long deadline = System.nanoTime() + ATTACH_TIMEOUT.toNanos();
		while (true) {
			long seen;
			synchronized (attachSignal) {
				seen = attaches;
			}
			List<String> missing = call(this::unattachedTargets);
			if (missing.isEmpty()) {
				return;
			}
			synchronized (attachSignal) {
				while (attaches == seen) {
					long left = deadline - System.nanoTime();
					if (left <= 0) {
						throw new IllegalStateException("DevTools did not attach to " + missing + " within " + ATTACH_TIMEOUT);
					}
					try {
						TimeUnit.NANOSECONDS.timedWait(attachSignal, left);
					}
					catch (InterruptedException e) {
						Thread.currentThread().interrupt();
						throw new IllegalStateException("Interrupted waiting for DevTools to attach", e);
					}
				}
			}
		}
	}

	/** Returns the pages and workers the browser has that aren't attached yet. Runs on the DevTools thread. */
	private @Nonnull List<String> unattachedTargets() {
		Set<String> mine = sessions.values().stream().map(Target::targetId).collect(Collectors.toSet());
		List<String> result = new ArrayList<>();
		for (Object info : (List<?>) send(null, "Target.getTargets", Map.of()).getOrDefault("targetInfos", List.of())) {
			if (info instanceof Map<?, ?> target) {
				String type = String.valueOf(target.get("type"));
				if (TOP_LEVEL_TYPES.contains(type) && (! mine.contains(String.valueOf(target.get("targetId"))))) {
					result.add(type + ' ' + target.get("url"));
				}
			}
		}
		return result;
	}

	/** Takes a new target: remembers it, applies the settings, has it attach its own children, then lets it run. */
	private void attached(@Nonnull Map<String, Object> params) {
		String sessionId = string(params, "sessionId");
		Map<String, Object> info = map(params, "targetInfo");
		Target target = new Target(string(info, "targetId"), string(info, "type"));
		try {
			if (gone.remove(sessionId)) {
				return; // it has gone already
			}
			sessions.put(sessionId, target);
			if ("tab".equals(target.type())) {
				send(sessionId, SET_AUTO_ATTACH, autoAttach(List.of("page")));
			}
			else {
				if (target.isPage()) {
					send(sessionId, "Page.enable", Map.of());
					for (String script : initScripts) {
						send(sessionId, "Page.addScriptToEvaluateOnNewDocument", Map.of("source", script));
					}
					if (touchPoints > 0) {
						send(sessionId, "Emulation.setTouchEmulationEnabled", touch());
					}
					send(sessionId, SET_AUTO_ATTACH, autoAttach(List.of("worker", "iframe")));
				}
				send(sessionId, "Network.enable", Map.of());
				send(sessionId, "Network.emulateNetworkConditions", network);
				if (! rules.isEmpty()) {
					send(sessionId, "Fetch.enable", fetchPatterns());
				}
			}
		}
		catch (RuntimeException e) {
			// It went before its settings could be applied (a closed tab, a worker that stopped at once).
			System.out.println(PREFIX + "could not set up " + target + ": " + firstLine(e));
		}
		finally {
			// A page that another page opens (window.open, a target="_blank" link) attaches saying it isn't waiting,
			// yet it is held (Chrome 158), presumably by its opener's auto-attach, which waits for the debugger though
			// its filter leaves the page out. So every target is told to run, which does nothing to one that isn't
			// waiting.
			sendQuietly(sessionId, "Runtime.runIfWaitingForDebugger", Map.of());
			synchronized (attachSignal) {
				attaches++;
				attachSignal.notifyAll();
			}
		}
	}

	/**
	 * Stops the DevTools thread and closes the connection, once the driver has quit (or the connection couldn't start).
	 * Held requests are not answered: they went with the browser.
	 */
	@Override
	@SuppressWarnings("resource") // the connection is this control's, closed here
	synchronized void release() {
		ExecutorService t = thread;
		Connection c = connection;
		thread = null;
		connection = null;
		if (t != null) {
			t.shutdownNow();
		}
		if (c != null) {
			c.close();
		}
	}

	// ---- plumbing

	/** Runs some work on the DevTools thread, connecting first if need be, and waits for it. */
	private <T> T call(@Nonnull Supplier<T> work) {
		Future<T> future = submit(work);
		try {
			return future.get(COMMAND_TIMEOUT.toSeconds() * 3, TimeUnit.SECONDS);
		}
		catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted waiting for DevTools", e);
		}
		catch (ExecutionException e) {
			if (e.getCause() instanceof RuntimeException runtime) {
				throw runtime;
			}
			throw new IllegalStateException(e.getCause());
		}
		catch (TimeoutException e) {
			future.cancel(true);
			throw new IllegalStateException("DevTools did not answer in time", e);
		}
	}

	/** Handles an event on the DevTools thread, in the order the events are queued. */
	private void listen(@Nonnull String event, @Nonnull Consumer<Map<String, Object>> handler) {
		listen(event, (sequence, params) -> handler.accept(params));
	}

	/**
	 * Handles an event on the DevTools thread, in the order the events are queued, with where it came in the order
	 * Chrome sent the events (Selenium numbers them as they arrive, before its pool of threads can reorder them).
	 */
	private void listen(@Nonnull String event, @Nonnull BiConsumer<Long, Map<String, Object>> handler) {
		ExecutorService t = thread;
		connection.addListener(new Event<>(event, ChromiumBrowserControl::read), (sequence, params) -> {
			if (! t.isShutdown()) {
				t.execute(() -> handler.accept(sequence, params));
			}
		});
	}

	/**
	 * Sends a command to each session of a kind. A session that has gone is skipped and dropped, as its detach would:
	 * it detached meanwhile, and its detach event, still to come, is then ignored. Runs on the DevTools thread.
	 */
	private void forEachSession(@Nonnull Predicate<Target> which, @Nonnull SessionCommand command) {
		for (Iterator<Entry<String, Target>> i = sessions.entrySet().iterator(); i.hasNext(); ) {
			Entry<String, Target> session = i.next();
			if (which.test(session.getValue())) {
				try {
					command.send(session.getKey());
				}
				catch (RuntimeException e) {
					if (! says(e, List.of(SESSION_NOT_FOUND))) {
						throw e;
					}
					i.remove();
					gone.add(session.getKey());
					closed(session.getValue().targetId());
				}
			}
		}
	}

	private @Nonnull String firstSession(@Nonnull Predicate<Target> which) {
		for (Entry<String, Target> session : sessions.entrySet()) {
			if (which.test(session.getValue())) {
				return session.getKey();
			}
		}
		throw new IllegalStateException("The browser has no page attached");
	}

	private @Nonnull Map<String, Object> send(@Nullable String sessionId, @Nonnull String method, @Nonnull Map<String, ?> params) {
		@SuppressWarnings("unchecked")
		Map<String, Object> p = (Map<String, Object>) params;
		Map<String, Object> result = connection.sendAndWait((sessionId == null) ? null : new SessionID(sessionId),
																new Command<Map<String, Object>>(method, p, Json.MAP_TYPE),
																COMMAND_TIMEOUT);
		return (result == null) ? Map.of() : result;
	}

	private void sendQuietly(@Nonnull String sessionId, @Nonnull String method, @Nonnull Map<String, ?> params) {
		try {
			send(sessionId, method, params);
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			// the target went
		}
	}

	/** Indicates whether a DevTools error says one of some things. */
	private static boolean says(@Nonnull RuntimeException e, @Nonnull List<String> errors) {
		String message = firstLine(e);
		for (String error : errors) {
			if (message.contains(error)) {
				return true;
			}
		}
		return false;
	}

	/** Returns the first line of an exception's message, as a DevTools error carries the whole build information after it. */
	private static @Nonnull String firstLine(@Nonnull RuntimeException e) {
		return String.valueOf(e.getMessage()).lines().findFirst().orElse("");
	}

	/** Returns the {@code Target.setAutoAttach} parameters for some target types and nothing else. */
	private static @Nonnull Map<String, Object> autoAttach(@Nonnull List<String> types) {
		List<Map<String, Object>> filter = new ArrayList<>(types.size() + 1);
		for (String type : types) {
			filter.add(Map.of("type", type));
		}
		filter.add(Map.of("exclude", Boolean.TRUE)); // and nothing else, such as browser_ui
		return Map.of("autoAttach", Boolean.TRUE, "waitForDebuggerOnStart", Boolean.TRUE, "flatten", Boolean.TRUE, "filter", filter);
	}

	@SuppressWarnings("unchecked")
	private static @Nonnull Map<String, Object> read(@Nonnull JsonInput input) {
		return (Map<String, Object>) input.read(Json.MAP_TYPE);
	}

	@SuppressWarnings("unchecked")
	private static @Nonnull Map<String, Object> map(@Nonnull Map<String, Object> params, @Nonnull String key) {
		Object result = params.get(key);
		return (result instanceof Map<?, ?>) ? (Map<String, Object>) result : Map.of();
	}

	private static @Nonnull String string(@Nonnull Map<String, Object> params, @Nonnull String key) {
		Object result = params.get(key);
		return (result == null) ? "" : result.toString();
	}

	private static @Nonnull Map<String, String> stringMap(@Nonnull Map<String, Object> map) {
		Map<String, String> result = new LinkedHashMap<>();
		for (Entry<String, Object> entry : map.entrySet()) {
			result.put(entry.getKey(), String.valueOf(entry.getValue()));
		}
		return result;
	}
}

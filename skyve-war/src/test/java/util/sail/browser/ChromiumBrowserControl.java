package util.sail.browser;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashSet;
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
 * conditions, init scripts, dialogs, the storage quota, page lifecycle and touch go through the Chrome DevTools
 * Protocol; cookies, tabs, keys and the window through plain WebDriver, as {@link WebDriverBrowserControl} does.
 * <p>
 * <b>The whole browser, not one page.</b> It attaches to every tab, every page in them, every service worker and
 * shared worker, and every dedicated worker and out-of-process frame, as they appear (DevTools
 * {@code Target.setAutoAttach}), and applies its settings to each one. This matters because the page's network
 * settings don't reach its service worker: a page set offline still gets the network through a worker's
 * {@code fetch}. A target that starts later (a new tab, a service worker Chrome starts again) waits until its settings
 * are applied. A service worker that Chrome stops and starts again keeps its DevTools session and so its settings.
 * <p>
 * <b>Lifetime.</b> It connects to DevTools as {@link SailBrowserControl#of(WebDriver)} makes it, so the request log and
 * the dialogs have what happened before a step first asks for them: a dialog already open when it attaches is never
 * reported. If it can't connect then, it tries again when a DevTools capability is used. Its settings last until the
 * browser stops; a browser started again (on the same profile, say) gets a new control, with none of them.
 * <p>
 * <b>Raw DevTools.</b> Chrome's own protocol is spoken raw over its own connection ({@link SeleniumCdpConnection}),
 * as Selenium's typed {@code selenium-devtools-vNNN} bindings lag Chrome: Chrome 158 against devtools v142 printed
 * "Unable to find CDP implementation matching 158". Nothing of the protocol leaves this class.
 * <p>
 * Ported from basix-cloud's {@code SailDevTools}.
 * <p>
 * Threading: methods may be called from any thread. Every DevTools command and event is handled in turn on one thread
 * of its own, so a target that attaches while a setting changes still ends up with the new setting.
 */
public final class ChromiumBrowserControl extends WebDriverBrowserControl
		implements RequestInterception, RequestLog, NetworkConditions, InitScripts, Dialogs, StorageQuota, PageLifecycle, TouchEmulation {
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
		private Integer status;
		private String failure;
		private boolean fromServiceWorker;

		@Nonnull LoggedRequest snapshot() {
			return new LoggedRequest(method, url, status, failure, fromServiceWorker);
		}
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

	/** The DevTools connection; guarded by {@code this}. */
	@SuppressWarnings("resource") // closed by release()
	private Connection connection;
	/** The thread every DevTools command and event runs on, made with the connection; guarded by {@code this}. */
	@SuppressWarnings("resource") // shut down by release()
	private ExecutorService thread;
	/** The sessions by session id; changed only on the DevTools thread, read anywhere. */
	private final Map<String, Target> sessions = new ConcurrentHashMap<>();

	// The settings, applied to each target as it attaches. Touched only on the DevTools thread.
	private Map<String, Object> network = networkConditions(false, 0, -1, -1);
	private int touchPoints;
	private final List<String> initScripts = new ArrayList<>();
	private final List<RequestRule> rules = new ArrayList<>();
	private final List<int[]> ruleUses = new ArrayList<>(); // how many times each rule has applied
	private final List<String> held = new ArrayList<>(); // the request ids held
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
	 * <p>This control records the dialogs from when it was made, or else from when it first connected to DevTools.
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

	@Override
	public void clearRules() {
		releaseHeld();
		call(() -> {
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
		finishHeld("Fetch.continueRequest", Map.of());
	}

	@Override
	public void failHeld(@Nonnull ErrorReason reason) {
		finishHeld("Fetch.failRequest", Map.of("errorReason", devToolsName(reason)));
	}

	private void finishHeld(@Nonnull String method, @Nonnull Map<String, Object> params) {
		call(() -> {
			for (String requestId : held) {
				Map<String, Object> p = new LinkedHashMap<>(params);
				p.put("requestId", requestId);
				toOwner(requestId, method, p);
			}
			held.clear();
			heldChanged();
			return null;
		});
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

	/** Takes a paused request: applies the first rule that matches and still has uses, or lets it carry on. */
	private void paused(@Nonnull Map<String, Object> params) {
		String requestId = string(params, "requestId");
		Map<String, Object> request = map(params, "request");
		Stage stage = (params.containsKey("responseStatusCode") || params.containsKey("responseErrorReason")) ? Stage.RESPONSE : Stage.REQUEST;
		RequestRule rule = null;
		for (int i = 0, l = rules.size(); i < l; i++) {
			RequestRule candidate = rules.get(i);
			int[] uses = ruleUses.get(i);
			if (candidate.matches(string(request, "url"), string(request, "method"), stage) &&
					((candidate.times() == 0) || (uses[0] < candidate.times()))) {
				uses[0]++;
				rule = candidate;
				break;
			}
		}
		if ((rule != null) && (rule.action() instanceof Hold)) {
			held.add(requestId);
			heldChanged();
			return;
		}
		Map<String, Object> answer = answer(requestId, params, rule, stage);
		toOwner(requestId, string(answer, "method"), map(answer, "params"));
	}

	/**
	 * Sends a command about a paused request to the session that paused it. The event doesn't say which session that
	 * is, so each is tried: the others refuse the request id.
	 *
	 * @return whether a session took it (none does if the request has gone, such as a page that gave up on it)
	 */
	private boolean toOwner(@Nonnull String requestId, @Nonnull String method, @Nonnull Map<String, Object> params) {
		for (Entry<String, Target> session : sessions.entrySet()) {
			if (session.getValue().hasNetwork()) {
				try {
					send(session.getKey(), method, params);
					return true;
				}
				catch (@SuppressWarnings("unused") RuntimeException e) {
					// not this session's request
				}
			}
		}
		System.out.println(PREFIX + "no session took " + method + " for request " + requestId);
		return false;
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
			return command(((stage == Stage.REQUEST) || (status == null)) ? "Fetch.continueRequest" : "Fetch.continueResponse",
							Map.of("requestId", requestId));
		}
		if (rule.action() instanceof Fail fail) {
			return command("Fetch.failRequest", Map.of("requestId", requestId, "errorReason", devToolsName(fail.reason())));
		}
		if (rule.action() instanceof Fulfil fulfil) {
			return command("Fetch.fulfillRequest", Map.of("requestId", requestId,
															"responseCode", Integer.valueOf(fulfil.status()),
															"responseHeaders", headerList(fulfil.headers()),
															"body", Base64.getEncoder().encodeToString(fulfil.body().getBytes(StandardCharsets.UTF_8))));
		}
		Headers headers = (Headers) rule.action();
		if (stage == Stage.REQUEST) {
			Map<String, String> original = stringMap(map(map(params, "request"), "headers"));
			return command("Fetch.continueRequest", Map.of("requestId", requestId, "headers", headerList(mergeHeaders(original, headers))));
		}
		Map<String, String> original = new LinkedHashMap<>();
		for (Object header : (List<?>) params.getOrDefault("responseHeaders", List.of())) {
			if (header instanceof Map<?, ?> h) {
				original.put(String.valueOf(h.get("name")), String.valueOf(h.get("value")));
			}
		}
		return command("Fetch.continueResponse", Map.of("requestId", requestId,
														"responseCode", status,
														"responseHeaders", headerList(mergeHeaders(original, headers))));
	}

	private static @Nonnull Map<String, Object> command(@Nonnull String method, @Nonnull Map<String, Object> params) {
		return Map.of("method", method, "params", params);
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
	 * <p>This control logs the requests from when it was made, or else from when it first connected to DevTools.
	 */
	@Override
	public @Nonnull List<LoggedRequest> requests() {
		return call(() -> requests.values().stream().map(RequestEntry::snapshot).toList());
	}

	@Override
	public void clearRequests() {
		call(() -> {
			requests.clear();
			return null;
		});
	}

	private @Nonnull RequestEntry logged(@Nonnull Map<String, Object> params) {
		return requests.computeIfAbsent(string(params, "requestId"), id -> new RequestEntry());
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
		listen("Target.detachedFromTarget", params -> sessions.remove(string(params, "sessionId")));
		listen("Fetch.requestPaused", this::paused);
		listen("Page.javascriptDialogOpening", params -> dialogs.add(new Dialog(dialogType(string(params, "type")),
																				string(params, "message"),
																				string(params, "url"))));
		listen("Network.requestWillBeSent", params -> {
			Map<String, Object> request = map(params, "request");
			RequestEntry logged = logged(params);
			logged.method = string(request, "method");
			logged.url = string(request, "url");
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
			send(null, "Target.setAutoAttach", autoAttach(List.of("tab", "service_worker", "shared_worker")));
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
		sessions.put(sessionId, target);
		try {
			if ("tab".equals(target.type())) {
				send(sessionId, "Target.setAutoAttach", autoAttach(List.of("page")));
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
					send(sessionId, "Target.setAutoAttach", autoAttach(List.of("worker", "iframe")));
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
			if (Boolean.TRUE.equals(params.get("waitingForDebugger"))) {
				sendQuietly(sessionId, "Runtime.runIfWaitingForDebugger", Map.of());
			}
			synchronized (attachSignal) {
				attaches++;
				attachSignal.notifyAll();
			}
		}
	}

	/**
	 * Lets held requests carry on, stops the DevTools thread and closes the connection, once the driver has quit (or
	 * the connection couldn't start).
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
		ExecutorService t = thread;
		connection.addListener(new Event<>(event, ChromiumBrowserControl::read), (sequence, params) -> {
			if (! t.isShutdown()) {
				t.execute(() -> handler.accept(params));
			}
		});
	}

	/** Sends a command to each session of a kind; one that detached meanwhile is skipped. Runs on the DevTools thread. */
	private void forEachSession(@Nonnull Predicate<Target> which, @Nonnull SessionCommand command) {
		for (Entry<String, Target> session : sessions.entrySet()) {
			if (which.test(session.getValue())) {
				try {
					command.send(session.getKey());
				}
				catch (RuntimeException e) {
					if (sessions.containsKey(session.getKey())) {
						throw e;
					}
					// it detached meanwhile
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

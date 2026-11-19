package util.sail.browser;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.codeborne.selenide.WebDriverRunner;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.PrimeFacesSelenide;
import util.sail.browser.Dialog.Type;
import util.sail.browser.RequestRule.ErrorReason;

/**
 * Proves {@link ChromiumBrowserControl} in a real Chrome against a small local server with a service worker, so it
 * needs no Skyve server, only Chrome. The browser starts as other SAIL browsers do, headless unless
 * {@code -Dsail.headless=false}. Run it with
 * {@code mvn -o -pl skyve-war -am test -Dtest=ChromiumBrowserControlIT -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1}.
 * <p>
 * The page ({@code page.html}) registers the worker ({@code sw.js}), which passes {@code /sw/*} requests to the network
 * with its own {@code fetch}, so those prove what reaches a service worker; it answers {@code /sw/whoami} itself with
 * the time it started, to show when Chrome starts it again. The server counts the requests it gets by path, to show
 * which reached it. Every wait is on a named event, bounded by the browser's SAIL timeout.
 */
@Tag("sail")
class ChromiumBrowserControlIT {
	/** Fetches {@code arguments[0]} (with the method {@code arguments[1]}, GET if none) and gives "status body", or "ERR" if it fails. */
	private static final String FETCH = """
			const r = await fetch(arguments[0], { cache: 'no-store', method: arguments[1] || 'GET' }).catch(() => null);
			return r ? r.status + ' ' + (await r.text()) : 'ERR';""";
	/** The requests the server got, by path. */
	private static final Map<String, AtomicInteger> HITS = new ConcurrentHashMap<>();

	private static HttpServer server;
	private static String origin;

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	private ChromiumDriver driver;
	private ChromiumBrowserControl control;

	@BeforeAll
	static void startServer() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", ChromiumBrowserControlIT::serve);
		server.start();
		origin = "http://localhost:" + server.getAddress().getPort(); // localhost is a secure context, as workers need
	}

	@AfterAll
	static void stopServer() {
		server.stop(0);
	}

	private static void serve(@Nonnull HttpExchange exchange) throws IOException {
		String path = exchange.getRequestURI().getPath();
		HITS.computeIfAbsent(path, p -> new AtomicInteger()).incrementAndGet();
		String type = "text/plain";
		byte[] body;
		if ("/".equals(path) || "/away".equals(path)) {
			type = "text/html; charset=utf-8";
			body = fixture("page.html");
		}
		else if ("/sw.js".equals(path)) {
			type = "text/javascript";
			body = fixture("sw.js");
		}
		else if (path.endsWith("/echo")) {
			body = String.valueOf(exchange.getRequestHeaders().getFirst("X-Test")).getBytes(StandardCharsets.UTF_8);
			exchange.getResponseHeaders().add("X-Server", "yes");
		}
		else if ("/cookies".equals(path)) {
			exchange.getResponseHeaders().add("Set-Cookie", "kept=1; Path=/; Max-Age=3600");
			exchange.getResponseHeaders().add("Set-Cookie", "session=1; Path=/");
			exchange.getResponseHeaders().add("Set-Cookie", "secret=1; Path=/; HttpOnly");
			body = "set".getBytes(StandardCharsets.UTF_8);
		}
		else {
			body = "real".getBytes(StandardCharsets.UTF_8);
		}
		exchange.getResponseHeaders().add("Content-Type", type);
		exchange.getResponseHeaders().add("Cache-Control", "no-store");
		exchange.sendResponseHeaders(200, body.length);
		try (OutputStream out = exchange.getResponseBody()) {
			out.write(body);
		}
	}

	private static @Nonnull byte[] fixture(@Nonnull String name) throws IOException {
		try (InputStream in = ChromiumBrowserControlIT.class.getResourceAsStream(name)) {
			if (in == null) {
				throw new IOException("No fixture " + name + " beside " + ChromiumBrowserControlIT.class.getName());
			}
			return in.readAllBytes();
		}
	}

	@BeforeEach
	void startBrowser() {
		HITS.clear();
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(origin + '/'));
		driver = assertInstanceOf(ChromiumDriver.class, WebDriverRunner.getWebDriver(), "Chrome");
		driver.get(origin + '/');
		js("return window.serviceWorkerReady;"); // the event: the service worker controls the page
		control = assertInstanceOf(ChromiumBrowserControl.class, SailBrowserControl.of(driver));
	}

	@AfterEach
	void stopBrowser() {
		selenium.stopBrowser();
	}

	private Object js(@Nonnull String script, Object... args) {
		return driver.executeScript(script, args);
	}

	private @Nonnull String fetch(@Nonnull String path) {
		return String.valueOf(js("return (async () => { " + FETCH + " })();", path));
	}

	private @Nonnull String post(@Nonnull String path) {
		return String.valueOf(js("return (async () => { " + FETCH + " })();", path, "POST"));
	}

	/** Starts a fetch and leaves it running; {@code return window.pending;} waits for its answer. */
	private void startFetch(@Nonnull String path) {
		js("window.pending = (async () => { " + FETCH + " })();", path);
	}

	private static int hits(@Nonnull String path) {
		AtomicInteger result = HITS.get(path);
		return (result == null) ? 0 : result.get();
	}

	/**
	 * Waits for something the control records from DevTools events, which may arrive after the page has its answer.
	 *
	 * @param event	what is waited for, for the message
	 * @param condition	true once it has happened
	 */
	private void awaitControl(@Nonnull String event, @Nonnull Predicate<ChromiumBrowserControl> condition) {
		new FluentWait<>(control).withTimeout(selenium.timeout())
									.pollingEvery(SailBrowserControl.POLLING)
									.withMessage(event)
									.until(c -> Boolean.valueOf(condition.test(c)));
	}

	/**
	 * Waits until a script in the page in front gives a value.
	 *
	 * @param script	the script
	 * @param value	the value it gives once the event has happened
	 */
	private void awaitScript(@Nonnull String script, @Nonnull Object value) {
		new WebDriverWait(driver, selenium.timeout(), SailBrowserControl.POLLING)
				.withMessage(script + " to give " + value)
				.until(d -> Boolean.valueOf(value.equals(((ChromiumDriver) d).executeScript(script))));
	}

	// ---- network conditions

	@Test
	void offlineReachesThePageAndTheServiceWorker() {
		assertEquals("200 real", fetch("/sw/data"), "online, through the worker");
		control.offline();
		assertAll(() -> assertEquals("ERR", fetch("/data"), "the page's own request"),
					() -> assertEquals("ERR", fetch("/sw/data"), "the worker's request"),
					() -> assertEquals(Boolean.FALSE, js("return navigator.onLine;"), "navigator.onLine"));
		control.online();
		assertAll(() -> assertEquals("200 real", fetch("/data"), "the page back online"),
					() -> assertEquals("200 real", fetch("/sw/data"), "the worker back online"));
	}

	@Test
	void aServiceWorkerChromeStartsAgainStaysOffline() {
		control.offline();
		String before = fetch("/sw/whoami");
		driver.executeCdpCommand("ServiceWorker.enable", Map.of());
		driver.executeCdpCommand("ServiceWorker.stopAllWorkers", Map.of());
		String after = fetch("/sw/whoami");
		assertAll(() -> assertNotEquals(before, after, "Chrome started the worker again"),
					() -> assertEquals("ERR", fetch("/sw/data"), "and it is still offline"));
	}

	@Test
	void throttleSlowsThePageAndTheWorker() {
		String timed = """
				return (async () => {
					const start = performance.now();
					await fetch(arguments[0], { cache: 'no-store' });
					return performance.now() - start;
				})();""";
		control.throttle(Duration.ofMillis(400), -1, -1);
		double page = ((Number) js(timed, "/data")).doubleValue();
		double worker = ((Number) js(timed, "/sw/data")).doubleValue();
		control.online();
		// a lower bound only: a fetch from localhost takes a few milliseconds unthrottled
		assertAll(() -> assertTrue(page >= 350, "the page's request took " + page + " ms"),
					() -> assertTrue(worker >= 350, "the worker's request took " + worker + " ms"));
	}

	@Test
	void aNewTabGetsTheSettings() {
		control.offline();
		control.emulateTouch(5);
		control.addInitScript("window.probe = 'here';");
		driver.switchTo().newWindow(WindowType.TAB);
		control.online(); // to load the page; the new tab must take this too
		driver.get(origin + '/');
		assertAll(() -> assertEquals("200 real", fetch("/data"), "online"),
					() -> assertEquals(Boolean.TRUE, js("return matchMedia('(pointer: coarse)').matches;"), "touch"),
					() -> assertEquals("here", js("return window.probe;"), "the init script"));
		control.offline();
		assertEquals("ERR", fetch("/data"), "offline in the new tab");
	}

	/**
	 * A tab the page opens itself (window.open, a link with target _blank) runs, and gets the settings: Chrome holds such
	 * a page for the debugger though it attaches saying it isn't waiting.
	 */
	@Test
	void aTabThePageOpensRunsWithTheSettings() {
		control.addInitScript("window.probe = 'here';");
		js("setTimeout(() => window.open('/away', '_blank'), 0);");
		driver.manage().timeouts().scriptTimeout(Duration.ofSeconds(5)); // a held page never answers: fail quickly
		String opened = control.visitNewTab(selenium.timeout(), true, () -> {
			awaitScript("return document.readyState;", "complete");
			return js("return location.pathname + ' ' + window.probe;").toString();
		});
		assertEquals("/away here", opened);
	}

	// ---- request interception

	@Test
	void failOnceThenCarryOnAndByMethod() {
		control.addRule(RequestRule.fail("*/data*", ErrorReason.CONNECTION_REFUSED).times(1));
		control.addRule(RequestRule.fail("*/data*", ErrorReason.FAILED).method("POST"));
		assertAll(() -> assertEquals("ERR", fetch("/data"), "failed once"),
					() -> assertEquals("200 real", fetch("/data"), "used up"),
					() -> assertEquals("ERR", post("/data"), "a POST, by the second rule"),
					() -> assertEquals(1, hits("/data"), "only the request that carried on reached the server"));
		control.clearRules();
		assertEquals("200 real", post("/data"), "no rules");
	}

	@Test
	void heldRequestsWaitUntilReleasedOrFailed() {
		control.addRule(RequestRule.hold("*/data*"));
		startFetch("/data");
		control.awaitHeld(1, selenium.timeout());
		assertAll(() -> assertEquals(1, control.heldCount(), "held"),
					() -> assertEquals(0, hits("/data"), "held before it is sent"));
		control.releaseHeld();
		assertEquals("200 real", js("return window.pending;"), "released");

		startFetch("/data");
		control.awaitHeld(1, selenium.timeout());
		control.failHeld(ErrorReason.TIMED_OUT);
		assertAll(() -> assertEquals("ERR", js("return window.pending;"), "failed"),
					() -> assertEquals(0, control.heldCount(), "none held"),
					() -> assertEquals(1, hits("/data"), "only the released request reached the server"));
	}

	/** A wait for held requests wakes when one is held after it began, and fails at its time when none is. */
	@Test
	void awaitingHeldRequestsWakesForOneHeldLater() {
		control.addRule(RequestRule.hold("*/data*"));
		// the page's own late action under test: it sends the request only after the wait has begun
		js("setTimeout(() => { window.pending = (async () => { " + FETCH + " })(); }, 500);", "/data?late");
		control.awaitHeld(1, selenium.timeout());
		assertAll(() -> assertEquals(1, control.heldCount(), "held"),
					() -> assertEquals(0, hits("/data"), "held before it is sent"));
		control.releaseHeld();
		assertEquals("200 real", js("return window.pending;"), "released");
		Duration shortly = Duration.ofMillis(200);
		AssertionError none = assertThrows(AssertionError.class, () -> control.awaitHeld(1, shortly));
		assertEquals("Expected 1 held request(s) within PT0.2S, but 0 were held", none.getMessage());
	}

	/**
	 * A held request the page gives up on (it reloaded) is no longer counted as held, and clearing the rules lets the
	 * held requests go and leaves none held.
	 */
	@Test
	void heldRequestsAreForgottenWhenThePageGivesUpOrTheRulesAreCleared() {
		control.addRule(RequestRule.hold("*/data*"));
		startFetch("/data?first");
		control.awaitHeld(1, selenium.timeout());
		driver.navigate().refresh();
		awaitControl("the reloaded page's held request no longer held", c -> c.heldCount() == 0);
		startFetch("/data?second");
		control.awaitHeld(1, selenium.timeout());
		control.clearRules();
		assertAll(() -> assertEquals(0, control.heldCount(), "none held once the rules are cleared"),
					() -> assertEquals("200 real", js("return window.pending;"), "the held request carried on"),
					() -> assertEquals(1, hits("/data"), "only the second reached the server"));
	}

	@Test
	void fulfilAtTheRequestAndTheResponse() {
		control.addRule(RequestRule.fulfil("*/data*", 503, Map.of("Content-Type", "text/plain"), "busy").times(1));
		control.addRule(RequestRule.fulfil("*/data*", 500, Map.of(), "broken").atResponse());
		assertEquals("503 busy", fetch("/data"), "answered before the server");
		assertEquals(0, hits("/data"), "the server didn't get it");
		assertEquals("500 broken", fetch("/data"), "the server's answer replaced");
		assertEquals(1, hits("/data"), "the server got it");
	}

	@Test
	void requestAndResponseHeadersCanBeChanged() {
		control.addRule(RequestRule.headers("*/echo*", Map.of("X-Test", "changed"), Set.of()));
		control.addRule(RequestRule.headers("*/echo*", Map.of("X-Added", "1"), Set.of("x-server")).atResponse());
		Object seen = js("""
				return fetch('/echo', { headers: { 'X-Test': 'sent' } })
						.then(async r => (await r.text()) + ' ' + r.headers.get('X-Added') + ' ' + r.headers.get('X-Server'));""");
		assertEquals("changed 1 null", seen, "the server saw the changed request header; the page the changed response headers");
	}

	@Test
	void aLostResponseWasHandledByTheServer() {
		control.addRule(RequestRule.loseResponse("*/save*", ErrorReason.CONNECTION_RESET).method("POST"));
		assertEquals("ERR", post("/save"), "the page got no answer");
		assertEquals(1, hits("/save"), "the server handled it");
		assertEquals("200 real", fetch("/save"), "a GET: not the rule's method");
	}

	@Test
	void aWorkerForwardedRequestIsIntercepted() {
		control.addRule(RequestRule.fulfil("*/sw/data*", 503, Map.of("Content-Type", "text/plain"), "busy").times(1));
		assertEquals("503 busy", fetch("/sw/data"), "fulfilled in the worker's session");
		assertEquals(0, hits("/sw/data"), "the server didn't get it");
		control.addRule(RequestRule.hold("*/sw/data*"));
		startFetch("/sw/data");
		control.awaitHeld(1, selenium.timeout());
		assertEquals(0, hits("/sw/data"), "held in the worker's session");
		control.releaseHeld();
		assertEquals("200 real", js("return window.pending;"), "released to the server");
	}

	// ---- request log

	@Test
	void requestsAreLoggedWithTheirServiceWorkerCopies() {
		control.clearRequests();
		fetch("/data?logged");
		fetch("/sw/data?logged");
		awaitControl("the page's request, logged with its status", c -> logged(c, r -> r.url().endsWith("/data?logged") &&
																					(! r.url().contains("/sw/")) &&
																					Integer.valueOf(200).equals(r.status()) &&
																					(! r.fromServiceWorker())));
		awaitControl("the page's copy of the request its worker answered", c -> logged(c, r -> r.url().endsWith("/sw/data?logged") &&
																							Integer.valueOf(200).equals(r.status()) &&
																							r.fromServiceWorker()));
		awaitControl("the worker's own request", c -> logged(c, r -> r.url().endsWith("/sw/data?logged") &&
																	Integer.valueOf(200).equals(r.status()) &&
																	(! r.fromServiceWorker())));
		control.offline();
		fetch("/data?failed");
		awaitControl("the failed request, logged with its failure", c -> logged(c, r -> r.url().endsWith("/data?failed") &&
																					(r.failure() != null) &&
																					(r.status() == null)));
		fetch("/sw/data?failed");
		// Chrome gives the page's copy no answer, so nothing marks it as the worker's: CheckRequests documents this
		awaitControl("a failed request through the worker, logged twice, as neither is marked",
						c -> c.requests().stream().filter(r -> r.url().endsWith("/sw/data?failed") && (r.failure() != null) && (! r.fromServiceWorker()))
													.count() == 2);
		assertTrue(control.requests().stream().allMatch(r -> "GET".equals(r.method())), "methods: " + control.requests());
	}

	private static boolean logged(@Nonnull ChromiumBrowserControl c, @Nonnull Predicate<LoggedRequest> matching) {
		return c.requests().stream().anyMatch(matching);
	}

	// ---- init scripts

	@Test
	void initScriptsRunInEveryNewDocumentOnce() {
		String script = "window.probe = location.pathname; window.runs = (window.runs || 0) + 1;";
		control.addInitScript(script);
		control.addInitScript(script);
		assertNull(js("return window.probe;"), "not in the open document");
		driver.get(origin + "/away");
		assertAll(() -> assertEquals("/away", js("return window.probe;"), "after a navigation"),
					() -> assertEquals(Long.valueOf(1), js("return window.runs;"), "added once"));
		driver.navigate().refresh();
		assertAll(() -> assertEquals("/away", js("return window.probe;"), "after a reload"),
					() -> assertEquals(Long.valueOf(1), js("return window.runs;"), "once after a reload"));
	}

	// ---- dialogs

	@Test
	void dialogsAreRecordedAndAnswered() {
		js("setTimeout(() => { window.answer = confirm('Sure?'); }, 0);");
		awaitControl("the confirm dialog", c -> c.dialogs().size() == 1);
		assertEquals(new Dialog(Type.CONFIRM, "Sure?", origin + '/'), control.dialogs().get(0));
		control.answerDialog(false, null);
		assertEquals(Boolean.FALSE, js("return window.answer;"), "Cancel");

		js("setTimeout(() => { window.answer = prompt('Name?', 'default'); }, 0);");
		awaitControl("the prompt dialog", c -> c.dialogs().size() == 2);
		control.answerDialog(true, "typed");
		assertAll(() -> assertEquals(Type.PROMPT, control.dialogs().get(1).type(), "a prompt"),
					() -> assertEquals("typed", js("return window.answer;"), "the text answered"),
					() -> assertThrows(IllegalStateException.class, () -> control.answerDialog(true, null), "none open"));

		control.clearDialogs();
		js("window.addEventListener('beforeunload', (e) => { e.preventDefault(); e.returnValue = ''; });");
		driver.findElement(By.tagName("body")).click(); // Chrome asks only after the user has used the page
		driver.findElement(By.id("away")).click();
		// ChromeDriver accepts a beforeunload dialog at once, so the record is the only way to see it
		awaitControl("the beforeunload dialog", c -> ! c.dialogs().isEmpty());
		assertEquals(Type.BEFOREUNLOAD, control.dialogs().get(0).type());
		control.clearDialogs();
		assertTrue(control.dialogs().isEmpty(), "cleared");
	}

	// ---- cookies

	@Test
	void cookiesAreListedAndDeletedHttpOnlyIncluded() {
		fetch("/cookies");
		List<BrowserCookie> cookies = control.cookies();
		assertAll(() -> assertTrue(cookies.stream().anyMatch(c -> "kept".equals(c.name()) && (! c.session())), "kept: " + cookies),
					() -> assertTrue(cookies.stream().anyMatch(c -> "session".equals(c.name()) && c.session()), "session: " + cookies),
					() -> assertTrue(cookies.stream().anyMatch(c -> "secret".equals(c.name())), "secret: " + cookies),
					() -> assertFalse(String.valueOf(js("return document.cookie;")).contains("secret"), "secret is HttpOnly"));
		assertEquals(1, control.deleteCookies("secret"));
		assertFalse(control.cookies().stream().anyMatch(c -> "secret".equals(c.name())), "deleted");
	}

	// ---- storage quota

	@Test
	void theQuotaIsEnforcedOnWrites() {
		// random bytes in CacheStorage, which doesn't compress them as IndexedDB might
		String store = """
				const bytes = new Uint8Array(3000000);
				for (let i = 0; i < bytes.length; i += 65536) { crypto.getRandomValues(bytes.subarray(i, i + 65536)); }
				return caches.open('quota').then(c => c.put('/big' + Math.random(), new Response(bytes)))
						.then(() => 'stored', (e) => e.name);""";
		control.overrideQuota(origin, 1_000_000);
		assertEquals("QuotaExceededError", js(store), "3 MB into a 1 MB quota");
		control.clearQuotaOverride(origin);
		assertEquals("stored", js(store), "the real quota back");
	}

	// ---- page lifecycle and touch

	@Test
	void aTabIsFrozenAndResumed() {
		String first = driver.getWindowHandle();
		control.openTab(origin + "/away");
		control.freeze(first);
		// the event: the frozen page's freeze handler, writing to the storage this tab shares
		awaitScript("return localStorage.getItem('lifecycle/');", "frozen");
		control.resume(first);
		awaitScript("return localStorage.getItem('lifecycle/');", "resumed");
		driver.switchTo().window(first);
		assertEquals("/", js("return location.pathname;"), "the resumed tab answers the driver");
	}

	@Test
	void touchMakesThePointerCoarse() {
		assertEquals(Boolean.FALSE, js("return matchMedia('(pointer: coarse)').matches;"), "a mouse at first");
		control.emulateTouch(5);
		assertEquals("true 5", js("return matchMedia('(pointer: coarse)').matches + ' ' + navigator.maxTouchPoints;"));
		driver.navigate().refresh();
		assertEquals(Boolean.TRUE, js("return matchMedia('(pointer: coarse)').matches;"), "after a reload");
		control.emulateTouch(0);
		assertEquals(Boolean.FALSE, js("return matchMedia('(pointer: coarse)').matches;"), "off again");
	}
}

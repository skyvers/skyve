package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;
import util.AbstractInterpretedSailWorkflow;

/**
 * Runs the standard calls from SAIL scripts, through {@code <call className="util.sail.call.…">}, against a small local
 * server with a service worker (the browser control fixtures {@code page.html} and {@code sw.js}), so it needs no
 * Skyve server, only a browser. The scripts have no sign-in: calls, script steps and navigations to the local server.
 * <p>
 * The first test uses only what every browser has (cookies, the window, keys and tabs), so it passes in any browser.
 * Each other test starts with a call that needs a Chromium capability (or a device profile), so in another browser it
 * reports skipped, with the call, what it needs and the browser in the message; in Chrome or Edge it passes. Run it with
 * {@code mvn -o -pl skyve-war -am test -Dtest=StandardCallsSailIT -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1},
 * and add {@code -Dsail.browser=firefox} for Firefox, or {@code -Dsail.headless=false} to watch.
 * <p>
 * Device profiles go in a temporary folder, deleted afterwards. The server counts the requests it gets by path, to show
 * which reached it.
 */
@Tag("sail")
class StandardCallsSailIT extends AbstractInterpretedSailWorkflow {
	/** The requests the server got, by path. */
	private static final Map<String, AtomicInteger> HITS = new ConcurrentHashMap<>();

	private static HttpServer server;
	private static String origin;
	private static Path profiles;

	@BeforeAll
	static void startServer() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", StandardCallsSailIT::serve);
		server.start();
		origin = "http://localhost:" + server.getAddress().getPort(); // localhost is a secure context, as workers need
		profiles = Files.createTempDirectory("sail-standard-calls");
	}

	@AfterAll
	static void stopServer() throws IOException {
		server.stop(0);
		try (Stream<Path> files = Files.walk(profiles)) {
			for (Path file : files.sorted(Comparator.reverseOrder()).toList()) {
				Files.delete(file);
			}
		}
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
		else if ("/echo".equals(path)) {
			body = String.valueOf(exchange.getRequestHeaders().getFirst("X-Test")).getBytes(StandardCharsets.UTF_8);
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
		try (InputStream in = StandardCallsSailIT.class.getResourceAsStream("/util/sail/browser/" + name)) {
			if (in == null) {
				throw new IOException("No browser control fixture " + name);
			}
			return in.readAllBytes();
		}
	}

	private static int hits(@Nonnull String path) {
		AtomicInteger result = HITS.get(path);
		return (result == null) ? 0 : result.get();
	}

	@BeforeEach
	void useTheLocalServer() {
		HITS.clear();
		settings().baseUrl(origin + '/').variable("origin", origin).profiles(profiles);
	}

	/** Returns a script of one automation and interaction, in a PrimeFaces executor, that asserts. */
	private static @Nonnull String automation(@Nonnull String interaction, @Nonnull String steps) {
		return "<automation uxui=\"external\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"" + interaction + "\"><method>" + steps + "</method></interaction></automation>";
	}

	/** Cookies, the window, keys and tabs: plain WebDriver has them, so this passes in every browser. */
	@Test
	void theCallsEveryBrowserHasRun() {
		sailWorkflowString(automation("every browser", """
				<navigateLink href="{var:origin}/" />
				<executeAsync variable="set"><![CDATA[ return (await fetch('/cookies', {cache: 'no-store'})).text(); ]]></executeAsync>
				<call className="util.sail.call.DeleteCookies" variable="deleted">
					<parameter name="names" value="kept, secret" />
				</call>
				<testScript message="DeleteCookies deleted kept and the HttpOnly secret, and left session"><![CDATA[
					return sail.vars.deleted === '2' && ! document.cookie.includes('kept') && document.cookie.includes('session');
				]]></testScript>

				<call className="util.sail.call.ResizeWindow" variable="size">
					<parameter name="width" value="900" />
					<parameter name="height" value="700" />
				</call>
				<testScript message="ResizeWindow resized the window"><![CDATA[
					return sail.vars.size === '900x700' && window.outerWidth === 900;
				]]></testScript>

				<execute><![CDATA[
					window.keys = [];
					document.addEventListener('keydown', (e) => window.keys.push(e.key + (e.isTrusted ? '' : ' (untrusted)')));
					document.body.tabIndex = -1;
					document.body.focus();
				]]></execute>
				<call className="util.sail.call.PressKey">
					<parameter name="key" value="ESCAPE" />
					<parameter name="count" value="2" />
				</call>
				<testScript message="PressKey pressed Escape twice, as a keyboard does"><![CDATA[
					return window.keys.join() === 'Escape,Escape';
				]]></testScript>

				<call className="util.sail.call.SwitchTab" variable="tabs">
					<parameter name="action" value="open" />
					<parameter name="url" value="away" />
				</call>
				<testScript message="SwitchTab opened a tab in front, relative to the base URL"><![CDATA[
					return sail.vars.tabs === '2' && location.pathname === '/away';
				]]></testScript>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="switch" />
					<parameter name="index" value="0" />
				</call>
				<testScript message="SwitchTab switched back to the first tab"><![CDATA[ return location.pathname === '/'; ]]></testScript>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="switch" />
					<parameter name="index" value="1" />
				</call>
				<call className="util.sail.call.SwitchTab" variable="tabs">
					<parameter name="action" value="close" />
				</call>
				<testScript message="SwitchTab closed the tab in front, and the first came back"><![CDATA[
					return sail.vars.tabs === '1' && location.pathname === '/';
				]]></testScript>

				<execute><![CDATA[ setTimeout(() => window.open('/away?late', '_blank'), 500); ]]></execute>
				<call className="util.sail.call.SwitchTab" variable="tabs">
					<parameter name="action" value="switch" />
					<parameter name="index" value="1" />
				</call>
				<testScript message="SwitchTab waited for the tab the page opened later"><![CDATA[
					return sail.vars.tabs === '2' && location.search === '?late';
				]]></testScript>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="close" />
				</call>

				<execute><![CDATA[ setTimeout(() => window.open('/away?checked', '_blank'), 500); ]]></execute>
				<call className="util.sail.call.CheckOpenedTab" variable="opened">
					<parameter name="script" value="return location.search === '?checked';" />
				</call>
				<testScript message="CheckOpenedTab visited the new tab, closed it and came back"><![CDATA[
					return sail.vars.opened.endsWith('/away?checked') && location.pathname === '/';
				]]></testScript>
				"""));
	}

	/**
	 * Offline, online and throttling reach the page and its service worker (Chromium). It starts with {@link Online}, which
	 * changes nothing in a fresh browser, so another browser skips before waiting for the worker.
	 */
	@Test
	void theNetworkCallsReachThePageAndItsWorker() {
		sailWorkflowString(automation("network", """
				<navigateLink href="{var:origin}/" />
				<call className="util.sail.call.Online" />
				<waitFor><![CDATA[ return await window.serviceWorkerReady; ]]></waitFor>
				<call className="util.sail.call.Offline" />
				<testScript message="Offline: neither the page nor its worker reaches the server"><![CDATA[
					const page = await fetch('/data', {cache: 'no-store'}).then(r => r.status, () => 'ERR');
					const worker = await fetch('/sw/data', {cache: 'no-store'}).then(r => r.status, () => 'ERR');
					return navigator.onLine === false && page === 'ERR' && worker === 'ERR';
				]]></testScript>
				<call className="util.sail.call.Online" />
				<testScript message="Online: the worker reaches the server again"><![CDATA[
					return navigator.onLine && (await fetch('/sw/data', {cache: 'no-store'})).status === 200;
				]]></testScript>
				<call className="util.sail.call.Throttle">
					<parameter name="latencyMillis" value="400" />
				</call>
				<testScript message="Throttle slowed the page's requests"><![CDATA[
					const start = performance.now();
					await fetch('/data?' + Math.random(), {cache: 'no-store'});
					return performance.now() - start >= 400;
				]]></testScript>
				<call className="util.sail.call.Online" />
				"""));
		assertEquals(1, hits("/sw/data"), "the worker's requests reached the server only once back online");
	}

	/** Requests are failed, held and released or failed, answered, lost and rewritten, then the rules cleared (Chromium). */
	@Test
	void theRequestRulesApplyUntilCleared() {
		sailWorkflowString(automation("request rules", """
				<navigateLink href="{var:origin}/" />
				<call className="util.sail.call.FailRequests">
					<parameter name="url" value="*/fail*" />
					<parameter name="times" value="1" />
				</call>
				<testScript message="FailRequests failed the first request only"><![CDATA[
					const first = await fetch('/fail', {cache: 'no-store'}).then(r => r.status, () => 'ERR');
					const second = await fetch('/fail', {cache: 'no-store'}).then(r => r.status, () => 'ERR');
					return first === 'ERR' && second === 200;
				]]></testScript>

				<call className="util.sail.call.HoldRequests">
					<parameter name="url" value="*/held*" />
				</call>
				<execute><![CDATA[ window.held = fetch('/held', {cache: 'no-store'}).then(r => r.text(), () => 'ERR'); ]]></execute>
				<call className="util.sail.call.AwaitHeldRequests" variable="held" />
				<call className="util.sail.call.ReleaseRequests" />
				<testScript message="AwaitHeldRequests saw the request held, and ReleaseRequests let it reach the server"><![CDATA[
					return sail.vars.held === '1' && (await window.held) === 'real';
				]]></testScript>
				<execute><![CDATA[ window.held = fetch('/held', {cache: 'no-store'}).then(r => r.text(), () => 'ERR'); ]]></execute>
				<call className="util.sail.call.AwaitHeldRequests" />
				<call className="util.sail.call.ReleaseRequests">
					<parameter name="fail" value="TimedOut" />
				</call>
				<testScript message="ReleaseRequests failed the held request"><![CDATA[ return (await window.held) === 'ERR'; ]]></testScript>

				<call className="util.sail.call.RespondToRequests">
					<parameter name="url" value="*/answer*" />
					<parameter name="status" value="503" />
					<parameter name="body" value="busy" />
					<parameter name="contentType" value="text/plain" />
				</call>
				<testScript message="RespondToRequests answered without the server"><![CDATA[
					const r = await fetch('/answer', {cache: 'no-store'});
					return r.status === 503 && (await r.text()) === 'busy';
				]]></testScript>

				<call className="util.sail.call.LoseResponses">
					<parameter name="url" value="*/lost*" />
				</call>
				<testScript message="LoseResponses lost the server's answer"><![CDATA[
					return (await fetch('/lost', {cache: 'no-store'}).then(r => r.status, () => 'ERR')) === 'ERR';
				]]></testScript>

				<call className="util.sail.call.RewriteRequestHeaders">
					<parameter name="url" value="*/echo*" />
					<parameter name="set" value="X-Test=rewritten" />
				</call>
				<testScript message="RewriteRequestHeaders set the request's header"><![CDATA[
					return (await (await fetch('/echo', {cache: 'no-store'})).text()) === 'rewritten';
				]]></testScript>

				<call className="util.sail.call.ClearRequestRules" />
				<testScript message="ClearRequestRules removed every rule"><![CDATA[
					const answer = await fetch('/answer', {cache: 'no-store'});
					return answer.status === 200 && (await answer.text()) === 'real';
				]]></testScript>
				"""));
		assertAll(() -> assertEquals(1, hits("/fail"), "/fail: the second only"),
					() -> assertEquals(1, hits("/held"), "/held: the released one only"),
					() -> assertEquals(1, hits("/answer"), "/answer: once the rules were cleared"),
					() -> assertEquals(1, hits("/lost"), "/lost: the server handled it"));
	}

	/** The request log is counted, waited on and read (Chromium). */
	@Test
	void theRequestChecksCountWaitAndRead() {
		sailWorkflowString(automation("request checks", """
				<navigateLink href="{var:origin}/" />
				<call className="util.sail.call.RequestLog">
					<parameter name="clear" value="true" />
				</call>
				<waitFor><![CDATA[ return await window.serviceWorkerReady; ]]></waitFor>
				<executeAsync variable="fetched"><![CDATA[
					await fetch('/sw/counted', {cache: 'no-store'});
					await fetch('/plain', {cache: 'no-store'});
					return 'fetched';
				]]></executeAsync>
				<call className="util.sail.call.CheckRequests" variable="once">
					<parameter name="matching" value="GET .*/sw/counted$" />
					<parameter name="exactly" value="1" />
					<parameter name="waitMillis" value="5000" />
				</call>
				<call className="util.sail.call.CheckRequests" variable="twice">
					<parameter name="matching" value="GET .*/sw/counted$" />
					<parameter name="ignoreServiceWorkerCopies" value="false" />
					<parameter name="exactly" value="2" />
					<parameter name="waitMillis" value="5000" />
				</call>

				<execute><![CDATA[ setTimeout(() => fetch('/late', {cache: 'no-store'}), 500); ]]></execute>
				<call className="util.sail.call.CheckRequests" variable="late">
					<parameter name="matching" value="GET .*/late$" />
					<parameter name="waitMillis" value="10000" />
				</call>

				<call className="util.sail.call.FailRequests">
					<parameter name="url" value="*/broken*" />
				</call>
				<executeAsync variable="broken"><![CDATA[
					return await fetch('/broken', {cache: 'no-store'}).then(() => 'answered', () => 'ERR');
				]]></executeAsync>
				<call className="util.sail.call.CheckRequests">
					<parameter name="matching" value="/broken$" />
					<parameter name="failed" value="true" />
					<parameter name="exactly" value="1" />
					<parameter name="waitMillis" value="5000" />
				</call>
				<call className="util.sail.call.ClearRequestRules" />

				<call className="util.sail.call.RequestLog" variable="log">
					<parameter name="matching" value="GET .*/plain$" />
				</call>
				<testScript message="CheckRequests counted the requests, and RequestLog gave them as JSON"><![CDATA[
					const log = JSON.parse(sail.vars.log);
					return sail.vars.once === '1' && sail.vars.twice === '2' && sail.vars.late === '1' && sail.vars.broken === 'ERR' &&
							log.length === 1 && log[0].method === 'GET' && log[0].status === 200 && log[0].fromServiceWorker === false;
				]]></testScript>
				"""));
	}

	/** Quota, a blocked IndexedDB, a shifted clock, a dialog and a frozen tab (Chromium). */
	@Test
	void theBrowserCallsChangeWhatThePageSees() {
		String store = """
				const bytes = new Uint8Array(3000000);
				for (let i = 0; i < bytes.length; i += 65536) { crypto.getRandomValues(bytes.subarray(i, i + 65536)); }
				return await caches.open('quota').then(c => c.put('/big' + Math.random(), new Response(bytes))).then(() => 'stored', (e) => e.name);""";
		sailWorkflowString(automation("browser", """
				<navigateLink href="{var:origin}/" />
				<call className="util.sail.call.LowerQuota" variable="quota">
					<parameter name="headroomKb" value="500" />
				</call>
				<executeAsync variable="lowered"><![CDATA[ %1$s ]]></executeAsync>
				<call className="util.sail.call.LowerQuota" variable="restored">
					<parameter name="restore" value="true" />
				</call>
				<executeAsync variable="real"><![CDATA[ %1$s ]]></executeAsync>
				<testScript message="LowerQuota left 500 KB, too little for 3 MB, then put the real quota back"><![CDATA[
					return sail.vars.lowered === 'QuotaExceededError' && sail.vars.restored === 'restored' && sail.vars.real === 'stored';
				]]></testScript>

				<call className="util.sail.call.BlockIndexedDb" />
				<call className="util.sail.call.ShiftClock">
					<parameter name="days" value="2" />
				</call>
				<testScript message="Neither changes the page open now"><![CDATA[
					return window.indexedDB !== undefined && Math.abs(Date.now() - (performance.timeOrigin + performance.now())) < 60000;
				]]></testScript>
				<navigateLink href="{var:origin}/away" />
				<testScript message="In the next page, IndexedDB is blocked and the clock is two days ahead"><![CDATA[
					const shift = Date.now() - (performance.timeOrigin + performance.now());
					return window.indexedDB === undefined && shift > 1.9 * 86400000 && shift < 2.1 * 86400000;
				]]></testScript>

				<execute><![CDATA[
					window.addEventListener('beforeunload', (e) => { e.preventDefault(); e.returnValue = ''; });
					document.body.tabIndex = -1;
					document.body.focus();
				]]></execute>
				<call className="util.sail.call.PressKey">
					<parameter name="key" value="ENTER" />
				</call>
				<navigateLink href="{var:origin}/" />
				<call className="util.sail.call.CheckDialog" />
				<call className="util.sail.call.CheckDialog">
					<parameter name="type" value="alert" />
					<parameter name="expected" value="false" />
				</call>

				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="open" />
					<parameter name="url" value="away" />
				</call>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="switch" />
					<parameter name="index" value="0" />
				</call>
				<call className="util.sail.call.FreezeTab">
					<parameter name="action" value="freeze" />
					<parameter name="index" value="1" />
				</call>
				<waitFor><![CDATA[ return localStorage.getItem('lifecycle/away') === 'frozen'; ]]></waitFor>
				<call className="util.sail.call.FreezeTab">
					<parameter name="action" value="resume" />
					<parameter name="index" value="1" />
				</call>
				<waitFor><![CDATA[ return localStorage.getItem('lifecycle/away') === 'resumed'; ]]></waitFor>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="switch" />
					<parameter name="index" value="1" />
				</call>
				<testScript message="The resumed tab answers"><![CDATA[ return location.pathname === '/away'; ]]></testScript>
				<call className="util.sail.call.SwitchTab">
					<parameter name="action" value="close" />
				</call>
				""".formatted(store)));
	}

	/** A device keeps its profile across a restart and another device, and can start afresh (Chrome and Edge only). */
	@Test
	void aDeviceKeepsItsProfileAcrossARestart() throws IOException {
		sailWorkflowString(automation("devices", """
				<call className="util.sail.call.StartDevice" variable="device">
					<parameter name="profile" value="phone1" />
					<parameter name="touch" value="true" />
				</call>
				<testScript message="StartDevice gave the run's profile folder"><![CDATA[
					return sail.vars.device.endsWith('/' + sail.run + '/phone1');
				]]></testScript>
				<navigateLink href="{var:origin}/" />
				<testScript message="A touch screen"><![CDATA[ return matchMedia('(pointer: coarse)').matches; ]]></testScript>
				<execute><![CDATA[ localStorage.setItem('device', sail.run); ]]></execute>

				<call className="util.sail.call.RestartDevice" variable="again" />
				<navigateLink href="{var:origin}/" />
				<testScript message="RestartDevice kept the device's storage, with a mouse by default"><![CDATA[
					return sail.vars.again === sail.vars.device && localStorage.getItem('device') === sail.run && ! matchMedia('(pointer: coarse)').matches;
				]]></testScript>
				<call className="util.sail.call.RestartDevice">
					<parameter name="offline" value="true" />
				</call>
				<testScript message="RestartDevice started the device offline"><![CDATA[ return navigator.onLine === false; ]]></testScript>

				<call className="util.sail.call.StartDevice">
					<parameter name="profile" value="phone2" />
				</call>
				<navigateLink href="{var:origin}/" />
				<testScript message="Another device has nothing of phone1's"><![CDATA[ return localStorage.getItem('device') === null; ]]></testScript>
				<call className="util.sail.call.StartDevice">
					<parameter name="profile" value="phone1" />
				</call>
				<navigateLink href="{var:origin}/" />
				<testScript message="Back on phone1, as it was left"><![CDATA[ return localStorage.getItem('device') === sail.run; ]]></testScript>
				<call className="util.sail.call.StartDevice">
					<parameter name="profile" value="phone1" />
					<parameter name="remembered" value="false" />
				</call>
				<navigateLink href="{var:origin}/" />
				<testScript message="phone1 started afresh"><![CDATA[ return localStorage.getItem('device') === null; ]]></testScript>
				"""));
		List<Path> runs;
		try (Stream<Path> list = Files.list(profiles)) {
			runs = list.toList();
		}
		List<String> devices;
		try (Stream<Path> list = Files.list(runs.get(0))) {
			devices = list.map(p -> p.getFileName().toString()).sorted().toList();
		}
		assertAll(() -> assertEquals(1, runs.size(), "one run folder: " + runs),
					() -> assertEquals(List.of("phone1", "phone2"), devices, "a profile folder for each device"));
	}
}

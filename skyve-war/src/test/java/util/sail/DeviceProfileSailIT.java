package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;

import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;

/**
 * Tests browser profiles in a real headless Chrome: what a page writes to {@code localStorage} and IndexedDB on one
 * profile is there after the browser is stopped and started again on that profile, and is absent on another profile.
 * The page comes from a JDK {@link HttpServer} on {@code localhost}, so this needs no Skyve server; it needs Chrome,
 * like the other SAIL ITs.
 */
@Tag("sail")
class DeviceProfileSailIT {
	private static final String PAGE = "<html><body>device</body></html>";
	private static final String WRITE_INDEXED_DB = """
			var done = arguments[arguments.length - 1];
			var open = indexedDB.open('device', 1);
			open.onupgradeneeded = function() { open.result.createObjectStore('s'); };
			open.onsuccess = function() {
				var tx = open.result.transaction('s', 'readwrite');
				tx.objectStore('s').put('stored', 'k');
				tx.oncomplete = function() { open.result.close(); done('ok'); };
			};
			open.onerror = function() { done('error'); };
			""";
	/** Reads the value, or null if the database or the key is absent (opening it creates an empty one: harmless here). */
	private static final String READ_INDEXED_DB = """
			var done = arguments[arguments.length - 1];
			var open = indexedDB.open('device', 1);
			open.onupgradeneeded = function() { open.result.createObjectStore('s'); };
			open.onsuccess = function() {
				var get = open.result.transaction('s', 'readonly').objectStore('s').get('k');
				get.onsuccess = function() { open.result.close(); done(get.result === undefined ? null : get.result); };
			};
			open.onerror = function() { done('error'); };
			""";

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	private HttpServer server;
	private String url;
	private Path root;

	@BeforeEach
	void startServer() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", exchange -> {
			byte[] body = PAGE.getBytes(StandardCharsets.UTF_8);
			exchange.getResponseHeaders().add("Content-Type", "text/html; charset=utf-8");
			exchange.sendResponseHeaders(200, body.length);
			try (java.io.OutputStream out = exchange.getResponseBody()) {
				out.write(body);
			}
		});
		server.start();
		url = "http://localhost:" + server.getAddress().getPort() + '/';
		root = Files.createTempDirectory("sail-device-profiles");
	}

	@AfterEach
	void cleanUp() throws IOException {
		selenium.stopBrowser();
		SailServerLock.releaseAll();
		server.stop(0);
		try (Stream<Path> files = Files.walk(root)) {
			files.sorted(Comparator.reverseOrder()).forEach(file -> file.toFile().delete());
		}
	}

	private BrowserConfiguration device(Path profile) {
		return new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(url).fixed().profile(profile);
	}

	private void open(BrowserConfiguration configuration) {
		selenium.startBrowser(configuration);
		selenium.driver.get(url);
	}

	private Object localStorage() {
		return ((JavascriptExecutor) selenium.driver).executeScript("return window.localStorage.getItem('k');");
	}

	private Object indexedDb() {
		return ((JavascriptExecutor) selenium.driver).executeAsyncScript(READ_INDEXED_DB);
	}

	@Test
	void aProfileKeepsItsStorageAcrossARestartAndAnotherProfileHasNone() {
		BrowserConfiguration a = device(root.resolve("a"));
		BrowserConfiguration b = device(root.resolve("b"));

		open(a);
		((JavascriptExecutor) selenium.driver).executeScript("window.localStorage.setItem('k', 'stored');");
		Object written = ((JavascriptExecutor) selenium.driver).executeAsyncScript(WRITE_INDEXED_DB);
		selenium.stopBrowser();

		open(a); // the same device again
		Object restoredLocalStorage = localStorage();
		Object restoredIndexedDb = indexedDb();
		String sameDevice = selenium.browserChange(a.resolve(), a);
		selenium.stopBrowser();

		open(b); // another device
		Object otherLocalStorage = localStorage();
		Object otherIndexedDb = indexedDb();
		String otherDevice = selenium.browserChange(a.resolve(), a);

		assertAll(() -> assertEquals("ok", written),
					() -> assertEquals("stored", restoredLocalStorage, "localStorage on the same profile"),
					() -> assertEquals("stored", restoredIndexedDb, "IndexedDB on the same profile"),
					() -> assertNull(sameDevice, "a restarted browser is the same device"),
					() -> assertNull(otherLocalStorage, "localStorage on another profile"),
					() -> assertNull(otherIndexedDb, "IndexedDB on another profile"),
					() -> assertEquals("profile " + b.getProfile() + " -> " + a.getProfile(), otherDevice, "the Selenide follows the restart"));
	}
}

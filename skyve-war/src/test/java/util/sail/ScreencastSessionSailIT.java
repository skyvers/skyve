package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.support.ui.FluentWait;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;
import util.sail.SailMedia.Frame;
import util.sail.SailMedia.Screencast;
import util.sail.browser.ChromiumBrowserControl;
import util.sail.browser.NetworkConditions;
import util.sail.browser.SailBrowserControl;

/**
 * Proves, in a headless Chrome against a small local server with a service worker, that the SAIL video's screencast
 * records and does not change what a page sees: a page loaded after the browser goes offline, while the screencast
 * is on, has {@code navigator.onLine === false}. It once had {@code true}, because the screencast opened a second
 * DevTools session on the page (see {@link SailMedia.Screencast}). It needs no Skyve server, only Chrome.
 * <p>
 * Two pages ({@code screencast/a.html}, {@code b.html}) are cached by a service worker, which answers them while the
 * browser is offline. Each shows a counter that changes, so the screencast has frames to send. Every wait is on a
 * named event, bounded by the browser's SAIL timeout.
 */
@Tag("sail")
@SuppressWarnings("java:S5960") // test-only assertions
class ScreencastSessionSailIT {
	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	@TempDir private Path video;
	private HttpServer server;
	private String origin;
	private ChromiumDriver driver;

	@BeforeEach
	void startTheServerAndBrowser() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", this::serve);
		server.start();
		origin = "http://localhost:" + server.getAddress().getPort(); // localhost is a secure context, as workers need
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(origin + '/').headless(true));
		driver = assertInstanceOf(ChromiumDriver.class, selenium.driver, "Chrome");
	}

	@AfterEach
	void cleanUp() {
		selenium.stopBrowser();
		SailServerLock.releaseAll();
		server.stop(0);
	}

	/** Serves the fixtures under {@code screencast/} from the classpath, which the service worker may not cache. */
	private void serve(@Nonnull HttpExchange exchange) throws IOException {
		String path = exchange.getRequestURI().getPath();
		byte[] body = new byte[0];
		boolean found = false;
		if (path.startsWith("/screencast/") && (! path.contains(".."))) {
			try (InputStream in = ScreencastSessionSailIT.class.getResourceAsStream(path.substring(1))) {
				if (in != null) {
					body = in.readAllBytes();
					found = true;
				}
			}
		}
		exchange.getResponseHeaders().add("Content-Type", path.endsWith(".js") ? "text/javascript" : "text/html; charset=utf-8");
		exchange.getResponseHeaders().add("Cache-Control", "no-store");
		exchange.sendResponseHeaders(found ? 200 : 404, found ? body.length : -1);
		if (found) {
			try (OutputStream out = exchange.getResponseBody()) {
				out.write(body);
			}
		}
	}

	private Object js(@Nonnull String script) {
		return driver.executeScript(script);
	}

	/** Waits for a count of the video's frames, the event of Chrome drawing and the screencast writing them. */
	private void awaitFrames(@Nonnull List<Frame> frames, int atLeast) {
		new FluentWait<>(frames).withTimeout(selenium.timeout())
								.pollingEvery(SailBrowserControl.POLLING)
								.withMessage(atLeast + " video frames")
								.until(f -> Boolean.valueOf(size(f) >= atLeast));
	}

	private static int size(@Nonnull List<Frame> frames) {
		synchronized (frames) {
			return frames.size();
		}
	}

	@Test
	void aPageLoadedOfflineWhileTheVideoRecordsSeesItselfOffline() throws IOException {
		Files.createDirectories(video.resolve("frames"));
		driver.get(origin + "/screencast/a.html");
		js("return window.serviceWorkerReady;"); // the event: the worker has cached b.html
		List<Frame> frames = new ArrayList<>();
		Screencast screencast = Screencast.start(driver, video, frames, selenium);
		assertNotNull(screencast, "the video started");
		try {
			awaitFrames(frames, 1);
			int before = size(frames);
			SailBrowserControl.of(driver).require(NetworkConditions.class, "ScreencastSessionSailIT").offline();
			driver.get(origin + "/screencast/b.html"); // from the worker's cache: the network is down
			assertEquals("B", driver.getTitle(), "loaded from the cache");
			awaitOnLineFalse();
			awaitFrames(frames, before + 1); // and the screencast goes on drawing the page loaded offline
			assertEquals(Boolean.FALSE, js("return navigator.onLine;"), "still offline, with the video on");
		}
		finally {
			SailBrowserControl.of(driver).require(NetworkConditions.class, "ScreencastSessionSailIT").online();
			screencast.stop();
		}
		assertFramesAreJpegs(frames);
	}

	/**
	 * The video has no DevTools connection of its own: the browser control is the one session on the page that the
	 * screencast needs. After the run ends the control has stopped it, and no frame arrives.
	 */
	@Test
	void stoppingEndsTheFramesAndTheControlKeepsWorking() throws IOException {
		Files.createDirectories(video.resolve("frames"));
		driver.get(origin + "/screencast/a.html");
		js("return window.serviceWorkerReady;");
		List<Frame> frames = new ArrayList<>();
		Screencast screencast = Screencast.start(driver, video, frames, selenium);
		assertNotNull(screencast, "the video started");
		awaitFrames(frames, 2);
		screencast.stop();
		int written = size(frames);
		assertTrue(written >= 2, "frames were written");
		// The page changes every 100 ms, so a screencast still on would send more
		js("return new Promise(resolve => setTimeout(resolve, 600));");
		assertAll(() -> assertEquals(written, size(frames), "no frame after stop"),
					() -> assertInstanceOf(ChromiumBrowserControl.class, SailBrowserControl.of(driver)));
		// it can start again, as it does for a new run on the same browser
		Screencast again = Screencast.start(driver, video, frames, selenium);
		assertNotNull(again, "restarted");
		awaitFrames(frames, written + 1);
		again.stop();
	}

	private void awaitOnLineFalse() {
		new FluentWait<WebDriver>(driver).withTimeout(selenium.timeout())
											.pollingEvery(SailBrowserControl.POLLING)
											.withMessage("navigator.onLine to be false in a page loaded offline")
											.until(d -> Boolean.valueOf(Boolean.FALSE.equals(((ChromiumDriver) d).executeScript("return navigator.onLine;"))));
	}

	private void assertFramesAreJpegs(@Nonnull List<Frame> frames) throws IOException {
		assertFalse(frames.isEmpty(), "frames were recorded");
		for (Frame frame : frames) {
			byte[] image = Files.readAllBytes(video.resolve(frame.file()));
			assertTrue((image.length > 2) && ((image[0] & 0xFF) == 0xFF) && ((image[1] & 0xFF) == 0xD8), frame.file() + " is a JPEG");
		}
	}
}

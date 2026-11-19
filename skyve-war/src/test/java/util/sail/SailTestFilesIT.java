package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import com.codeborne.selenide.WebDriverRunner;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;

/**
 * Proves that {@link SailTestFiles#video(WebDriver, long)} records a video that plays, in a real Chrome against a page
 * from a local server, so it needs no Skyve server, only Chrome. The browser starts as other SAIL browsers do,
 * headless unless {@code -Dsail.headless=false}. Run it with
 * {@code mvn -o -pl skyve-war -am test -Dtest=SailTestFilesIT -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1}.
 * <p>
 * The video is chosen in a file input, as an upload would be, and played from there in a {@code <video>}. Every wait
 * is on a named event, bounded by the browser's SAIL timeout.
 */
@Tag("sail")
class SailTestFilesIT {
	/** More than one 5 MB upload chunk, as basix-cloud's chunked upload test needed. */
	private static final long SIZE = 6L * 1024 * 1024;
	private static final String PAGE = "<!doctype html><title>SAIL test files</title><input type=\"file\" id=\"file\">";
	/**
	 * Plays the chosen file in a {@code <video>} and gives its duration, width and height, and whether a frame showed
	 * after seeking to the middle. A {@code MediaRecorder} WebM has no duration in its header, so Chrome reports
	 * {@code Infinity} at {@code loadedmetadata}; seeking far past the end makes it read to the end and fire
	 * {@code durationchange} with the real duration.
	 */
	private static final String PLAY = """
			const video = document.createElement('video');
			video.muted = true;
			const failed = new Promise((resolve, reject) => {
			    video.addEventListener('error', () => reject(new Error('the video failed: ' + (video.error && video.error.message))));
			});
			const event = (name, done) => Promise.race([failed, new Promise((resolve) => {
			    const check = () => {
			        if (done()) {
			            video.removeEventListener(name, check);
			            resolve();
			        }
			    };
			    video.addEventListener(name, check);
			})]);
			const loaded = event('loadedmetadata', () => true);
			video.src = URL.createObjectURL(document.getElementById('file').files[0]);
			await loaded;
			const infinite = ! Number.isFinite(video.duration);
			if (infinite) {
			    const known = event('durationchange', () => Number.isFinite(video.duration));
			    video.currentTime = Number.MAX_SAFE_INTEGER;
			    await known;
			}
			const seeked = event('seeked', () => true);
			video.currentTime = video.duration / 2;
			await seeked;
			return [video.duration, video.videoWidth, video.videoHeight, video.readyState >= HTMLMediaElement.HAVE_CURRENT_DATA, infinite];""";

	private static HttpServer server;
	private static String origin;

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	private WebDriver driver;

	@BeforeAll
	static void startServer() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", SailTestFilesIT::serve);
		server.start();
		origin = "http://localhost:" + server.getAddress().getPort();
	}

	@AfterAll
	static void stopServer() {
		server.stop(0);
	}

	private static void serve(@Nonnull HttpExchange exchange) throws IOException {
		try (InputStream in = exchange.getRequestBody()) {
			in.readAllBytes();
		}
		byte[] body = PAGE.getBytes(StandardCharsets.UTF_8);
		exchange.getResponseHeaders().add("Content-Type", "text/html; charset=utf-8");
		exchange.sendResponseHeaders(200, body.length);
		try (OutputStream out = exchange.getResponseBody()) {
			out.write(body);
		}
	}

	@BeforeEach
	void startBrowser() {
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(origin + '/'));
		driver = WebDriverRunner.getWebDriver();
		driver.get(origin + '/');
	}

	@AfterEach
	void stopBrowser() {
		selenium.stopBrowser();
	}

	@Test
	void aRecordedVideoPlaysAndIsReused() throws IOException {
		Path expected = SailTestFiles.FOLDER.resolve("video-" + SIZE + ".webm");
		Files.deleteIfExists(expected);
		Path video = SailTestFiles.video(driver, SIZE);
		byte[] start = new byte[4];
		try (InputStream in = Files.newInputStream(video)) {
			assertEquals(4, in.readNBytes(start, 0, 4), "the file has a header");
		}

		driver.findElement(By.id("file")).sendKeys(video.toString());
		List<?> played = assertInstanceOf(List.class, ((JavascriptExecutor) driver).executeScript("return (async () => { " + PLAY + " })();"), "what the video gave");
		Number duration = assertInstanceOf(Number.class, played.get(0), "the duration");
		assertAll(() -> assertEquals(expected, video, "the path"),
					() -> assertTrue(Files.size(video) >= SIZE, "at least the size: " + Files.size(video)),
					() -> assertArrayEquals(new byte[] {0x1A, 0x45, (byte) 0xDF, (byte) 0xA3}, start, "a WebM (EBML) file: " + Arrays.toString(start)),
					() -> assertTrue(Double.isFinite(duration.doubleValue()) && (duration.doubleValue() > 0), "a finite, positive duration: " + duration),
					() -> assertEquals(Long.valueOf(SailTestFiles.VIDEO_WIDTH), Long.valueOf(((Number) played.get(1)).longValue()), "the width"),
					() -> assertEquals(Long.valueOf(SailTestFiles.VIDEO_HEIGHT), Long.valueOf(((Number) played.get(2)).longValue()), "the height"),
					() -> assertEquals(Boolean.TRUE, played.get(3), "a frame shows after seeking to the middle"),
					() -> assertEquals(Boolean.TRUE, ((JavascriptExecutor) driver).executeScript("return window.__sailTestVideo === undefined;"),
										"the page no longer holds the video"));
		System.out.println("SAIL test video: " + Files.size(video) + " bytes, " + duration + " s, infinite at first: " + played.get(4));

		// a second call finds it there with the size, and doesn't record again
		Files.setLastModifiedTime(video, FileTime.fromMillis(86_400_000L));
		assertAll(() -> assertEquals(video, SailTestFiles.video(driver, SIZE), "the same path"),
					() -> assertEquals(FileTime.fromMillis(86_400_000L), Files.getLastModifiedTime(video), "not written again"));
	}
}

package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import util.sail.SailMedia.Frame;
import util.sail.SailMedia.Screencast;
import util.sail.browser.Capability;
import util.sail.browser.FakeBrowserControl;
import util.sail.browser.PageScreencast;

/**
 * Tests the video's screencast against a fake browser control, with no browser: that it uses the control's screencast
 * capability and nothing else, writes the frames in order with the times the browser gave, and stops. Its use in a
 * real Chrome, with a page loaded offline, is {@code ScreencastSessionSailIT}.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailMediaScreencastTest {
	private static PrimeFacesSelenide selenide() {
		PrimeFacesSelenide result = mock(PrimeFacesSelenide.class);
		when(result.timeout()).thenReturn(Duration.ofSeconds(5));
		return result;
	}

	/** Returns a screencast that started, so a test can stop it. */
	private static Screencast started(Screencast screencast) {
		assertNotNull(screencast, "the screencast started");
		return screencast;
	}

	private static PageScreencast.Image frame(String content, long millis) {
		return new PageScreencast.Image(Base64.getEncoder().encodeToString(content.getBytes(StandardCharsets.UTF_8)), millis);
	}

	@Test
	void itStartsOnTheControlsScreencastForTheWindowWithTheVideoOptions(@TempDir Path folder) {
		FakeBrowserControl control = new FakeBrowserControl("chrome", Capability.SCREENCAST);
		Screencast screencast = started(Screencast.start(control, "CDwindow-1", folder, new ArrayList<>(), selenide()));
		assertAll(() -> assertEquals(List.of("startScreencast CDwindow-1 jpeg 60 1280 800"), control.calls()));
		screencast.stop();
	}

	@Test
	void framesAreWrittenInOrderWithTheTimesTheBrowserGave(@TempDir Path folder) throws IOException {
		Files.createDirectories(folder.resolve("frames"));
		FakeBrowserControl control = new FakeBrowserControl("chrome", Capability.SCREENCAST);
		List<Frame> frames = new ArrayList<>();
		Screencast screencast = started(Screencast.start(control, "w", folder, frames, selenide()));
		control.drawFrame(frame("one", 1000L));
		control.drawFrame(frame("two", 1500L));
		control.drawFrame(frame("three", 2600L));
		screencast.stop(); // waits for the writes
		assertAll(() -> assertEquals(List.of(new Frame("frames/000001.jpg", 1000L),
												new Frame("frames/000002.jpg", 1500L),
												new Frame("frames/000003.jpg", 2600L)), frames),
					() -> assertEquals("one", Files.readString(folder.resolve("frames/000001.jpg"))),
					() -> assertEquals("two", Files.readString(folder.resolve("frames/000002.jpg"))),
					() -> assertEquals("three", Files.readString(folder.resolve("frames/000003.jpg"))));
	}

	@Test
	void aSecondScreencastContinuesTheNumberingOfTheFirst(@TempDir Path folder) throws IOException {
		Files.createDirectories(folder.resolve("frames"));
		List<Frame> frames = new ArrayList<>();
		FakeBrowserControl first = new FakeBrowserControl("chrome", Capability.SCREENCAST);
		Screencast one = started(Screencast.start(first, "a", folder, frames, selenide()));
		first.drawFrame(frame("x", 10L));
		one.stop();
		FakeBrowserControl second = new FakeBrowserControl("chrome", Capability.SCREENCAST); // a new browser
		Screencast two = started(Screencast.start(second, "b", folder, frames, selenide()));
		second.drawFrame(frame("y", 20L));
		two.stop();
		assertEquals(List.of(new Frame("frames/000001.jpg", 10L), new Frame("frames/000002.jpg", 20L)), frames);
	}

	@Test
	void stoppingStopsTheControlsScreencastAndLaterFramesAreDropped(@TempDir Path folder) {
		FakeBrowserControl control = new FakeBrowserControl("chrome", Capability.SCREENCAST);
		List<Frame> frames = new ArrayList<>();
		Screencast screencast = started(Screencast.start(control, "w", folder, frames, selenide()));
		screencast.stop();
		assertAll(() -> assertEquals(List.of("startScreencast w jpeg 60 1280 800", "stopScreencast"), control.calls()),
					() -> assertFalse(control.drawFrame(frame("late", 1L)), "the control's sink is gone"),
					() -> assertTrue(frames.isEmpty()));
	}

	/** A browser without the capability (not Chromium) has no video, and the run carries on. */
	@Test
	void aBrowserWithoutTheCapabilityHasNoVideo(@TempDir Path folder) {
		FakeBrowserControl control = new FakeBrowserControl("firefox", Capability.COOKIES);
		PrimeFacesSelenide selenide = selenide();
		assertAll(() -> assertNull(Screencast.start(control, "w", folder, new ArrayList<>(), selenide)),
					() -> assertEquals(List.of(), control.calls(), "nothing was asked of the browser"));
		verify(selenide).trace(contains("SAIL video is not supported for firefox"));
	}

	@Test
	void aScreencastThatCannotStartMeansNoVideoNotAFailedRun(@TempDir Path folder) {
		FakeBrowserControl control = new FakeBrowserControl("chrome", Capability.SCREENCAST)
											.withScreencastFailure(new IllegalStateException("The browser has no attached page w"));
		PrimeFacesSelenide selenide = selenide();
		assertNull(Screencast.start(control, "w", folder, new ArrayList<>(), selenide));
		verify(selenide).trace(contains("SAIL video could not start, so this run has none: java.lang.IllegalStateException: The browser has no attached page w"));
	}

	@Test
	void aFrameThatCannotBeWrittenIsTracedAndTheRestCarryOn(@TempDir Path folder) throws IOException {
		Files.createDirectories(folder.resolve("frames"));
		FakeBrowserControl control = new FakeBrowserControl("chrome", Capability.SCREENCAST);
		List<Frame> frames = new ArrayList<>();
		PrimeFacesSelenide selenide = selenide();
		Screencast screencast = started(Screencast.start(control, "w", folder, frames, selenide));
		control.drawFrame(new PageScreencast.Image("!not base64!", 5L));
		control.drawFrame(frame("ok", 6L));
		screencast.stop();
		verify(selenide).trace(contains("Could not save a SAIL video frame"));
		assertEquals("ok", Files.readString(folder.resolve("frames/000001.jpg")));
	}
}

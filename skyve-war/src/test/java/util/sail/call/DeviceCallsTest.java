package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static util.sail.call.AbstractSailCallTest.context;
import static util.sail.call.AbstractSailCallTest.mockSelenide;
import static util.sail.call.AbstractSailCallTest.run;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import org.openqa.selenium.chrome.ChromeDriver;

import util.sail.BrowserConfiguration;
import util.sail.PrimeFacesSelenide;
import util.sail.SailSettings;
import util.sail.browser.Capability;
import util.sail.browser.FakeBrowserControl;
import util.sail.browser.SailCapabilityUnavailableException;

/**
 * Tests the device calls with a mock Selenide and a fake browser control: {@link StartDevice} puts the browser on the
 * run's profile folder under {@code sail.profiles}, starting it again only when it isn't there already or must start
 * afresh, and applies the device's network and touch; {@link RestartDevice} starts it again on the same folder.
 */
class DeviceCallsTest {
	@TempDir
	Path root;

	private SailSettings settings;
	private BrowserConfiguration configuration;
	private PrimeFacesSelenide selenide;
	private FakeBrowserControl browser;
	private AbstractSailCall.Context context;

	@BeforeEach
	void setUp() {
		settings = SailSettings.code().baseUrl("http://h/app/").profiles(root).resolve();
		configuration = new BrowserConfiguration();
		selenide = mockSelenide();
		when(selenide.getConfiguration()).thenReturn(configuration);
		when(selenide.getSettings()).thenReturn(settings);
		browser = FakeBrowserControl.supportingAll();
		ChromeDriver driver = mock(ChromeDriver.class);
		context = context(() -> driver, selenide, d -> browser);
	}

	private static String message(Class<? extends Throwable> type, Executable executable) {
		return assertThrows(type, executable).getMessage();
	}

	@Test
	void aDeviceStartsOnItsRunsProfileFolderWithItsNetworkAndTouch() throws Exception {
		Path folder = root.resolve("run1").resolve("phone1");
		String started = run(new StartDevice(), context, "profile", "phone1", "offline", "true", "touch", "true");
		List<String> first = List.copyOf(browser.calls());
		browser.calls().clear();
		String again = run(new StartDevice(), context, "profile", "phone1");
		InOrder order = inOrder(selenide);
		order.verify(selenide).stopBrowser();
		order.verify(selenide).startBrowser(configuration, settings);
		assertAll(() -> assertEquals(folder.toString(), started, "<sail.profiles>/<run>/<profile>"),
					() -> assertEquals(folder, configuration.getProfile(), "the browser started on it"),
					() -> assertEquals(List.of("offline", "emulateTouch 5"), first),
					() -> assertEquals(folder.toString(), again),
					() -> verify(selenide, times(1)).startBrowser(configuration, settings),
					() -> assertEquals(List.of("online", "emulateTouch 0"), browser.calls(), "on it already: online, and a mouse, by default"));
	}

	@Test
	void anotherDeviceOrAFreshOneStartsTheBrowserAgain() throws Exception {
		Path phone1 = root.resolve("run1").resolve("phone1");
		Path kept = Files.createDirectories(phone1.resolve("Default")).resolve("kept");
		Files.writeString(kept, "IndexedDB");
		configuration.profile(phone1);
		run(new StartDevice(), context, "profile", "phone1", "remembered", "true");
		boolean keptWhenRemembered = Files.exists(kept);
		run(new StartDevice(), context, "profile", "phone2");
		Path phone2 = configuration.getProfile();
		run(new StartDevice(), context, "profile", "phone1", "remembered", "false");
		assertAll(() -> assertTrue(keptWhenRemembered, "remembered: kept, and the browser left running"),
					() -> assertEquals(root.resolve("run1").resolve("phone2"), phone2, "another device"),
					() -> assertFalse(Files.exists(kept), "not remembered: started afresh"),
					() -> assertEquals(phone1, configuration.getProfile()),
					() -> verify(selenide, times(2)).stopBrowser(),
					() -> verify(selenide, times(2)).startBrowser(configuration, settings));
	}

	@Test
	void startDeviceChecksItsProfileAndNeedsAStartedBrowser() {
		PrimeFacesSelenide unstarted = mockSelenide();
		AbstractSailCall.Context before = context(() -> mock(ChromeDriver.class), unstarted, d -> browser);
		assertAll(() -> assertEquals("StartDevice's profile must be letters, digits, - or _, not ../escape",
										message(IllegalArgumentException.class, () -> run(new StartDevice(), context, "profile", "../escape"))),
					() -> assertEquals("StartDevice needs the parameter profile", message(IllegalArgumentException.class, () -> run(new StartDevice(), context))),
					() -> assertEquals("StartDevice runs in a leg whose browser has started",
										message(IllegalStateException.class, () -> run(new StartDevice(), before, "profile", "phone1"))),
					() -> verify(selenide, never()).stopBrowser());
	}

	@Test
	void aDeviceRestartsOnTheSameProfile() throws Exception {
		IllegalStateException noProfile = assertThrows(IllegalStateException.class, () -> run(new RestartDevice(), context));
		Path folder = root.resolve("run1").resolve("phone1");
		configuration.profile(folder);
		String restarted = run(new RestartDevice(), context, "offline", "true", "touch", "true");
		List<String> first = List.copyOf(browser.calls());
		browser.calls().clear();
		run(new RestartDevice(), context);
		InOrder order = inOrder(selenide);
		order.verify(selenide).stopBrowser();
		order.verify(selenide).startBrowser(configuration, settings);
		assertAll(() -> assertEquals("RestartDevice needs the leg on a device profile: use StartDevice first", noProfile.getMessage()),
					() -> assertEquals(folder.toString(), restarted),
					() -> assertEquals(folder, configuration.getProfile(), "the same profile"),
					() -> assertEquals(List.of("offline", "emulateTouch 5"), first),
					() -> assertEquals(List.of("online", "emulateTouch 0"), browser.calls(), "online, and a mouse, by default"),
					() -> verify(selenide, times(2)).startBrowser(configuration, settings));
	}

	/** A browser that can't change the network or touch is left as it is when the device asks for what it has anyway. */
	@Test
	void aDeviceNeedsTheNetworkOrTouchCapabilityOnlyToChangeThem() {
		FakeBrowserControl mouseOnly = new FakeBrowserControl("chromium");
		AbstractSailCall.Context limited = context(() -> mock(ChromeDriver.class), selenide, d -> mouseOnly);
		assertAll(() -> assertEquals(root.resolve("run1").resolve("phone1").toString(), run(new StartDevice(), limited, "profile", "phone1")),
					() -> assertEquals(List.of(), mouseOnly.calls(), "online and a mouse already"),
					() -> assertEquals("StartDevice needs NETWORK_CONDITIONS, which chromium doesn't support",
										message(SailCapabilityUnavailableException.class, () -> run(new StartDevice(), limited, "profile", "phone1", "offline", "true"))),
					() -> assertEquals("RestartDevice needs TOUCH, which chromium doesn't support",
										message(SailCapabilityUnavailableException.class, () -> run(new RestartDevice(), limited, "touch", "true"))),
					() -> assertFalse(mouseOnly.supports(Capability.TOUCH)));
	}
}

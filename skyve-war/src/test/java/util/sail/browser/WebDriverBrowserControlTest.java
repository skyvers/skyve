package util.sail.browser;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Options;
import org.openqa.selenium.WebDriver.TargetLocator;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.interactions.Interactive;
import org.openqa.selenium.interactions.Sequence;

/**
 * Tests {@link WebDriverBrowserControl} without a browser: the driver is a mock. Cookies are listed and deleted through
 * WebDriver; a tab that hasn't opened yet is waited for, up to the timeout; a visit to a new tab always comes back; and
 * keys are pressed as one action sequence, a chord holding its first keys down.
 */
@SuppressWarnings("unchecked") // Mockito's thenReturn(T, T...) with a generic Set
class WebDriverBrowserControlTest {
	private static final Duration SHORT = Duration.ofMillis(300);

	private WebDriver driver;
	private Options options;
	private TargetLocator target;
	private WebDriverBrowserControl control;

	@BeforeEach
	void setUp() {
		driver = mock(WebDriver.class, withSettings().extraInterfaces(Interactive.class));
		options = mock(Options.class);
		target = mock(TargetLocator.class);
		when(driver.manage()).thenReturn(options);
		when(driver.switchTo()).thenReturn(target);
		control = (WebDriverBrowserControl) SailBrowserControl.of(driver);
	}

	private static Set<String> handles(String... handles) {
		return new LinkedHashSet<>(List.of(handles));
	}

	@Test
	void cookiesAreTheSitesCookiesWithSessionCookiesMarked() {
		Cookie remember = new Cookie("remember", "1", "h", "/app", new Date(System.currentTimeMillis() + 86_400_000L), false, true);
		Cookie session = new Cookie("JSESSIONID", "2");
		when(options.getCookies()).thenReturn(new LinkedHashSet<>(List.of(remember, session)));
		assertEquals(List.of(new BrowserCookie("remember", "1", "h", "/app", false), new BrowserCookie("JSESSIONID", "2", "", "/", true)),
						control.cookies());
	}

	@Test
	void deletingCookiesCountsThoseWithTheNameAndDeletesThemOnce() {
		when(options.getCookies()).thenReturn(new LinkedHashSet<>(List.of(new Cookie("remember", "1"), new Cookie("JSESSIONID", "2"))));
		int deleted = control.deleteCookies("remember");
		int none = control.deleteCookies("absent");
		assertAll(() -> assertEquals(1, deleted),
					() -> assertEquals(0, none),
					() -> verify(options).deleteCookieNamed("remember"),
					() -> verify(options, never()).deleteCookieNamed("absent"));
	}

	@Test
	void aTabOpensInFrontOnItsUrl() {
		control.openTab("http://h/app/");
		InOrder order = inOrder(target, driver);
		order.verify(target).newWindow(WindowType.TAB);
		order.verify(driver).get("http://h/app/");
	}

	/** A tab that a link opens a moment later is waited for, so the switch finds it. */
	@Test
	void switchingToATabWaitsForItToOpen() {
		when(driver.getWindowHandles()).thenReturn(handles("a"), handles("a", "b"));
		control.switchToTab(1, Duration.ofSeconds(5));
		verify(target).window("b");
	}

	@Test
	void switchingToATabThatNeverOpensFailsAtTheTimeout() {
		when(driver.getWindowHandles()).thenReturn(handles("a"));
		AssertionError e = assertThrows(AssertionError.class, () -> control.switchToTab(1, SHORT));
		IllegalArgumentException negative = assertThrows(IllegalArgumentException.class, () -> control.switchToTab(-1, SHORT));
		assertAll(() -> assertEquals("Tab 1 (0 being the first) did not open within PT0.3S: 1 tab(s) are open", e.getMessage()),
					() -> assertEquals("A tab's index is 0 (the first) or more, not -1", negative.getMessage()),
					() -> verify(target, never()).window(anyString()));
	}

	@Test
	void closingATabBringsTheFirstToTheFrontAndTabsAreCounted() {
		when(driver.getWindowHandles()).thenReturn(handles("a", "b"), handles("a"));
		int before = control.tabCount();
		control.closeTab();
		InOrder order = inOrder(target, driver);
		order.verify(driver).close();
		order.verify(target).window("a");
		assertEquals(2, before);
	}

	/** A visit waits for the new tab, visits it there, closes it, and comes back to the tab it started from. */
	@Test
	void aVisitWaitsForTheNewTabAndComesBack() {
		when(driver.getWindowHandle()).thenReturn("home");
		when(driver.getWindowHandles()).thenReturn(handles("home"), handles("home", "old", "new"));
		List<String> seen = new ArrayList<>();
		String result = control.visitNewTab(Duration.ofSeconds(5), true, () -> {
			seen.add("visited");
			return "the new tab's answer";
		});
		InOrder order = inOrder(target, driver);
		order.verify(target).window("new");
		order.verify(driver).close();
		order.verify(target).window("home");
		assertAll(() -> assertEquals("the new tab's answer", result),
					() -> assertEquals(List.of("visited"), seen));
	}

	/** A visit that throws, or a tab the page closed itself, still comes back; a tab can be left open. */
	@Test
	void aVisitComesBackWhenItThrowsOrTheTabHasClosedAndCanLeaveTheTabOpen() {
		when(driver.getWindowHandle()).thenReturn("home");
		when(driver.getWindowHandles()).thenReturn(handles("home", "new"));
		IllegalStateException thrown = new IllegalStateException("the visit failed");
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> control.visitNewTab(SHORT, true, () -> {
			throw thrown;
		}));
		doThrow(new NoSuchWindowException("closed")).when(driver).close();
		control.visitNewTab(SHORT, true, () -> null);
		control.visitNewTab(SHORT, false, () -> null);
		assertAll(() -> assertSame(thrown, e),
					() -> verify(driver, times(2)).close(),
					() -> verify(target, times(3)).window("home"));
	}

	@Test
	void aVisitFailsAtTheTimeoutWhenNoNewTabOpens() {
		when(driver.getWindowHandle()).thenReturn("home");
		when(driver.getWindowHandles()).thenReturn(handles("home"));
		AssertionError e = assertThrows(AssertionError.class, () -> control.visitNewTab(SHORT, true, () -> "never"));
		assertAll(() -> assertEquals("No new tab opened within PT0.3S", e.getMessage()),
					() -> verify(target, never()).window(anyString()));
	}

	/** Returns the key actions WebDriver was asked to perform, as "keyDown ". */
	private List<String> keyActions() {
		ArgumentCaptor<Collection<Sequence>> sequences = ArgumentCaptor.forClass(Collection.class);
		verify((Interactive) driver).perform(sequences.capture());
		List<String> result = new ArrayList<>();
		for (Sequence sequence : sequences.getValue()) {
			for (Object action : (List<?>) sequence.encode().get("actions")) {
				Map<String, Object> encoded = (Map<String, Object>) action;
				result.add(encoded.get("type") + " " + encoded.get("value"));
			}
		}
		return result;
	}

	@Test
	void aKeyIsPressedAndReleased() {
		control.press(org.openqa.selenium.Keys.ESCAPE);
		String escape = org.openqa.selenium.Keys.ESCAPE.toString();
		assertEquals(List.of("keyDown " + escape, "keyUp " + escape), keyActions());
	}

	@Test
	void aChordHoldsItsFirstKeysDownWhileTheLastIsPressed() {
		control.press(org.openqa.selenium.Keys.CONTROL, org.openqa.selenium.Keys.SHIFT, "a");
		String ctrl = org.openqa.selenium.Keys.CONTROL.toString();
		String shift = org.openqa.selenium.Keys.SHIFT.toString();
		assertEquals(List.of("keyDown " + ctrl, "keyDown " + shift, "keyDown a", "keyUp a", "keyUp " + shift, "keyUp " + ctrl),
						keyActions());
	}

	@Test
	void pressingNoKeyIsRefused() {
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> control.press());
		assertEquals("Press needs at least one key", e.getMessage());
	}

	@Test
	void theWindowIsResizedAndItsSizeReturned() {
		WebDriver.Window window = mock(WebDriver.Window.class);
		when(options.window()).thenReturn(window);
		when(window.getSize()).thenReturn(new Dimension(800, 580));
		Dimension size = control.resizeWindow(800, 600);
		assertAll(() -> verify(window).setSize(new Dimension(800, 600)),
					() -> assertEquals(new Dimension(800, 580), size, "the size the platform allowed"));
	}
}

package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static util.sail.call.AbstractSailCallTest.context;
import static util.sail.call.AbstractSailCallTest.mockSelenide;
import static util.sail.call.AbstractSailCallTest.run;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.TargetLocator;

import util.sail.browser.FakeBrowserControl;
import util.sail.browser.SailBrowserControl;

/**
 * Tests the tab calls: {@link SwitchTab} against a fake browser control, and against plain WebDriver's control over a
 * mock driver to show that it waits for a tab the page opens a moment later; and {@link CheckOpenedTab}, which visits
 * the new tab, waits there for its script, and comes back.
 */
@SuppressWarnings({"static-method", "unchecked"}) // Mockito's thenReturn(T, T...) with a generic Set
class TabCallsTest {
	private static final String SCRIPT = "return document.title === 'Opened';";

	private static String message(Class<? extends Throwable> type, Executable executable) {
		return assertThrows(type, executable).getMessage();
	}

	private static Set<String> handles(String... handles) {
		return new LinkedHashSet<>(List.of(handles));
	}

	@Test
	void switchTabOpensSwitchesAndClosesTabs() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		String opened = run(new SwitchTab(), context, "action", "open", "url", "away");
		run(new SwitchTab(), context, "action", "open", "url", "/rooted?a=1");
		run(new SwitchTab(), context, "action", "open", "url", "https://elsewhere/x");
		String switched = run(new SwitchTab(), context, "action", "switch", "index", "3");
		String closed = run(new SwitchTab(), context, "action", " close ");
		assertAll(() -> assertEquals("2", opened, "the tabs open afterwards"),
					() -> assertEquals("4", switched),
					() -> assertEquals("3", closed),
					() -> assertEquals(List.of("openTab http://h/app/away", "tabCount",
												"openTab http://h/app/rooted?a=1", "tabCount",
												"openTab https://elsewhere/x", "tabCount",
												"switchToTab 3 PT7S", "tabCount",
												"closeTab", "tabCount"),
										browser.calls(), "relative to the base URL, or absolute; the leg's timeout"),
					() -> assertEquals("SwitchTab's action must be open, switch or close, not move",
										message(IllegalArgumentException.class, () -> run(new SwitchTab(), context, "action", "move"))),
					() -> assertEquals("SwitchTab needs the parameter url",
										message(IllegalArgumentException.class, () -> run(new SwitchTab(), context, "action", "open"))),
					() -> assertEquals("SwitchTab needs the parameter index",
										message(IllegalArgumentException.class, () -> run(new SwitchTab(), context, "action", "switch"))),
					() -> assertEquals("Tab 5 (0 being the first) did not open within PT7S: 3 tab(s) are open",
										message(AssertionError.class, () -> run(new SwitchTab(), context, "action", "switch", "index", "5"))));
	}

	/** A tab that a link opens a moment after the step before is waited for, so the switch finds it. */
	@Test
	void switchTabWaitsForANewTab() throws Exception {
		WebDriver driver = mock(WebDriver.class);
		TargetLocator target = mock(TargetLocator.class);
		when(driver.switchTo()).thenReturn(target);
		// the page's new tab appears at the third look
		when(driver.getWindowHandles()).thenReturn(handles("first"), handles("first"), handles("first", "new"));
		AbstractSailCall.Context context = context(() -> driver, mockSelenide(), SailBrowserControl::of);
		String tabs = run(new SwitchTab(), context, "action", "switch", "index", "1");
		assertAll(() -> verify(target).window("new"),
					() -> assertEquals("2", tabs));
	}

	@Test
	void checkOpenedTabWaitsForItsScriptInTheNewTabAndComesBack() throws Exception {
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
		when(((JavascriptExecutor) driver).executeScript(SCRIPT)).thenThrow(new JavascriptException("still loading"))
																	.thenReturn(Boolean.FALSE, Boolean.TRUE);
		when(driver.getCurrentUrl()).thenReturn("http://h/app/opened");
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withTabs(2);
		AbstractSailCall.Context context = context(driver, browser);
		String address = run(new CheckOpenedTab(), context, "script", SCRIPT);
		assertAll(() -> assertEquals("http://h/app/opened", address, "the new tab's address"),
					() -> assertEquals(List.of("visitNewTab PT7S true"), browser.calls(), "closed, within the leg's timeout"),
					() -> assertEquals(1, browser.tabCount(), "the visited tab was closed"));
	}

	@Test
	void checkOpenedTabFailsWhenItsScriptNeverHoldsOrNoTabOpens() {
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
		when(((JavascriptExecutor) driver).executeScript(SCRIPT)).thenThrow(new JavascriptException("title is broken"));
		when(((JavascriptExecutor) driver).executeScript("return gone;")).thenThrow(new NoSuchWindowException("closed"));
		when(driver.getCurrentUrl()).thenReturn("http://h/app/opened");
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withTabs(2);
		AbstractSailCall.Context context = context(driver, browser);
		String never = message(AssertionError.class, () -> run(new CheckOpenedTab(), context, "script", SCRIPT, "timeoutMillis", "300", "close", "false"));
		String gone = message(IllegalStateException.class, () -> run(new CheckOpenedTab(), context, "script", "return gone;", "timeoutMillis", "5000"));
		FakeBrowserControl oneTab = FakeBrowserControl.supportingAll();
		String noTab = message(AssertionError.class, () -> run(new CheckOpenedTab(), context(driver, oneTab), "script", SCRIPT));
		assertAll(() -> assertTrue(never.startsWith("The new tab (http://h/app/opened) did not show what was expected within PT0.3S: " + SCRIPT
														+ " (the script last failed with: title is broken"), never),
					() -> assertTrue(gone.startsWith("The new tab or the browser is gone: closed"), "fails at once: " + gone),
					() -> assertEquals("visitNewTab PT0.3S false", browser.calls().get(0), "left open"),
					() -> assertEquals("No new tab opened within PT7S", noTab),
					() -> assertEquals("CheckOpenedTab needs the parameter script",
										message(IllegalArgumentException.class, () -> run(new CheckOpenedTab(), context))));
	}
}

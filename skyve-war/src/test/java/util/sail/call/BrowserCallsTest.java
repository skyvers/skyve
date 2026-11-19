package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static util.sail.call.AbstractSailCallTest.context;
import static util.sail.call.AbstractSailCallTest.run;

import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import util.sail.browser.BrowserCookie;
import util.sail.browser.Dialog;
import util.sail.browser.FakeBrowserControl;

/**
 * Tests the browser calls (dialogs, cookies, quota, init scripts, keys, window and tab freezing) against a fake browser
 * control and a mock driver: what each asks of the browser, its defaults, its parameter checks, and what it returns.
 */
@SuppressWarnings("static-method")
class BrowserCallsTest {
	private static String message(Class<? extends Throwable> type, Executable executable) {
		return assertThrows(type, executable).getMessage();
	}

	private static WebDriver scriptingDriver() {
		return mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
	}

	@Test
	void checkDialogChecksTheDialogsSeenAndForgetsThem() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withDialogs(new Dialog(Dialog.Type.BEFOREUNLOAD, "", "http://h/"));
		AbstractSailCall.Context context = context(browser);
		CheckDialog check = new CheckDialog();
		run(check, context);
		AssertionError gone = assertThrows(AssertionError.class, () -> run(check, context));
		browser.withDialogs(new Dialog(Dialog.Type.CONFIRM, "Sure?", "http://h/"));
		run(check, context, "type", "alert", "expected", "false");
		browser.withDialogs(new Dialog(Dialog.Type.ALERT, "Hi", "http://h/"));
		AssertionError unexpected = assertThrows(AssertionError.class, () -> run(check, context, "type", "Alert", "expected", "false"));
		assertAll(() -> assertEquals("Expected a beforeunload dialog, but the dialogs were []", gone.getMessage(), "forgotten after the first check"),
					() -> assertEquals("Expected no alert dialog, but the dialogs were [Dialog[type=ALERT, message=Hi, url=http://h/]]", unexpected.getMessage()),
					() -> assertEquals(List.of("dialogs", "clearDialogs", "dialogs", "clearDialogs", "dialogs", "clearDialogs", "dialogs", "clearDialogs"),
										browser.calls()),
					() -> assertTrue(message(IllegalArgumentException.class, () -> run(new CheckDialog(), context, "type", "toast"))
										.startsWith("CheckDialog's type must be one of [ALERT, CONFIRM, PROMPT, BEFOREUNLOAD]"), "a bad type"));
	}

	/**
	 * A check of a record of dialogs that began in its own step fails, as the record knows nothing of the steps before
	 * and would pass a check that no dialog opened; it says to start watching with an earlier call.
	 */
	@Test
	void checkDialogFailsWhenTheRecordBeganInItsStep() {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context step = context(browser);
		browser.withWatchingSince(Instant.now()); // the control began watching as this step asked for it
		CheckDialog check = new CheckDialog();
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> run(check, step, "expected", "false"));
		assertEquals("CheckDialog reads the record of dialogs, which only began in this step, as the browser's control began watching then:"
						+ " it holds nothing from the steps before. Put a call before the steps to watch, such as RequestLog with clear=true,"
						+ " so watching begins there",
						e.getMessage());
	}

	@Test
	void deleteCookiesDeletesEachNameAndCountsThem() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll()
										.withCookies(new BrowserCookie("JSESSIONID", "1", "h", "/", true),
														new BrowserCookie("remember", "2", "h", "/", false),
														new BrowserCookie("other", "3", "h", "/", false));
		AbstractSailCall.Context context = context(browser);
		String deleted = run(new DeleteCookies(), context, "names", "JSESSIONID, remember, absent");
		assertAll(() -> assertEquals("2", deleted),
					() -> assertEquals(List.of("deleteCookies JSESSIONID", "deleteCookies remember", "deleteCookies absent"), browser.calls()),
					() -> assertEquals("DeleteCookies needs the parameter names", message(IllegalArgumentException.class, () -> run(new DeleteCookies(), context))));
	}

	@Test
	void lowerQuotaLeavesTheHeadroomAboveWhatThePageUsesAndRestores() throws Exception {
		WebDriver driver = scriptingDriver();
		JavascriptExecutor page = (JavascriptExecutor) driver;
		when(page.executeScript("return location.origin;")).thenReturn("http://h:8080");
		when(page.executeAsyncScript(anyString())).thenReturn(Long.valueOf(1000L), "no estimate");
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(driver, browser);
		String quota = run(new LowerQuota(), context, "headroomKb", "2");
		String restored = run(new LowerQuota(), context, "restore", "true");
		LowerQuota lower = new LowerQuota();
		IllegalStateException unknown = assertThrows(IllegalStateException.class, () -> run(lower, context, "headroomKb", "2"));
		assertAll(() -> assertEquals("3048", quota, "1000 bytes used and 2 KB more"),
					() -> assertEquals("restored", restored),
					() -> assertEquals(List.of("overrideQuota http://h:8080 3048", "clearQuotaOverride http://h:8080"), browser.calls()),
					() -> assertEquals("LowerQuota could not read how much storage http://h:8080 uses: no estimate", unknown.getMessage()),
					() -> assertEquals("LowerQuota needs the parameter headroomKb", message(IllegalArgumentException.class, () -> run(new LowerQuota(), context))));
	}

	@Test
	void indexedDbIsBlockedAndTheClockShiftedByInitScripts() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		run(new BlockIndexedDb(), context);
		run(new ShiftClock(), context, "days", "2");
		run(new ShiftClock(), context, "days", "-3650");
		run(new ShiftClock(), context, "days", "2");
		List<String> scripts = browser.initScripts();
		assertAll(() -> assertEquals(4, scripts.size(), "a repeated shift is a script of its own, as the control adds a script once, so it adds"),
					() -> assertEquals("Object.defineProperty(window, 'indexedDB', { value: undefined, configurable: true });", scripts.get(0)),
					() -> assertTrue(scripts.get(1).contains("const offset = 172800000;"), scripts.get(1)),
					() -> assertTrue(scripts.get(2).contains("const offset = -315360000000;"), scripts.get(2)),
					() -> assertTrue(scripts.get(3).contains("const offset = 172800000;"), scripts.get(3)),
					() -> assertEquals("ShiftClock's days must be -3650 or more, not -3651",
										message(IllegalArgumentException.class, () -> run(new ShiftClock(), context, "days", "-3651"))));
	}

	@Test
	void pressKeyPressesAKeyThatManyTimes() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		run(new PressKey(), context, "key", "escape");
		run(new PressKey(), context, "key", "ArrowDown", "count", "2");
		assertAll(() -> assertEquals(List.of("press ESCAPE", "press ARROW_DOWN", "press ARROW_DOWN"), browser.calls()),
					() -> assertEquals("PressKey needs the parameter key", message(IllegalArgumentException.class, () -> run(new PressKey(), context))),
					() -> assertTrue(message(IllegalArgumentException.class, () -> run(new PressKey(), context, "key", "Hyper"))
										.startsWith("PressKey's key must be one of [NULL, CANCEL, HELP"), "a bad key"),
					() -> assertEquals("PressKey's count must be 1 or more, not 0",
										message(IllegalArgumentException.class, () -> run(new PressKey(), context, "key", "TAB", "count", "0"))));
	}

	@Test
	void resizeWindowGivesTheSizeAfterwards() {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		assertAll(() -> assertEquals("1023x700", run(new ResizeWindow(), context, "width", "1023", "height", "700")),
					() -> assertEquals(List.of("resizeWindow 1023 700"), browser.calls()),
					() -> assertEquals("ResizeWindow's width must be 200 or more, not 100",
										message(IllegalArgumentException.class, () -> run(new ResizeWindow(), context, "width", "100", "height", "700"))),
					() -> assertEquals("ResizeWindow needs the parameter height",
										message(IllegalArgumentException.class, () -> run(new ResizeWindow(), context, "width", "300"))));
	}

	@Test
	void freezeTabFreezesAndResumesATabByIndex() throws Exception {
		WebDriver driver = scriptingDriver();
		when(driver.getWindowHandles()).thenReturn(new LinkedHashSet<>(List.of("first", "second")));
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(driver, browser);
		run(new FreezeTab(), context, "action", "freeze", "index", "1");
		run(new FreezeTab(), context, "action", "resume", "index", "1");
		assertAll(() -> assertEquals(List.of("freeze second", "resume second"), browser.calls()),
					() -> assertEquals("FreezeTab can't freeze tab 2 (0 being the first): 2 tab(s) are open",
										message(IllegalStateException.class, () -> run(new FreezeTab(), context, "action", "freeze", "index", "2"))),
					() -> assertEquals("FreezeTab's action must be one of [FREEZE, RESUME], not thaw",
										message(IllegalArgumentException.class, () -> run(new FreezeTab(), context, "action", "thaw", "index", "1"))),
					() -> assertEquals("FreezeTab needs the parameter index",
										message(IllegalArgumentException.class, () -> run(new FreezeTab(), context, "action", "freeze"))));
	}
}

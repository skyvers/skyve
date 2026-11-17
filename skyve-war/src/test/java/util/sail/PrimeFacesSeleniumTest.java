package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.opentest4j.AssertionFailedError;

/**
 * Unit tests of {@link PrimeFacesSelenium}, the base of the Java tests the Control Panel generates, with a mocked driver.
 * A missing element costs the settings' timeout, which the tests set short.
 */
class PrimeFacesSeleniumTest {
	/**
	 * Every view has the messages element, shown even when empty, and a Save puts a "Saved" information message in it:
	 * only an error or fatal message is a failure.
	 */
	@Test
	@SuppressWarnings("static-method")
	void onlyErrorAndFatalMessagesAreUnsuccessful() {
		assertAll(() -> assertTrue(withMessages("").verifySuccess(), "no message"),
					() -> assertTrue(withMessages("<div class=\"ui-messages-info ui-corner-all\"><ul><li>Saved</li></ul></div>").verifySuccess(),
										"an information message"),
					() -> assertFalse(withMessages("<div class=\"ui-messages-error ui-corner-all\"><ul><li>Required</li></ul></div>").verifySuccess(),
										"an error message"),
					() -> assertFalse(withMessages("<div class=\"ui-messages-fatal ui-corner-all\"><ul><li>Broken</li></ul></div>").verifySuccess(),
										"a fatal message"));
	}

	/**
	 * A gesture on an element that is not on the page, is hidden or is disabled fails the test, naming the element,
	 * rather than doing nothing, so a generated test cannot pass without doing what its script says.
	 */
	@Test
	@SuppressWarnings("static-method")
	void gesturesOnMissingHiddenOrDisabledElementsFail() {
		WebElement hidden = element(false, true, null, null);
		WebElement disabled = element(true, false, null, null);
		WebElement disabledAttribute = element(true, true, "disabled", null);
		WebElement disabledStyle = element(true, true, null, "ui-button ui-state-disabled");
		WebElement readOnly = readOnly();
		WebDriver driver = mock(WebDriver.class);
		when(driver.getCurrentUrl()).thenReturn("http://localhost/skyve/?a=e");
		when(driver.findElement(By.id("missing"))).thenThrow(new NoSuchElementException("missing"));
		when(driver.findElement(By.id("hidden"))).thenReturn(hidden);
		when(driver.findElement(By.id("disabled"))).thenReturn(disabled);
		when(driver.findElement(By.id("disabledAttribute"))).thenReturn(disabledAttribute);
		when(driver.findElement(By.id("disabledStyle"))).thenReturn(disabledStyle);
		when(driver.findElement(By.id("readOnly"))).thenReturn(readOnly);
		PrimeFacesSelenium selenium = new PrimeFacesSelenium();
		selenium.driver = driver;
		selenium.settings = timeoutSeconds(1);

		assertAll(() -> assertMessage("#missing is not on the page http://localhost/skyve/?a=e", () -> selenium.button("missing", true, false)),
					() -> assertMessage("#hidden is not displayed on http://localhost/skyve/?a=e", () -> selenium.text("hidden", "x")),
					() -> assertMessage("#disabled is disabled on http://localhost/skyve/?a=e", () -> selenium.radio("disabled", 0)),
					() -> assertMessage("#disabledAttribute is disabled", () -> selenium.selectOne("disabledAttribute", 0)),
					() -> assertMessage("#disabledStyle is disabled", () -> selenium.listGridButton("disabledStyle", "x", false)),
					() -> assertMessage("#readOnly is read only", () -> selenium.text("readOnly", "x")));
	}

	/**
	 * The assertions are JUnit 5's, so a generated test needs no JUnit 4.
	 */
	@Test
	@SuppressWarnings("static-method")
	void assertionsAreJUnit5s() {
		PrimeFacesSelenium selenium = withMessages("<div class=\"ui-messages-error\"><ul><li>Required</li></ul></div>");
		assertThrows(AssertionFailedError.class, selenium::assertSuccess);
	}

	/**
	 * A lookup waits for its element up to the settings' timeout ({@code sail.timeoutSeconds}), not a fixed second: an
	 * element that a slow page shows after 1.5 seconds is found within a 30 second timeout, and a missing element is
	 * given up on only after the timeout. The checks are outcomes and a lower bound, which a slow machine cannot break.
	 */
	@Test
	@SuppressWarnings("static-method")
	void lookupsWaitForTheSettingsTimeout() {
		WebElement late = element(true, true, null, null);
		long shown = System.nanoTime() + 1_500_000_000L;
		WebDriver driver = mock(WebDriver.class);
		when(driver.findElement(By.id("late"))).thenAnswer(invocation -> {
			if (System.nanoTime() < shown) {
				throw new NoSuchElementException("late");
			}
			return late;
		});
		when(driver.findElement(By.id("missing"))).thenThrow(new NoSuchElementException("missing"));
		PrimeFacesSelenium selenium = new PrimeFacesSelenium();
		selenium.driver = driver;
		selenium.settings = timeoutSeconds(30);
		WebElement found = selenium.byId("late");
		selenium.settings = timeoutSeconds(2);
		long start = System.nanoTime();
		WebElement missing = selenium.byId("missing");
		long waited = System.nanoTime() - start;

		assertAll(() -> assertSame(late, found, "the late element"),
					() -> assertNull(missing, "no missing element"),
					() -> assertTrue(waited >= 2_000_000_000L, () -> "gave up on the missing element after " + waited + " ns"));
	}

	/**
	 * A button that goes to another page waits for the page by its view state, and pauses for nothing else: with the
	 * view state changed at the first check, it returns after that one check, and it never sleeps (a sleep on a thread
	 * whose interrupt flag is set would end at once and clear the flag). No time is measured.
	 */
	@Test
	@SuppressWarnings("static-method")
	void aRedirectButtonWaitsOnlyForThePage() {
		WebElement button = element(true, true, null, null);
		WebElement before = viewState("before");
		WebElement after = viewState("after");
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
		when(driver.findElement(By.id("go"))).thenReturn(button);
		List<WebElement> beforeState = List.of(before);
		List<WebElement> afterState = List.of(after);
		when(driver.findElements(By.name("jakarta.faces.ViewState"))).thenReturn(beforeState).thenReturn(afterState);
		when(driver.findElement(By.name("jakarta.faces.ViewState"))).thenReturn(before, after);
		when(((JavascriptExecutor) driver).executeScript("return document.readyState")).thenReturn("complete");
		PrimeFacesSelenium selenium = new PrimeFacesSelenium();
		selenium.driver = driver;
		selenium.settings = timeoutSeconds(1);

		boolean stillInterrupted;
		try {
			Thread.currentThread().interrupt();
			selenium.redirectButton("go", false);
		}
		finally {
			stillInterrupted = Thread.interrupted(); // reads and clears the flag
		}

		// the view state is read once before the click, then twice by the one condition check
		assertAll(() -> assertTrue(stillInterrupted, "nothing slept"),
					() -> verify(driver, times(3)).findElements(By.name("jakarta.faces.ViewState")),
					() -> verify((JavascriptExecutor) driver, times(1)).executeScript("return document.readyState"));
	}

	private static WebElement viewState(String value) {
		WebElement result = element(true, true, null, null);
		when(result.getDomProperty("value")).thenReturn(value);
		return result;
	}

	private static SailSettings timeoutSeconds(long seconds) {
		return SailSettings.resolve(SailSettings.Environment.none(), List.of(SailSettings.code().timeoutSeconds(seconds).layer()));
	}

	private static void assertMessage(String expected, Executable gesture) {
		AssertionError e = assertThrows(AssertionError.class, gesture);
		assertTrue(e.getMessage().startsWith(expected), e::getMessage);
	}

	private static WebElement element(boolean displayed, boolean enabled, String disabled, String classes) {
		WebElement result = mock(WebElement.class);
		when(Boolean.valueOf(result.isDisplayed())).thenReturn(Boolean.valueOf(displayed));
		when(Boolean.valueOf(result.isEnabled())).thenReturn(Boolean.valueOf(enabled));
		when(result.getDomAttribute("disabled")).thenReturn(disabled);
		when(result.getDomAttribute("class")).thenReturn(classes);
		return result;
	}

	private static WebElement readOnly() {
		WebElement result = element(true, true, null, null);
		when(result.getDomAttribute("readonly")).thenReturn("readonly");
		return result;
	}

	/**
	 * Returns a PrimeFacesSelenium whose page shows the messages element with the given content.
	 */
	private static PrimeFacesSelenium withMessages(String innerHTML) {
		WebElement messages = mock(WebElement.class);
		when(Boolean.valueOf(messages.isDisplayed())).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(messages.isEnabled())).thenReturn(Boolean.TRUE);
		when(messages.getDomProperty("innerHTML")).thenReturn(innerHTML);
		WebDriver driver = mock(WebDriver.class);
		when(driver.findElements(By.id("messages"))).thenReturn(List.of(messages));
		when(driver.findElement(By.id("messages"))).thenReturn(messages);
		PrimeFacesSelenium result = new PrimeFacesSelenium();
		result.driver = driver;
		return result;
	}
}

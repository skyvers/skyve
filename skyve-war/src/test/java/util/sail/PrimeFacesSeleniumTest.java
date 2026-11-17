package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.opentest4j.AssertionFailedError;

/**
 * Unit tests of {@link PrimeFacesSelenium}, the base of the Java tests the Control Panel generates, with a mocked driver.
 * A missing element costs the harness's one second wait for it.
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
		when(driver.findElement(By.id("messages"))).thenReturn(messages);
		PrimeFacesSelenium result = new PrimeFacesSelenium();
		result.driver = driver;
		return result;
	}
}

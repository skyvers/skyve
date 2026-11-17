package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

/**
 * Unit tests of {@link PrimeFacesSelenium}, the base of the Java tests the Control Panel generates, with a mocked driver.
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

package util.sail;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.TargetLocator;

/**
 * Unit tests of the window gestures in {@link Selenide} that need no browser: the driver is a mock.
 */
@SuppressWarnings("static-method")
class SelenideWindowsTest {
	/**
	 * A new window whose page fails to load is closed, and the driver is back in the window that opened it, so the
	 * failed step leaves the automation where it was: nothing else tracks the new window, so nothing else would close it.
	 */
	@Test
	@SuppressWarnings("unchecked") // Mockito's thenReturn(T, T...) with a generic Set
	void aNewWindowThatLoadsNoPageIsClosedAndTheDriverReturnsToItsOpener() {
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
		JavascriptExecutor js = (JavascriptExecutor) driver;
		TargetLocator target = mock(TargetLocator.class);
		when(driver.switchTo()).thenReturn(target);
		when(driver.getWindowHandle()).thenReturn("opener");
		when(driver.getWindowHandles()).thenReturn(Set.of("opener"), Set.of("opener", "opened"));
		// the new window's page cannot be read: the wait for it to load fails at once
		when(js.executeScript(contains("about:blank"))).thenThrow(new IllegalStateException("the new window's page cannot be read"));
		PrimeFacesSelenide selenide = new PrimeFacesSelenide();
		selenide.driver = driver;

		assertThrows(IllegalStateException.class, () -> selenide.openNewWindow(() -> { /* the gesture */ }, "The link"));

		InOrder order = inOrder(target, driver);
		order.verify(target).window("opened");
		order.verify(driver).close();
		order.verify(target).window("opener");
	}
}

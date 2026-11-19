package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

/**
 * Unit tests of how the sign in page's Remember me box is set, which need no browser: the driver is a mock.
 */
@SuppressWarnings("static-method")
class RememberMeTest {
	private static final By BOX = By.id("remember");
	private static final By LABEL = By.cssSelector("label[for=\"remember\"]");

	/** A browser whose Remember me box is ticked as given, and whose label click toggles the box when it works. */
	private static final class Page {
		final WebDriver driver = mock(WebDriver.class);
		final WebElement box = mock(WebElement.class);
		final WebElement label = mock(WebElement.class);
		private boolean ticked;

		Page(boolean ticked, boolean clickWorks) {
			this.ticked = ticked;
			when(driver.findElement(BOX)).thenReturn(box);
			when(driver.findElement(LABEL)).thenReturn(label);
			when(Boolean.valueOf(box.isSelected())).thenAnswer(i -> Boolean.valueOf(this.ticked));
			if (clickWorks) {
				org.mockito.Mockito.doAnswer(i -> {
					this.ticked = ! this.ticked;
					return null;
				}).when(label).click();
			}
		}

		void setRememberMe(boolean remember) {
			PrimeFacesSelenium selenium = new PrimeFacesSelenium();
			selenium.driver = driver;
			selenium.setRememberMe(remember);
		}
	}

	/** The label is clicked once when the box shows the other value, and never when it shows the value already. */
	@Test
	void theLabelIsClickedOnlyToChangeTheBox() {
		Page untickedToTick = new Page(false, true);
		Page tickedToUntick = new Page(true, true);
		Page tickedToTick = new Page(true, true);
		Page untickedToUntick = new Page(false, true);
		untickedToTick.setRememberMe(true);
		tickedToUntick.setRememberMe(false);
		tickedToTick.setRememberMe(true);
		untickedToUntick.setRememberMe(false);
		assertAll(() -> verify(untickedToTick.label).click(),
					() -> verify(tickedToUntick.label).click(),
					() -> verify(tickedToTick.label, never()).click(),
					() -> verify(untickedToUntick.label, never()).click(),
					() -> verify(tickedToTick.driver, never()).findElement(LABEL));
	}

	/** A click that does not change the box fails the step, rather than signing in with the wrong value. */
	@Test
	void aClickThatDoesNotSetTheBoxFails() {
		Page page = new Page(false, false);
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> page.setRememberMe(true));
		assertAll(() -> org.junit.jupiter.api.Assertions.assertEquals("The Remember me box did not take the value true", e.getMessage()),
					() -> verify(page.driver, never()).get(any()));
	}
}

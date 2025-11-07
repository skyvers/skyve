package util.sail;

import static com.codeborne.selenide.Condition.enabled;
import static com.codeborne.selenide.Condition.exist;
import static com.codeborne.selenide.Condition.visible;
import static com.codeborne.selenide.Selectors.byId;
import static com.codeborne.selenide.Selenide.$;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.codeborne.selenide.SelenideElement;

import util.sail.commands.impl.SmartClientButtonCommand;
import util.sail.commands.impl.SmartClientCheckboxCommand;
import util.sail.commands.impl.SmartClientDataGridButtonCommand;
import util.sail.commands.impl.SmartClientDataGridSelectCommand;
import util.sail.commands.impl.SmartClientListGridButtonCommand;
import util.sail.commands.impl.SmartClientListGridSelectCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionByRowCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionBySearchCommand;
import util.sail.commands.impl.SmartClientRadioCommand;
import util.sail.commands.impl.SmartClientSelectOneCommand;
import util.sail.commands.impl.SmartClientTabCommand;
import util.sail.commands.impl.SmartClientTextCommand;

/**
 * Selenide implementation for the SmartClient framework,
 * binding SmartClient-specific command types to enable typed, framework-specific UI automation.
 * 
 * @author simeonsolomou
 */
public class SmartClientSelenide extends Selenide<
	SmartClientButtonCommand,
	SmartClientCheckboxCommand,
	SmartClientDataGridButtonCommand,
	SmartClientDataGridSelectCommand,
	SmartClientListGridButtonCommand,
	SmartClientListGridSelectCommand,
	SmartClientLookupDescriptionByRowCommand,
	SmartClientLookupDescriptionBySearchCommand,
	SmartClientRadioCommand,
	SmartClientTabCommand,
	SmartClientTextCommand,
	SmartClientSelectOneCommand> {

	// 50ms x 100 = 10s
	private static final long WAIT_UNTIL_IDLE_MILLIS = 50;
	private static final long WAIT_UNTIL_IDLE_MAX_MILLIS = 10000;

	// 50ms x 20 = 1s
	private static final long SLEEP_FOR_RETRY_MILLIS = 50;
	private static final int SLEEP_FOR_RETRY_MAX_ATTEMPTS = 20;

	@Override
	public boolean button(SmartClientButtonCommand command) {
		String locator = command.locator();
		boolean confirm = command.confirm();

		SelenideElement element = locate(locator);
		if (element != null && !disabledClass(element)) {
			prepare(element);
			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean checkbox(SmartClientCheckboxCommand command) {
		String locator = command.locator();
		Boolean value = command.value();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);

			// Detect the current state
			Boolean current = null;
			String aria = element.getAttribute("aria-checked");
			if ("true".equalsIgnoreCase(aria)) {
				current = Boolean.TRUE;
			} else if ("false".equalsIgnoreCase(aria)) {
				current = Boolean.FALSE;
			}

			boolean success = false;
			int attempts = 0;
			final int maxAttempts = 3;

			// Attempt until desired state is reached or max attempts reached
			while (!Objects.equals(current, value) && attempts < maxAttempts) {
				attempts++;
				click(element);

				// Evaluate state
				aria = element.getAttribute("aria-checked");

				if ("true".equalsIgnoreCase(aria)) {
					current = Boolean.TRUE;
				} else if ("false".equalsIgnoreCase(aria)) {
					current = Boolean.FALSE;
				} else {
					current = null;
				}
			}

			// Succeed if checkbox value equals desired value
			if (Objects.equals(current, value)) {
				success = true;
			}

			return success;
		}

		return false;
	}

	@Override
	public boolean dataGridButton(SmartClientDataGridButtonCommand command) {
		String locator = command.locator();
		boolean confirm = command.confirm();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);
			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean dataGridSelect(SmartClientDataGridSelectCommand command) {
		String locator = command.locator();
		int row = command.row();

		SelenideElement element = locate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean listGridButton(SmartClientListGridButtonCommand command) {
		String locator = command.locator();
		boolean confirm = command.confirm();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);
			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean listGridSelect(SmartClientListGridSelectCommand command) {
		String locator = command.locator();
		int row = command.row();

		SelenideElement element = locate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean lookupDescriptionByRow(SmartClientLookupDescriptionByRowCommand command) {
		String locator = command.locator();
		int row = command.row();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);

			// Find the drop down button
			element = locate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/[icon=\"picker\"]", locator));
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			// Wait for the pick list drop down
			element = locate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator));
			element.shouldBe(visible);

			// Value here should be an index in the drop down starting from 0
			element = locate(String.format(
					"%s/item[Class=ComboBoxItem||name=_combo]/pickList/body/row[%d]/col[0]", locator,
					Integer.valueOf(row)));
			element.click();

			return true;
		}

		return false;
	}

	@Override
	public boolean lookupDescriptionBySearch(SmartClientLookupDescriptionBySearchCommand command) {
		String locator = command.locator();
		String search = command.search();
		
		SelenideElement element = locate(locator);
		if (element != null && element.exists()) {
			prepare(element);
			
			// Enter search
			String searchBarLocator = String.format("%s/item[Class=ComboBoxItem||name=_combo]/element", locator);
			boolean searchEntered = text(new SmartClientTextCommand(searchBarLocator, search, true));
			if (searchEntered) {
				// Wait for the pick list drop down
				element = locate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator));
				if (!element.is(visible)) {
					element.scrollIntoView(true);
				}

				element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);
				
				// Select the first row
				element = locate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList/body/row[0]", locator));
				element.click();

				return true;
			}
		}
		
		return false;
	}

	@Override
	public boolean radio(SmartClientRadioCommand command) {
		String locator = command.locator();
		int index = command.index();

		SelenideElement element = locate(String.format("%s/item[index=%d]/element", locator, Integer.valueOf(index)));
		if (element != null && !disabledClass(element)) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean tab(SmartClientTabCommand command) {
		String locator = command.locator();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean text(SmartClientTextCommand command) {
		String locator = command.locator();
		String value = command.value();
		boolean keyPresses = command.keyPresses();

		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);

			if (element.isDisplayed()
					&& element.isEnabled()
					&& element.attr("disabled") == null
					&& element.attr("readonly") == null) {
				element.clear();

				if (keyPresses) {
					if (value != null) {
						element.sendKeys(value);
					}
				} else {
					element.val(value == null ? "" : value);
				}

				return true;
			}
		}

		return false;
	}

	@Override
	public boolean selectOne(SmartClientSelectOneCommand command) {
		String locator = command.locator();
		int index = command.index();

		SelenideElement element = locate(locator);
		if (element != null && !disabledClass(element)) {
			prepare(element);

			element = locate(String.format("%s/[icon=\"picker\"]", locator));
			prepare(element);
			click(element);

			// Wait for pick list drop down
			element = locate(String.format("%s/pickList", locator));
			prepare(element);

			// Value here should be an index in the drop down starting from 0
			element = locate(String.format("%s/pickList/body/row[%d]", locator, Integer.valueOf(index)));
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean confirm() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"Yes\"]");
		if (element != null) {
			click(element);

			return true;
		}

		return false;
	}

	@Override
	public boolean verifySuccess() {
		// 1. Check for global warning dialog first (RuntimeException)
		SelenideElement dialogElement = locate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/");
		if (dialogElement != null && dialogElement.isDisplayed()) {
			String innerHTML = dialogElement.getDomProperty("innerHTML");

			System.err.println("**************");
			System.err.println("Not successful");
			System.err.println(innerHTML);
			System.err.println("**************");

			return false;
		}

		// 2. If no dialog, check for any visible error icons on the page (ValidationException)
		List<WebElement> errorIcons = driver.findElements(By.xpath("//img[contains(@src, 'exclamation.png') and @aria-label]"));

		boolean visibleErrorFound = errorIcons.stream()
				.anyMatch(WebElement::isDisplayed);
		if (visibleErrorFound) {
			System.err.println("**************");
			System.err.println("Not successful - error indicators found on form items.");
			System.err.println("Count: " + errorIcons.size());
			System.err.println("**************");

			return false;
		}

		// 3. No errors found at all
		return true;
	}

	@Override
	public void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	@Override
	public boolean verifyFailure(String messageToCheck) {
		// 1. Check for global warning dialog first (RuntimeException)
		SelenideElement dialogElement = locate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/");
		if (dialogElement != null && dialogElement.isDisplayed()) {
			String innerHTML = dialogElement.getDomProperty("innerHTML");

			if (messageToCheck == null || innerHTML.contains(messageToCheck)) {
				return true;
			}

			System.err.println("**************");
			System.err.println("Not successful - error present, but expected message not found:");
			System.err.println(innerHTML);
			System.err.println("**************");
			return false;
		}

		// 2. If no dialog, check for any visible error icons on the page (ValidationException)
		List<WebElement> errorIcons = driver.findElements(By.xpath("//img[contains(@src, 'exclamation.png') and @aria-label]"));

		if (!errorIcons.isEmpty()) {
			System.err.println("**************");
			System.err.println("Error indicators found on one or more form items.");
			System.err.println("Count: " + errorIcons.size());
			System.err.println("**************");
			return true;
		}

		// 3. No errors found at all
		System.err.println("**************");
		System.err.println("Not successful - no error messages or icons found.");
		System.err.println("**************");
		return false;
	}

	@Override
	public boolean verifyFailure() {
		return verifyFailure(null);
	}

	@Override
	public void assertFailure(String messageToCheck) {
		Assert.assertTrue("Successful", verifyFailure(messageToCheck));
	}

	@Override
	public void assertFailure() {
		Assert.assertTrue("Successful", verifyFailure());
	}

	/**
	 * Locates the element specified by the given locator.
	 *
	 * @param locator the SmartClient locator string
	 * @return the located SelenideElement; or null if the element cannot be found
	 */
	public SelenideElement locate(String locator) {
		trace("Locating... " + locator);
		
		for (int attempt = 1; attempt <= SLEEP_FOR_RETRY_MAX_ATTEMPTS; attempt++) {
			waitForFullPageResponse();

			try {
				WebElement element = com.codeborne.selenide.Selenide
						.executeJavaScript("return isc.AutoTest.getElement(arguments[0])", locator);
				if (element == null) {
					continue;
				}
				
				SelenideElement result = $(element);
				result.should(exist);

				return result;
			} catch (@SuppressWarnings("unused") StaleElementReferenceException | AssertionError e) {
				// Do nothing
			}

			if (attempt < SLEEP_FOR_RETRY_MAX_ATTEMPTS) {
				sleepForRetry(locator);
			}
		}

		return null;
	}
	
	/**
	 * Sleeps for a configured interval between retries.
	 *
	 * @param locator the locator string for logging purposes
	 */
	private static void sleepForRetry(String locator) {
		try {
			Thread.sleep(SLEEP_FOR_RETRY_MILLIS);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();

			throw new IllegalStateException(String.format("Interrupted while waiting for element: %s", locator), e);
		}
	}

	/**
	 * Performs a lookup description new interaction.
	 * 
	 * @param locator the locator string for the lookup description widget
	 * @return true if successful, false otherwise
	 */
	public boolean lookupDescriptionNew(String locator) {
		SelenideElement element = locate(locator);
		if (element != null && element.exists()) {
			prepare(element);

			// Select the drop-down
			element = locate(String.format(
					"%s/item[Class=CanvasItem||name=_splitButton]/canvas/member[Class=MenuButton||index=1]",
					locator));
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			// Select the edit button
			element = locate("//Menu[level=0]/body/row[title=New]/col[fieldName=title]");
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			return true;
		}

		return false;
	}

	/**
	 * Performs a lookup description edit interaction.
	 * 
	 * @param locator the locator string for the lookup description widget
	 * @return true if successful, false otherwise
	 */
	public boolean lookupDescriptionEdit(String locator) {
		SelenideElement element = locate(locator);
		if (element != null && element.exists()) {
			prepare(element);

			// Select the drop-down
			element = locate(String.format(
					"%s/item[Class=CanvasItem||name=_splitButton]/canvas/member[Class=MenuButton||index=1]",
					locator));
			prepare(element);
			click(element);

			// Select the edit button
			element = locate("//Menu[level=0]/body/row[title=Edit]/col[fieldName=title]");
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	/**
	 * Clicks the global OK button on a dialog if it is present.
	 *
	 * @return {@code true} if the button was found and clicked, {@code false} otherwise
	 */
	public boolean ok() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"OK\"]");
		if (element != null && element.exists()) {
			click(element);

			return true;
		}

		return false;
	}

	/**
	 * Waits for the full page to load and for the system to become idle.
	 */
	public void waitForFullPageResponse() {
		// Wait up to 5 seconds for the page to be fully loaded
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(5));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState")
				.equals("complete") ? Boolean.TRUE : Boolean.FALSE);

		// Wait until SmartClient system is idle
		waitUntilIdle();
	}

	/**
	 * Waits for the SmartClient system to become idle by repeatedly checking the isc.AutoTest.isSystemDone() status.
	 */
	private static void waitUntilIdle() {
		boolean done = false;

		for (long l = 0; l <= WAIT_UNTIL_IDLE_MAX_MILLIS; l += WAIT_UNTIL_IDLE_MILLIS) {
			Boolean isDone;

			try {
				isDone = com.codeborne.selenide.Selenide.executeJavaScript("return isc?.AutoTest?.isSystemDone()");

				// If status is null, fallback to short sleep and return
				if (isDone == null) {
					sleepSilently(WAIT_UNTIL_IDLE_MILLIS, "Interrupted while waiting");
					return;
				}
			} catch (@SuppressWarnings("unused") Exception e) {
				// If JS execution fails, fallback to short sleep and return
				sleepSilently(WAIT_UNTIL_IDLE_MILLIS, "Interrupted while waiting");
				return;
			}

			// Exit loop if system is done
			if (Boolean.TRUE.equals(isDone)) {
				done = true;
				break;
			}

			// Wait before retrying
			sleepSilently(WAIT_UNTIL_IDLE_MILLIS, "Could not wait until system is responsive");
		}

		// Throw if system never became idle
		if (!done) {
			throw new IllegalStateException(
					String.format("The system is not responsive after %d seconds", Long.valueOf(WAIT_UNTIL_IDLE_MAX_MILLIS / 1000)));
		}
	}

	/**
	 * Helper method to sleep and handle InterruptedException.
	 * 
	 * @param millis the number of milliseconds to sleep
	 * @param errorMessage the error message to include if the sleep is interrupted
	 */
	private static void sleepSilently(long millis, String errorMessage) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			throw new IllegalStateException(errorMessage, e);
		}
	}

	/**
	 * Clicks the specified element once.
	 *
	 * @param element the element to click
	 */
	public void click(SelenideElement element) {
		click(element, false);
	}

	/**
	 * Performs a double-click on the specified element.
	 *
	 * @param element the element to double-click
	 */
	public void doubleClick(SelenideElement element) {
		click(element, true);
	}

	/**
	 * Attempts to click or double-click the given element, retrying with different
	 * scroll positions if the element is obstructed or off-screen.
	 *
	 * @param element the element to click
	 * @param doubleClick {@code true} to perform a double-click, {@code false} for a single click
	 */
	private void click(SelenideElement element, boolean doubleClick) {
		try {
			try {
				// Scroll the element into view on the page and see if the element can be made visible
				if (!element.is(visible, Duration.ofMillis(250))) {
					element.scrollIntoView(true);
				}
				
				clickIt(element, doubleClick);
			} catch (@SuppressWarnings("unused") WebDriverException e) {
				// This could occur when the control is behind a floating element
				JavascriptExecutor js = (JavascriptExecutor) driver;

				try {
					// Scroll to the top of the page and see if the element can be made visible
					trace("    Could not click on the element - scroll to the top of the page and try again");

					js.executeScript("javascript:window.scrollTo(0, 0)");
					clickIt(element, doubleClick);
				} catch (@SuppressWarnings("unused") WebDriverException e1) {
					try {
						// Scroll to the bottom of the page and try again in case the floating element is at the bottom
						trace("    Could not click on the element - scroll to the bottom of the page and try again");

						js.executeScript("javascript:window.scrollTo(0, 999999)");
						clickIt(element, doubleClick);
					} catch (@SuppressWarnings("unused") WebDriverException e2) {
						// Scroll the element into view on the page using javascript and see if the element can be made visible
						trace("    Could not click on the element - scroll to the element and try again");

						js.executeScript("arguments[0].scrollIntoView(true);", element);
						clickIt(element, doubleClick);
					}
				}
			}
		} catch (@SuppressWarnings("unused") StaleElementReferenceException ser) {
			String id = element.attr("id");
			if (id != null) {
				click($(byId(id)), doubleClick);
			}
		}
	}
	
	/**
	 * Performs the actual single or double click on the given element.
	 *
	 * @param element the element to click
	 * @param doubleClick {@code true} to perform a double-click, {@code false} for a single click
	 */
	private static void clickIt(SelenideElement element, boolean doubleClick) {
		if (doubleClick) {
			element.doubleClick();
		} else {
			element.click();
		}
	}

	/**
	 * Checks whether the element has a CSS class indicating it is disabled.
	 *
	 * @param element the element to inspect
	 * @return {@code true} if the element has a "ui-state-disabled" class, {@code false} otherwise
	 */
	private static boolean disabledClass(SelenideElement element) {
		String styleClass = element.attr("class");

		return styleClass != null && styleClass.contains("ui-state-disabled");
	}

	/**
	 * Ensures the given element is visible and enabled for interaction.
	 *
	 * @param element the {@link SelenideElement} to prepare
	 */
	private static void prepare(SelenideElement element) {
		element.should(exist);
		
		if (!element.is(visible)) {
			element.scrollIntoView(true);
		}

		element.shouldBe(visible)
				.shouldBe(enabled);
	}
}

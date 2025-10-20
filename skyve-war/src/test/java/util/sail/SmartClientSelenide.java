package util.sail;

import static com.codeborne.selenide.Condition.enabled;
import static com.codeborne.selenide.Condition.exist;
import static com.codeborne.selenide.Condition.visible;
import static com.codeborne.selenide.Selectors.byId;
import static com.codeborne.selenide.Selectors.byName;
import static com.codeborne.selenide.Selenide.$;
import static com.codeborne.selenide.Selenide.open;

import java.time.Duration;

import org.junit.Assert;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.skyve.domain.messages.DomainException;

import com.codeborne.selenide.Selenide;
import com.codeborne.selenide.SelenideElement;
import com.codeborne.selenide.WebDriverRunner;

/**
 * Provides Selenide-based automation for interacting with SmartClient.
 * 
 * @author simeonsolomou
 */
public class SmartClientSelenide extends CrossBrowserSelenium {

	private String baseUrl;

	// 50ms x 100 = 10s
	private static final long WAIT_UNTIL_IDLE_MILLIS = 50;
	private static final long WAIT_UNTIL_IDLE_MAX_MILLIS = 10000;

	// 50ms x 100 = 5s
	private static final long SLEEP_FOR_RETRY_MILLIS = 50;
	private static final int SLEEP_FOR_RETRY_MAX_ATTEMPTS = 200;

	@Override
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		super.startBrowser(configuration);

		WebDriverRunner.setWebDriver(driver);
		this.baseUrl = configuration.getBaseUrl();
	}
	
	public void get(String url) {
		open(baseUrl + url);
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
				WebElement element = Selenide.executeJavaScript("return isc.AutoTest.getElement(arguments[0])", locator);
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
	
	public boolean tab(String locator) {
		SelenideElement element = locate(locator);
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	public void login(String username, String password) {
		login(null, username, password);
	}
	
	public void login(String customer, String username, String password) {
		open(baseUrl);
		if (customer != null) {
			$(byName("customer")).val(customer);
		}

		$(byName("user")).val(username);
		$(byName("password")).val(password);

		$("input[type=\"submit\"").click();
	}

	public void logout() {
		open(baseUrl + "loggedOut");
	}

	public boolean checkbox(String locator, Boolean value) {
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

			// Attempt up to 2 times to reach the desired state
			for (int i = 0; i < 2; i++) {
				if ((current == null && value == null) || (current != null && current.equals(value))) {
					return true; // No changes required
				}

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
			if ((current == null && value == null) || (current != null && current.equals(value))) {
				return true;
			}

			return false;
		}

		return false;
	}

	public boolean _input(String locator, String value, boolean keyPresses) {
		return text(locator, value, keyPresses);
	}

	public boolean text(String locator, String value, boolean keyPresses) {
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

	public boolean radio(String locator, int index) {
		SelenideElement element = locate(String.format("%s/item[index=%d]/element", locator, Integer.valueOf(index)));
		if (element != null && !disabledClass(element)) {
			prepare(element);
			click(element);
			
			return true;
		}

		return false;
	}

	public boolean selectOne(String locator, int index) {
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

	public boolean button(String locator, boolean confirm) {
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

	public boolean lookupDescription(String locator, int row) {
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

	public boolean lookupDescription(String locator, String search) {
		SelenideElement element = locate(locator);
		if (element != null && element.exists()) {
			prepare(element);
			
			// Enter search
			boolean searchEntered = text(String.format("%s/item[Class=ComboBoxItem||name=_combo]/element", locator), search, true);
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

	public boolean dataGridButton(String locator, boolean confirm) {
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

	public boolean dataGridSelect(String locator, int row) {
		SelenideElement element = locate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	public boolean listGridButton(String locator, boolean confirm) {
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

	public boolean listGridSelect(String locator, int row) {
		SelenideElement element = locate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			prepare(element);
			click(element);

			return true;
		}

		return false;
	}

	public boolean verifySuccess() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/");
		if (element != null && element.isDisplayed()) {
			String innerHTML = element.getDomProperty("innerHTML");

			System.err.println("**************");
			System.err.println("Not successful");
			System.err.println(innerHTML);
			System.err.println("**************");

			return false;
		}

		return true;
	}

	public void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	public boolean verifyFailure(String messageToCheck) {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/");
		if (element != null && element.isDisplayed()) {
	        String innerHTML = element.getDomProperty("innerHTML");

	        // If no specific message to check, just presence of an error counts as failure
	        if (messageToCheck == null) {
	            return true;
	        }

	        // If specific message is provided, check for its presence
	        if (innerHTML.contains(messageToCheck)) {
	            return true;
	        }

	        System.err.println("**************");
			System.err.println("Not successful - error present, but expected message not found:");
	        System.err.println(innerHTML);
	        System.err.println("**************");

	        return false;
	    }

		System.err.println("**************");
		System.err.println("Not successful - no error messages.");
		System.err.println("**************");

		return false;
	}

	public boolean verifyFailure() {
		return verifyFailure(null);
	}

	public void assertFailure(String messageToCheck) {
		Assert.assertTrue("Successful", verifyFailure(messageToCheck));
	}

	public void assertFailure() {
		Assert.assertTrue("Successful", verifyFailure());
	}

	public boolean confirm() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"Yes\"]");
		if (element != null) {
			click(element);

			return true;
		}

		return false;
	}

	public boolean ok() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"OK\"]");
		if (element != null && element.exists()) {
			click(element);

			return true;
		}

		return false;
	}

	public boolean okIfPresent() {
		SelenideElement element = locate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"OK\"]");
		if (element != null) {
			click(element);

			return true;
		}

		return false;
	}

	/**
	 * Waits for the full page to load and for the system to become idle.
	 * <p>
	 * First, waits until the document's ready state is 'complete'.
	 * Then, waits for the SmartClient system to signal that it is idle.
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
	 * <p>
	 * If the status is {@code null} or JavaScript execution fails (e.g., when the ISC framework is not accessible, or the current
	 * view is outside the SmartClient context such as the login page), the method falls back to a short wait of 0.5 seconds before
	 * returning. This ensures that automated tests do not fail in non-SC contexts while still providing responsiveness checks when
	 * SmartClient is available.
	 */
	private static void waitUntilIdle() {
		boolean done = false;

		for (long l = 0; l <= WAIT_UNTIL_IDLE_MAX_MILLIS; l += WAIT_UNTIL_IDLE_MILLIS) {
			Boolean isDone;

			try {
				isDone = Selenide.executeJavaScript("return isc?.AutoTest?.isSystemDone()");

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

	@SuppressWarnings("static-method")
	public void trace(String comment) {
		System.out.println(comment);
	}
	
	@SuppressWarnings("static-method")
	public void pause(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			throw new DomainException("Couldn't pause", e);
		}
	}

	public void click(SelenideElement element) {
		click(element, false);
	}

	public void doubleClick(SelenideElement element) {
		click(element, true);
	}

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
	
	private static void clickIt(SelenideElement element, boolean doubleClick) {
		if (doubleClick) {
			element.doubleClick();
		} else {
			element.click();
		}
	}

	private static boolean disabledClass(SelenideElement element) {
		String styleClass = element.attr("class");

		return styleClass != null && styleClass.contains("ui-state-disabled");
	}

	/**
	 * Ensures the given element is visible and enabled for interaction.
	 * <p>
	 * If the element is not currently visible, it is scrolled into view.
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

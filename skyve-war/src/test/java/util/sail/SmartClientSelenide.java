package util.sail;

import static com.codeborne.selenide.Condition.appear;
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
	
	private static final int DEFAULT_RETRY_COUNT = 3;

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
	 * Locates the element specified by the given locator, waiting indefinitely until it is found.
	 *
	 * @param locator the SmartClient locator string
	 * @return the located SelenideElement
	 * @throws IllegalStateException if the element cannot be found
	 */
	public SelenideElement waitAndLocate(String locator) {
		return waitAndLocate(locator, -1, false);
	}

	/**
	 * Locates the element specified by the given locator, waiting up to a default number of retries.
	 *
	 * @param locator the SmartClient locator string
	 * @param nullable if true, returns null when the element is not found; otherwise throws
	 * @return the located SelenideElement, or null if nullable and not found
	 * @throws IllegalStateException if the element cannot be found and nullable is false
	 */
	public SelenideElement waitAndLocate(String locator, boolean nullable) {
		return waitAndLocate(locator, DEFAULT_RETRY_COUNT, nullable);
	}

	/**
	 * Locates the element specified by the given locator, retrying up to a specified number of times.
	 *
	 * @param locator the SmartClient locator string
	 * @param maxRetries maximum number of retry attempts; -1 indicates unlimited retries
	 * @param nullable if true, returns null when the element is not found; otherwise throws
	 * @return the located SelenideElement, or null if nullable and not found
	 * @throws IllegalStateException if the element cannot be found and nullable is false
	 */
	public SelenideElement waitAndLocate(String locator, int maxRetries, boolean nullable) {
		int attempt = 0;

		while (maxRetries < 0 || attempt <= maxRetries) {
			SelenideElement element = locate(locator);
			if (element != null) {
				return element;
			}

			if (maxRetries > 0 && attempt >= maxRetries) {
				break;
			}

			sleepForRetry(locator);
			attempt++;
		}

		if (nullable) {
			trace("Element not found after retries, returning null");
			return null;
		}

		throw new IllegalStateException(String.format(
				"Element not found after %s retries: %s",
				(maxRetries < 0 ? "unlimited" : Integer.valueOf(maxRetries)),
				locator));
	}

	/**
	 * Attempts to immediately locate the element specified by the given locator without waiting.
	 *
	 * @param locator the SmartClient locator string
	 * @return the located SelenideElement, or null if not found or stale
	 */
	public SelenideElement locate(String locator) {
		trace("Locating... " + locator);

		try {
			WebElement element = Selenide.executeJavaScript("return isc.AutoTest.getElement(arguments[0])", locator);
			if (element == null) {
				return null;
			}

			SelenideElement result = $(element);
			result.getWrappedElement()
					.isEnabled();

			return result;
		} catch (@SuppressWarnings("unused") StaleElementReferenceException se) {
			return null;
		}
	}

	/**
	 * Sleeps for a configured interval between retries.
	 *
	 * @param locator the locator string for logging purposes
	 */
	private static void sleepForRetry(String locator) {
		try {
			Thread.sleep(WAIT);
		} catch (InterruptedException e) {
			Thread.currentThread()
					.interrupt();

			throw new IllegalStateException("Interrupted while waiting for element: " + locator, e);
		}
	}
	
	public boolean tab(String locator) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
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
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			element.shouldBe(visible)
					.shouldBe(enabled);
	
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
		// TODO

		return text(locator, value, keyPresses);
	}

	public boolean text(String locator, String value, boolean keyPresses) {
		boolean success = false;

		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

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

				success = true;
			}
		}

		return success;
	}

	public boolean radio(String locator, int index) {
		SelenideElement element = waitAndLocate(String.format("%s/item[index=%d]/element", locator, Integer.valueOf(index)));
		if (element.exists() && (!disabledClass(element))) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.shouldBe(enabled);
			
			click(element);
			
			return true;
		}

		return false;
	}

	public boolean selectOne(String locator, int index) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists() && (! disabledClass(element))) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.shouldBe(enabled);

			element = waitAndLocate(String.format("%s/[icon=\"picker\"]", locator));
			element.should(exist);
			click(element);

			// Wait for pick list drop down
			element = waitAndLocate(String.format("%s/pickList", locator));
			element.should(appear);

			// Value here should be an index in the drop down starting from 0
			element = waitAndLocate(String.format("%s/pickList/body/row[%d]", locator, Integer.valueOf(index)));
			element.should(exist);
			click(element);
			
			return true;
		}

		return false;
	}

	public boolean button(String locator, boolean confirm) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists() && !disabledClass(element)) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

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
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}
			
			element.shouldBe(visible)
					.should(enabled);

			// Find the drop down button
			element = waitAndLocate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/[icon=\"picker\"]", locator));
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			// Wait for the pick list drop down
			element = waitAndLocate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator));
			element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);
			
			// Value here should be an index in the drop down starting from 0
			element = waitAndLocate(String.format(
					"%s/item[Class=ComboBoxItem||name=_combo]/pickList/body/row[%d]/col[0]", locator,
					Integer.valueOf(row)));
			element.should(exist);
			element.click();

			return true;
		}

		return false;
	}

	public boolean lookupDescription(String locator, String search) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.should(enabled);
			
			// Enter search
			text(String.format("%s/item[Class=ComboBoxItem||name=_combo]/element", locator), search, true);

			// Wait for the pick list drop down
			element = waitAndLocate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator));
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);
			
			// Select the first row
			element = waitAndLocate(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList/body/row[0]", locator));
			element.click();

			return true;
		}
		
		return false;
	}

	public boolean lookupDescriptionNew(String locator) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.should(enabled);

			// Select the drop-down
			element = waitAndLocate(String.format(
					"%s/item[Class=CanvasItem||name=_splitButton]/canvas/member[Class=MenuButton||index=1]",
					locator));
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			// Select the edit button
			element = waitAndLocate("//Menu[level=0]/body/row[title=New]/col[fieldName=title]");
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			return true;
		}

		return false;
	}

	public boolean lookupDescriptionEdit(String locator) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.should(enabled);

			// Select the drop-down
			element = waitAndLocate(String.format(
					"%s/item[Class=CanvasItem||name=_splitButton]/canvas/member[Class=MenuButton||index=1]",
					locator));
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			// Select the edit button
			element = waitAndLocate("//Menu[level=0]/body/row[title=Edit]/col[fieldName=title]");
			element.shouldBe(visible)
					.should(enabled);
			click(element);

			return true;
		}

		return false;
	}

	public void dataGridButton(String locator, boolean confirm) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.should(enabled);

			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse();
		}
	}

	public void dataGridSelect(String locator, int row) {
		SelenideElement element = waitAndLocate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}
			element.shouldBe(visible)
					.should(enabled);

			click(element);
		}
	}

	public void listGridButton(String locator, boolean confirm) {
		SelenideElement element = waitAndLocate(locator);
		if (element.exists()) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}
			element.shouldBe(visible)
					.should(enabled);

			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse();
		}
	}

	public void listGridSelect(String locator, int row) {
		SelenideElement element = waitAndLocate(String.format(locator, Integer.valueOf(row), Integer.valueOf(0)));
		if (element != null) {
			if (!element.is(visible)) {
				element.scrollIntoView(true);
			}

			element.shouldBe(visible)
					.should(enabled);

			click(element);
		}
	}

	public boolean verifySuccess() {
		SelenideElement element = waitAndLocate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/", true);

		if (element != null && element.exists() && element.isDisplayed()) {
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
		SelenideElement element = waitAndLocate("//:Dialog[ID=\"isc_globalWarn\"]/messageLabel/", true);

		if (element != null && element.exists() && element.isDisplayed()) {
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

	public void confirm() {
		SelenideElement element = waitAndLocate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"Yes\"]");
		click(element);
	}

	public void ok() {
		SelenideElement element = waitAndLocate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"OK\"]");
		click(element);
	}

	public void okIfPresent() {
		SelenideElement element = waitAndLocate("//:Dialog[ID=\"isc_globalWarn\"]//IButton[title=\"OK\"]", true);
		if (element != null) {
			click(element);
		}
	}

	private static long MAX_WAIT = 10000L;
	private static long WAIT = 50L;

	private static void waitUntilIdle() {
		boolean done = false;

		for (long l = 0; l <= MAX_WAIT; l += WAIT) {
			if (Boolean.TRUE.equals(Selenide.executeJavaScript("return isc && isc.AutoTest && isc.AutoTest.isSystemDone()"))) {
				done = true;
				break;
			}

			try {
				Thread.sleep(WAIT);
			} catch (InterruptedException e) {
				throw new IllegalStateException("Could not wait until system is responsive", e);
			}
		}

		if (!done) {
			throw new IllegalStateException("The system is not responsive after 10 seconds");
		}
	}
	
	public void waitForFullPageResponse() {
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(5));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
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
}

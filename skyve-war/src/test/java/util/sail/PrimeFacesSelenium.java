package util.sail;

import java.time.Duration;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.skyve.domain.messages.DomainException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class PrimeFacesSelenium extends CrossBrowserSelenium {
	private String baseUrl;
	
	@Override
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration, @Nonnull SailSettings resolved) {
		super.startBrowser(configuration, resolved);
		this.baseUrl = resolved.requireBaseUrl();
	}
	
	public void get(String url) {
		String viewState = getViewState();
		driver.get(baseUrl + url);
		waitForFullPageResponse(viewState);
	}

	/**
	 * Selects a tab by its link.
	 *
	 * @param id	the tab's client id
	 * @throws AssertionError	if the page has no such tab, or it is not displayed or is disabled
	 */
	public void tab(String id) {
		String xpath = String.format("//a[contains(@href, '#%s')]", id);
		click(usable(byXpath(xpath), "tab " + id));
		waitForAjaxResponse();
	}

	public void step(String id) {
		// a wizard step is selected by its link, like a tab
		tab(id);
	}

	public void login(String username, String password) {
		login(null, username, password);
	}
	
	/**
	 * Signs in on the sign in page.
	 *
	 * @param customer	the customer, or null if the page asks for none
	 * @param username	the user name
	 * @param password	the password
	 * @throws AssertionError	if the page has no field for a value given
	 */
	public void login(String customer, String username, String password) {
		driver.get(baseUrl);

		WebElement element = null;
		if (customer != null) {
			element = usable(byName("customer"), "the customer field");
			element.clear();
			element.sendKeys(customer);
		}

		element = usable(byName("user"), "the user field");
		element.click();
		element.clear();
		element.sendKeys(username);

		element = usable(byName("password"), "the password field");
		element.clear();
		element.sendKeys(password);

		driver.findElement(By.cssSelector("input[type=\"submit\"]")).click();
	}

	public void logout() {
		driver.get(baseUrl + "loggedOut");
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(10));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets a check box, clicking it until it shows the value.
	 *
	 * @param id	the check box's client id
	 * @param value	true, false, or null for a tri-state check box's unknown state
	 * @throws AssertionError	if the page has no such check box, or it is not displayed or is disabled
	 * @throws IllegalStateException	if two clicks do not set the value
	 */
	public void checkbox(String id, Boolean value) {
		WebElement element = usable(id);
		if (byId(String.format("%s_input", id)) == null) {
			Assertions.fail("#" + id + " is not a check box (it has no input) on " + driver.getCurrentUrl());
		}
		// check the value and only click if we need a different value
		String js = String.format("return window.SKYVE.PF.getCheckboxValue('%s')", id);
		Boolean checkboxValue = (Boolean) ((JavascriptExecutor) driver).executeScript(js);
		if (value == null) {
			for (int i = 0, l = 2; i < l; i++) { // try at most twice
				if (checkboxValue != null) {
					click(element);
					waitForAjaxResponse();
					checkboxValue = (Boolean) ((JavascriptExecutor) driver).executeScript(js);
				}
			}
			if (checkboxValue != null) {
				throw new IllegalStateException("Could not set checkbox to null or unknown value");
			}
		}
		else {
			for (int i = 0, l = 2; i < l; i++) { // try at most twice
				if ((checkboxValue == null) || (value.booleanValue() != checkboxValue.booleanValue())) {
					click(element);
					waitForAjaxResponse();
					checkboxValue = (Boolean) ((JavascriptExecutor) driver).executeScript(js);
				}
			}
			if ((checkboxValue == null) || (value.booleanValue() != checkboxValue.booleanValue())) {
				throw new IllegalStateException("Could not set checkbox to " + value);
			}
		}
	}

	public void _input(String id, String value) {
		text(String.format("%s_input", id), value);
	}

	/**
	 * Enters text in a field, replacing what it had.
	 *
	 * @param id	the field's client id
	 * @param value	the text, or null to clear the field
	 * @throws AssertionError	if the page has no such field, or it is not displayed, is disabled or is read only
	 */
	public void text(String id, String value) {
		boolean success = false;
		while (! success) {
			WebElement element = usable(id);
			if (element.getDomAttribute("readonly") != null) {
				Assertions.fail("#" + id + " is read only on " + driver.getCurrentUrl());
			}
			try {
				element.clear();
				element.sendKeys((value == null) ? "" : value);
				success = true;
			}
			catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
				// do nothing - we'll try again
			}
		}
		waitForAjaxResponse();
	}

	/**
	 * Clicks a radio button's option.
	 *
	 * @param id	the radio button's client id
	 * @param index	the option's zero-based position
	 * @throws AssertionError	if the page has no such radio button or option, or it is not displayed or is disabled
	 */
	public void radio(String id, int index) {
		usable(id);
		click(usable(byXpath("//label[@for='" + id + ":" + index + "']"), "option " + index + " of #" + id));
		waitForAjaxResponse();
	}

	/**
	 * Picks a select menu's option.
	 *
	 * @param id	the select menu's client id
	 * @param index	the option's zero-based position, counting an empty first option
	 * @throws AssertionError	if the page has no such select menu or option, or it is not displayed or is disabled
	 */
	public void selectOne(String id, int index) {
		usable(id);
		click(usable(byId(String.format("%s_label", id)), "the label of #" + id));

		// Wait for pick list drop down
		By panelXpath = By.id(String.format("%s_panel", id));
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
		wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(panelXpath),
											ExpectedConditions.visibilityOfElementLocated(panelXpath),
											ExpectedConditions.attributeToBe(panelXpath, "opacity", "1")));

		// Value here should be an index in the drop down starting from 0
		WebElement element = byId(String.format("%s_%s", id, String.valueOf(index)));
		if ((element == null) || (! element.isEnabled())) { // NB it may not be displayed
			Assertions.fail("#" + id + " has no option " + index + " to pick on " + driver.getCurrentUrl());
		}
		// Scroll the drop down panel so the item is visible
		((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView()", element);

		// Wait for pick list element to scroll into view
		wait = new WebDriverWait(driver, Duration.ofSeconds(1));
		wait.until(ExpectedConditions.visibilityOf(element));

		click(element);

		// Wait for pick list drop down to disappear (opacity is taken into account)
		wait = new WebDriverWait(driver, Duration.ofSeconds(1));
		wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(panelXpath),
											ExpectedConditions.invisibilityOfElementLocated(panelXpath)));

		waitForAjaxResponse();
	}

	/**
	 * Clicks a button, answers its confirmation, and waits for its response.
	 *
	 * @param id	the button's client id
	 * @param ajax	true to wait for an AJAX response, false for a new page
	 * @param confirm	true to accept a confirmation
	 * @throws AssertionError	if the page has no such button, or it is not displayed or is disabled
	 */
	public void button(String id, boolean ajax, boolean confirm) {
		WebElement element = usable(id);
		String viewState = getViewState();
		click(element);
		if (confirm) {
			confirm();
		}
		if (ajax) {
			waitForAjaxResponse();
		}
		else {
			waitForFullPageResponse(viewState);
		}
	}

	/**
	 * Clicks a button that goes to another page, answers its confirmation, and waits for the page.
	 *
	 * @param id	the button's client id
	 * @param confirm	true to accept a confirmation
	 * @throws AssertionError	if the page has no such button, or it is not displayed or is disabled
	 */
	public void redirectButton(String id, boolean confirm) {
		WebElement element = usable(id);
		String viewState = getViewState();
		click(element);
		if (confirm) {
			confirm();
		}
		waitForFullPageResponse(viewState);
	}

	/**
	 * Picks a row from a lookup description's drop down.
	 *
	 * @param id	the lookup description's client id
	 * @param row	the row's zero-based position
	 * @throws AssertionError	if the page has no such lookup, drop down button or row, or one is not displayed or is
	 * 							disabled
	 */
	public void lookupDescription(String id, int row) {
		usable(id);
		// Click the drop down button
		click(usable(byXpath(String.format("//span[@id='%s']/button", id)), "the drop down button of #" + id));

		// Wait for pick list drop down - can be a span or div depending on theme
		By xpath = By.xpath(String.format("//*[@id='%s_panel']", id));
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
		wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(xpath),
											ExpectedConditions.visibilityOfElementLocated(xpath)));

		// Select the row
		// NB could be <div><div><ul><li> or <span><ul><li>
		// in the middle means find the <ul> anywhere below
		WebElement element = usable(byXpath(String.format("//*[@id='%s_panel']//ul/li[%d]", id, Integer.valueOf(row + 1))),
									"row " + row + " of #" + id);
		// Scroll the drop down panel so the item is visible
		((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView()", element);
		click(element);
		waitForAjaxResponse();
	}

	/**
	 * Types a search into a lookup description and picks the first suggestion.
	 *
	 * @param id	the lookup description's client id
	 * @param search	the search
	 * @throws AssertionError	if the page has no such lookup, or it suggests nothing, or one is not displayed or is
	 * 							disabled
	 */
	public void lookupDescription(String id, String search) {
		usable(id);
		_input(id, search);

		// Wait for pick list drop down - can be a span or div depending on theme
		By xpath = By.xpath(String.format("//*[@id='%s_panel']", id));
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
		wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(xpath),
											ExpectedConditions.visibilityOfElementLocated(xpath)));

		// Select the first row
		// NB could be <div><div><ul><li> or <span><ul><li>
		// in the middle means find the <ul> anywhere below
		click(usable(byXpath(String.format("//*[@id='%s_panel']//ul/li", id)), "the first suggestion of #" + id));
		waitForAjaxResponse();
	}

	/**
	 * Clicks a data grid's button.
	 *
	 * @param dataGridId	the data grid's client id
	 * @param buttonId	the button's client id
	 * @param ajax	true to wait for an AJAX response, false for a new page
	 * @throws AssertionError	if the page has no such data grid or button, or one is not displayed or is disabled
	 */
	public void dataGridButton(String dataGridId, String buttonId, boolean ajax) {
		usable(dataGridId);
		gridButton(buttonId, ajax);
	}

	/**
	 * Clicks a data grid's row.
	 *
	 * @param dataGridId	the data grid's client id
	 * @param row	the row's zero-based position
	 * @throws AssertionError	if the page has no such data grid, or it is not displayed or is disabled
	 * @throws org.openqa.selenium.NoSuchElementException	if the grid has no such row
	 */
	public void dataGridSelect(String dataGridId, int row) {
		gridRow(dataGridId, row);
	}

	/**
	 * Clicks a list grid's button.
	 *
	 * @param listGridId	the list grid's client id
	 * @param buttonId	the button's client id
	 * @param ajax	true to wait for an AJAX response, false for a new page
	 * @throws AssertionError	if the page has no such list grid or button, or one is not displayed or is disabled
	 */
	public void listGridButton(String listGridId, String buttonId, boolean ajax) {
		usable(listGridId);
		gridButton(buttonId, ajax);
	}

	/**
	 * Clicks a list grid's row.
	 *
	 * @param listGridId	the list grid's client id
	 * @param row	the row's zero-based position
	 * @throws AssertionError	if the page has no such list grid, or it is not displayed or is disabled
	 * @throws org.openqa.selenium.NoSuchElementException	if the grid has no such row
	 */
	public void listGridSelect(String listGridId, int row) {
		gridRow(listGridId, row);
	}

	private void gridButton(String buttonId, boolean ajax) {
		WebElement element = usable(buttonId);
		String viewState = getViewState();
		click(element);
		if (ajax) {
			waitForAjaxResponse();
		}
		else {
			waitForFullPageResponse(viewState);
		}
	}

	private void gridRow(String gridId, int row) {
		WebElement element = usable(gridId).findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
		click(element);
		waitForAjaxResponse();
	}

	/**
	 * Returns the element with an id, for a gesture on it.
	 *
	 * @param id	the element's id
	 * @return	the element
	 * @throws AssertionError	if the page has no such element, or it is not displayed or is disabled
	 */
	private @Nonnull WebElement usable(@Nonnull String id) {
		return usable(byId(id), "#" + id);
	}

	/**
	 * Returns an element found for a gesture, failing the test if it was not found or cannot be used, so that a
	 * generated test never passes without doing what its script says.
	 *
	 * @param element	the element found, or null
	 * @param what	what the element is, for the failure message
	 * @return	the element
	 * @throws AssertionError	if the element is null, not displayed, or disabled (by its {@code disabled} attribute or
	 * 							PrimeFaces' {@code ui-state-disabled} style)
	 */
	private @Nonnull WebElement usable(@Nullable WebElement element, @Nonnull String what) {
		if (element == null) {
			return Assertions.fail(what + " is not on the page " + driver.getCurrentUrl());
		}
		if (! element.isDisplayed()) {
			Assertions.fail(what + " is not displayed on " + driver.getCurrentUrl());
		}
		String classes = element.getDomAttribute("class");
		if ((! element.isEnabled()) || (element.getDomAttribute("disabled") != null) ||
				((classes != null) && classes.contains("ui-state-disabled"))) {
			Assertions.fail(what + " is disabled on " + driver.getCurrentUrl());
		}
		return element;
	}

	/**
	 * Indicates whether the page shows no error or fatal message. The messages element is always on a view, and shows
	 * information messages such as "Saved" too, so only its error and fatal messages count.
	 *
	 * @return	false if the page shows an error or fatal message (printed to standard error)
	 */
	public boolean verifySuccess() {
		WebElement messages = present(By.id("messages"));
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getDomProperty("innerHTML");
			if (innerHTML.contains("ui-messages-error") || innerHTML.contains("ui-messages-fatal")) {
				System.err.println("**************");
				System.err.println("Not successful");
				System.err.println(innerHTML);
				System.err.println("**************");
				return false;
			}
		}

		return true;
	}

	public void assertSuccess() {
		Assertions.assertTrue(verifySuccess(), "Not successful");
	}

	public boolean verifyFailure(String messageToCheck) {
		WebElement messages = present(By.id("messages"));
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getDomProperty("innerHTML");
			if (innerHTML.contains("ui-messages-error") || innerHTML.contains("ui-messages-fatal")) {
				if ((messageToCheck == null) || innerHTML.contains(messageToCheck)) {
					return true;
				}

				System.err.println("**************");
				System.err.println("Not successful");
				System.err.println(innerHTML);
				System.err.println("**************");
			}
		}

		System.err.println("**************");
		System.err.println("Not successful - no error or fatal messages.");
		System.err.println("**************");
		return false;
	}

	public boolean verifyFailure() {
		return verifyFailure(null);
	}

	public void assertFailure(String messageToCheck) {
		Assertions.assertTrue(verifyFailure(messageToCheck), "Successful");
	}

	public void assertFailure() {
		Assertions.assertTrue(verifyFailure(), "Successful");
	}

	public void confirm() {
		// Wait for confirm dialog to appear
		By id = By.id("confirmOK");
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(2));
		try {
			wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(id),
												ExpectedConditions.visibilityOfElementLocated(id)));
		}
		catch (RuntimeException e) {
			System.out.println("Confirm dialog was never shown");
			throw e;
		}
		click(byId("confirmOK"));
	}

	public void waitForAjaxResponse() {
		// Wait until wheelOfDeath is invisible after AJAX
		WebElement element = present(By.id("wheelOfDeath_start"));
		if (element != null) {
			WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
			wait.until(ExpectedConditions.invisibilityOf(element));
		}
	}

	public void waitForFullPageResponse(String oldViewState) {
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
		try {
			wait.ignoring(StaleElementReferenceException.class)
					.until(d -> ((getViewState() == null) || getViewState().equals(oldViewState)) ? Boolean.FALSE : Boolean.TRUE);
		}
		catch (RuntimeException e) {
			System.err.println("Timed out waiting for a navigation from " + driver.getCurrentUrl() + " : oldViewState = " + oldViewState);
			throw e;
		}
		wait = new WebDriverWait(driver, Duration.ofSeconds(5));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Runs JavaScript in the page, as a SAIL {@code <execute>} step does, and waits for any AJAX request it starts.
	 *
	 * @param script	the JavaScript
	 */
	public void execute(String script) {
		((JavascriptExecutor) driver).executeScript(script);
		waitForAjaxResponse();
	}

	@SuppressWarnings("static-method")
	public void trace(String comment) {
		System.out.println(comment);
	}
	
	@SuppressWarnings("static-method")
	public void pause(long millis) {
		try {
			Thread.sleep(millis);
		}
		catch (InterruptedException e) {
			throw new DomainException("Couldn't pause", e);
		}
	}

	public String getViewState() {
		String result = null;

		WebElement element = present(By.name("jakarta.faces.ViewState"));
		if (element != null) {
			result = element.getDomProperty("value");
		}

		return result;
	}

	public void click(WebElement element) {
		try {
			element.click();
		}
		// This could occur when the control is behind a floating element
		catch (@SuppressWarnings("unused") WebDriverException e) {
			JavascriptExecutor js = (JavascriptExecutor) driver;
			// Scroll the element into view on the page and see if the element can be made visible
			try {
				trace("    Could not click on the element - scroll to the element and try again");
				js.executeScript("arguments[0].scrollIntoView(true);", element);
				element.click();
			}
			// Scroll to the top of the page and see if the element can be made visible
			catch (@SuppressWarnings("unused") WebDriverException e1) {
				try {
					trace("    Could not click on the element - scroll to the top of the page and try again");
					js.executeScript("javascript:window.scrollTo(0, 0)");
					element.click();
				}
				// Scroll to the bottom of the page and try again in case the floating element is at the bottom
				catch (@SuppressWarnings("unused") WebDriverException e2) {
					trace("    Could not click on the element - scroll to the bottom of the page and try again");
					js.executeScript("javascript:window.scrollTo(0, 999999)");
					element.click();
				}
				
			}
		}
	}

	public List<WebElement> byClass(String className) {
		try {
			return driver.findElements(By.className(className));
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	public List<WebElement> byCss(String selector) {
		try {
			return driver.findElements(By.cssSelector(selector));
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	/**
	 * Returns the element with an id, waiting for it as {@link #by(By)} does.
	 *
	 * @param id	the id
	 * @return	the element, or null if it is not on the page by the timeout
	 */
	public WebElement byId(String id) {
		return by(By.id(id));
	}

	/**
	 * Returns the first element an XPath finds, waiting for it as {@link #by(By)} does.
	 *
	 * @param xpath	the XPath
	 * @return	the element, or null if it is not on the page by the timeout
	 */
	public WebElement byXpath(String xpath) {
		return by(By.xpath(xpath));
	}

	/**
	 * Returns the first element with a name, waiting for it as {@link #by(By)} does.
	 *
	 * @param name	the name
	 * @return	the element, or null if it is not on the page by the timeout
	 */
	public WebElement byName(String name) {
		return by(By.name(name));
	}

	/*
	By.linkText(linkText)
	By.partialLinkText(linkText)
	By.tagName(name)
	 */

	/**
	 * Returns an element that a gesture needs, waiting until it is on the page (and not stale) for up to the settings'
	 * timeout ({@value SailSettings#TIMEOUT_SECONDS}): the page may still be rendering after a request.
	 *
	 * @param locator	how to find the element
	 * @return	the element, or null if it is not on the page by the timeout
	 */
	private @Nullable WebElement by(@Nonnull By locator) {
		SailSettings resolved = settings;
		Duration timeout = (resolved == null) ? Duration.ofSeconds(SailSettings.DEFAULT_TIMEOUT_SECONDS) : resolved.timeout();
		try {
			// WebDriverWait ignores NotFoundException; a stale element is looked for again
			return new WebDriverWait(driver, timeout).ignoring(StaleElementReferenceException.class).until(d -> {
				WebElement result = d.findElement(locator);
				result.isDisplayed(); // check for stale element
				result.isEnabled(); // check for stale element
				return result;
			});
		}
		catch (@SuppressWarnings("unused") TimeoutException e) {
			return null;
		}
	}

	/**
	 * Returns an element that a page may or may not have, such as its messages, without waiting.
	 *
	 * @param locator	how to find the element
	 * @return	the first element found, or null if there is none
	 */
	private @Nullable WebElement present(@Nonnull By locator) {
		List<WebElement> found = driver.findElements(locator);
		return found.isEmpty() ? null : found.get(0);
	}
}

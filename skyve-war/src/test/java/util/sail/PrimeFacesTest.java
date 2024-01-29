package util.sail;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

public abstract class PrimeFacesTest extends CrossBrowserTest {
	protected void get(String url) {
		String viewState = getViewState();
		driver.get(baseUrl + url);
		waitForFullPageResponse(viewState);
	}

	protected void tab(String id) {
		String xpath = String.format("//a[contains(@href, '#%s')]", id);
		WebElement element = byXpath(xpath);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			click(element);
			waitForAjaxResponse();
		}
	}

	protected void step(String id) {
		String xpath = String.format("//a[contains(@href, '#%s')]", id);
		WebElement element = byXpath(xpath);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			click(element);
			waitForAjaxResponse();
		}
	}

	protected void login(String customer, String username, String password) throws Exception {
		driver.get(baseUrl);

		WebElement element = null;
		if (customer != null) {
			element = driver.findElement(By.name("customer"));
			element.clear();
			element.sendKeys(customer);
		}

		element = driver.findElement(By.name("user"));
		element.clear();
		element.sendKeys(username);

		element = driver.findElement(By.name("password"));
		element.clear();
		element.sendKeys(password);

		driver.findElement(By.cssSelector("input[type=\"submit\"]")).click();
	}

	protected void logout() {
		driver.get(baseUrl + "loggedOut");
		WebDriverWait wait = new WebDriverWait(driver, 10);
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	protected void checkbox(String id, Boolean value) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			WebElement inputElement = byId(String.format("%s_input", id));
			if ((inputElement != null) && element.isDisplayed() && element.isEnabled()) {
				// don't need to check if checkbox is disabled coz we can still try to click it
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
		}
	}

	protected void _input(String id, String value) {
		text(String.format("%s_input", id), value);
	}

	protected void text(String id, String value) {
		boolean success = false;
		while (! success) {
			WebElement element = byId(id);
			if ((element != null) && 
					element.isDisplayed() && 
					element.isEnabled() &&
					(element.getAttribute("disabled") == null) && 
					(element.getAttribute("readonly") == null)) {
				try {
					element.clear();
					element.sendKeys((value == null) ? "" : value);
					success = true;
				}
				catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
					// do nothing - we'll try again
				}
			}
			else {
				break;
			}
		}
		if (success) {
			waitForAjaxResponse();
		}
	}

	protected void radio(String id, int index) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getAttribute("class").contains("ui-state-disabled")) {
				element = byXpath("//label[@for='" + id + ":" + index + "']");
				if ((element != null) && element.isDisplayed() && element.isEnabled()) {
					click(element);
					waitForAjaxResponse();
				}
			}
		}
	}

	protected void selectOne(String id, int index) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getAttribute("class").contains("ui-state-disabled")) {
				element = byId(String.format("%s_label", id));
				if ((element != null) && element.isEnabled() && element.isDisplayed()) {
					click(element);

					// Wait for pick list drop down
					By panelXpath = By.id(String.format("%s_panel", id));
					WebDriverWait wait = new WebDriverWait(driver, 1);
					wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(panelXpath),
														ExpectedConditions.visibilityOfElementLocated(panelXpath),
														ExpectedConditions.attributeToBe(panelXpath, "opacity", "1")));

					// Value here should be an index in the drop down starting from 0
					element = byId(String.format("%s_%s", id, String.valueOf(index)));
					if ((element != null) && element.isEnabled()) { // NB it may not be displayed
						// Scroll the drop down panel so the item is visible
						((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView()", element);

						// Wait for pick list element to scroll into view
						wait = new WebDriverWait(driver, 1);
						wait.until(ExpectedConditions.visibilityOf(element));

						click(element);

						// Wait for pick list drop down to disappear (opacity is taken into account)
						wait = new WebDriverWait(driver, 1);
						wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(panelXpath),
															ExpectedConditions.invisibilityOfElementLocated(panelXpath)));

						waitForAjaxResponse();
					}
				}
			}
		}
	}

	protected void button(String id, boolean ajax, boolean confirm) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getAttribute("class").contains("ui-state-disabled")) {
				String viewState = getViewState();
				if (Browsers.chrome.equals(browser)) {
					Actions clicker = new Actions(driver);
					clicker.moveToElement(element).moveByOffset(10, 10).click().build().perform();
				}
				else {
					click(element);
				}
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
		}
	}

	protected void redirectButton(String id, boolean confirm) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getAttribute("class").contains("ui-state-disabled")) {
				String viewState = getViewState();
				if (Browsers.chrome.equals(browser)) {
					Actions clicker = new Actions(driver);
					clicker.moveToElement(element).moveByOffset(10, 10).click().build().perform();
				}
				else {
					click(element);
				}
				if (confirm) {
					confirm();
				}
				try {
					Thread.sleep(250);
				}
				catch (@SuppressWarnings("unused") InterruptedException e) {
					// nothing to do here
				}
				waitForFullPageResponse(viewState);
			}
		}
	}

	protected void lookupDescription(String id, int row) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the drop down button
			element = byXpath(String.format("//span[@id='%s']/button", id));
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Click the drop down button
//				Actions clicker = new Actions(driver);
//				clicker.moveToElement(element).moveByOffset(10, 10).click().build().perform();
				click(element);

				// Wait for pick list drop down - can be a span or div depending on theme
				By xpath = By.xpath(String.format("//*[@id='%s_panel']", id));
				WebDriverWait wait = new WebDriverWait(driver, 30);
				wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(xpath),
													ExpectedConditions.visibilityOfElementLocated(xpath)));

				// Select the row
				// NB could be <div><div><ul><li> or <span><ul><li>
				// in the middle means find the <ul> anywhere below
				element = byXpath(String.format("//*[@id='%s_panel']//ul/li[%d]", id, Integer.valueOf(row + 1)));
				if ((element != null) && element.isDisplayed() && element.isEnabled()) {
					// Scroll the drop down panel so the item is visible
					((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView()", element);
					click(element);
					waitForAjaxResponse();
				}
			}
		}
	}

	protected void lookupDescription(String id, String search) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			_input(id, search);

			// Wait for pick list drop down - can be a span or div depending on theme
			By xpath = By.xpath(String.format("//*[@id='%s_panel']", id));
			WebDriverWait wait = new WebDriverWait(driver, 30);
			wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(xpath),
												ExpectedConditions.visibilityOfElementLocated(xpath)));

			// Select the first row
			// NB could be <div><div><ul><li> or <span><ul><li>
			// in the middle means find the <ul> anywhere below
			element = byXpath(String.format("//*[@id='%s_panel']//ul/li", id));
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				click(element);
				waitForAjaxResponse();
			}
		}
	}

	protected void dataGridButton(String dataGridId, String buttonId, boolean ajax) {
		// check data grid is present
		WebElement element = byId(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = byId(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on data grid button
				if (! element.getAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click(element);
					if (ajax) {
						waitForAjaxResponse();
					}
					else {
						waitForFullPageResponse(viewState);
					}
				}
			}
		}
	}

	protected void dataGridSelect(String dataGridId, int row) {
		// check list grid is present
		WebElement element = byId(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click(element);
			waitForAjaxResponse();
		}
	}

	protected void listGridButton(String listGridId, String buttonId, boolean ajax) {
		// check list grid is present
		WebElement element = byId(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = byId(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on list grid button
				if (! element.getAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click(element);
					if (ajax) {
						waitForAjaxResponse();
					}
					else {
						waitForFullPageResponse(viewState);
					}
				}
			}
		}
	}

	protected void listGridSelect(String listGridId, int row) {
		// check list grid is present
		WebElement element = byId(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click(element);
			waitForAjaxResponse();
		}
	}

	protected boolean verifySuccess() {
		WebElement messages = byId("messages");
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getAttribute("innerHTML");
			if (innerHTML.contains("ui-messages-error") || innerHTML.contains("ui-messages-fatal")) {
				System.err.println("**************");
				System.err.println("Not successful");
				System.err.println(innerHTML);
				System.err.println("**************");
			}
			return false;
		}

		return true;
	}

	protected void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	protected boolean verifyFailure(String messageToCheck) {
		WebElement messages = byId("messages");
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getAttribute("innerHTML");
			if (innerHTML.contains("ui-messages-error") || innerHTML.contains("ui-messages-fatal")) {
				if (messageToCheck == null) {
					return true;
				}
				else if (innerHTML.contains(messageToCheck)) {
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

	protected boolean verifyFailure() {
		return verifyFailure(null);
	}

	protected void assertFailure(String messageToCheck) {
		Assert.assertTrue("Successful", verifyFailure(messageToCheck));
	}

	protected void assertFailure() {
		Assert.assertTrue("Successful", verifyFailure());
	}

	protected void confirm() {
		// Wait for confirm dialog to appear
		By id = By.id("confirmOK");
		WebDriverWait wait = new WebDriverWait(driver, 2);
		try {
			wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(id),
												ExpectedConditions.visibilityOfElementLocated(id)));
		}
		catch (RuntimeException e) {
			System.out.println("Confirm dialog was never showed");
			throw e;
		}
		click(byId("confirmOK"));
	}

	protected void waitForAjaxResponse() {
		// Wait until busy is invisible after AJAX
		WebElement element = byId("busy");
		if (element != null) {
			WebDriverWait wait = new WebDriverWait(driver, 30);
			wait.until(ExpectedConditions.invisibilityOf(element));
		}
	}

	protected void waitForFullPageResponse(String oldViewState) {
		WebDriverWait wait = new WebDriverWait(driver, 30);
		try {
			wait.ignoring(StaleElementReferenceException.class)
					.until(d -> ((getViewState() == null) || getViewState().equals(oldViewState)) ? Boolean.FALSE : Boolean.TRUE);
		}
		catch (RuntimeException e) {
			System.err.println("Timed out waiting for a navigation from " + driver.getCurrentUrl() + " : oldViewState = " + oldViewState);
			throw e;
		}
		wait = new WebDriverWait(driver, 5);
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	@SuppressWarnings("static-method")
	protected void trace(String comment) {
		System.out.println(comment);
	}

	protected String getViewState() {
		String result = null;

		WebElement element = byName("jakarta.faces.ViewState");
		if (element != null) {
			result = element.getAttribute("value");
		}

		return result;
	}

	protected void click(WebElement element) {
		try {
			element.click();
		}
		// This could occur when the control is behind a floating element
		// So scroll to the top of the page and see if the element can be made visible
		catch (@SuppressWarnings("unused") WebDriverException e) {
			JavascriptExecutor js = (JavascriptExecutor) driver;
			try {
				trace("    Could not click on the element - scroll to the top of the page and try again");
				js.executeScript("javascript:window.scrollTo(0, 0)");
				element.click();
			}
			// Scroll to the bottom of the page and try again in case the floating element is at the bottom
			catch (@SuppressWarnings("unused") WebDriverException e1) {
				trace("    Could not click on the element - scroll to the bottom of the page and try again");
				js.executeScript("javascript:window.scrollTo(0, 999999)");
				element.click();
			}
		}
	}

	protected List<WebElement> byClass(String className) {
		try {
			return driver.findElements(By.className(className));
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	protected List<WebElement> byCss(String selector) {
		try {
			return driver.findElements(By.cssSelector(selector));
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	protected WebElement byId(String id) {
		try {
			WebElement result = driver.findElement(By.id(id));
			result.isDisplayed(); // check for stale element
			result.isEnabled(); // check for stale element
			return result;
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
		catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
			try {
				Thread.sleep(50);
			}
			catch (@SuppressWarnings("unused") InterruptedException ie) {
				// do nothing here
			}
			return byId(id);
		}
	}

	protected WebElement byXpath(String xpath) {
		try {
			WebElement result = driver.findElement(By.xpath(xpath));
			result.isDisplayed(); // check for stale element
			result.isEnabled(); // check for stale element
			return result;
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
		catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
			try {
				Thread.sleep(10);
			}
			catch (@SuppressWarnings("unused") InterruptedException ie) {
				// do nothing here
			}
			return byXpath(xpath);
		}
	}

	protected WebElement byName(String name) {
		try {
			WebElement result = driver.findElement(By.name(name));
			result.isDisplayed(); // check for stale element
			result.isEnabled(); // check for stale element
			return result;
		}
		catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
		catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
			try {
				Thread.sleep(10);
			}
			catch (@SuppressWarnings("unused") InterruptedException ie) {
				// do nothing here
			}
			return byName(name);
		}
	}

	/*
	By.linkText(linkText)
	By.partialLinkText(linkText)
	By.tagName(name)
	 */
}

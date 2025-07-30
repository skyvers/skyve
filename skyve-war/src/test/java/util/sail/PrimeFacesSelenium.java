package util.sail;

import java.time.Duration;
import java.util.List;
import java.util.function.Function;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.StaleElementReferenceException;
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
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		super.startBrowser(configuration);
		this.baseUrl = configuration.getBaseUrl();
	}
	
	public void get(String url) {
		String viewState = getViewState();
		driver.get(baseUrl + url);
		waitForFullPageResponse(viewState);
	}

	public void tab(String id) {
		String xpath = String.format("//a[contains(@href, '#%s')]", id);
		WebElement element = byXpath(xpath);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			click(element);
			waitForAjaxResponse();
		}
	}

	public void step(String id) {
		String xpath = String.format("//a[contains(@href, '#%s')]", id);
		WebElement element = byXpath(xpath);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			click(element);
			waitForAjaxResponse();
		}
	}

	public void login(String username, String password) {
		login(null, username, password);
	}
	
	public void login(String customer, String username, String password) {
		driver.get(baseUrl);

		WebElement element = null;
		if (customer != null) {
			element = byName("customer");
			element.clear();
			element.sendKeys(customer);
		}

		element = byName("user");
		element.click();
		element.clear();
		element.sendKeys(username);

		element = byName("password");
		element.clear();
		element.sendKeys(password);

		driver.findElement(By.cssSelector("input[type=\"submit\"]")).click();
	}

	public void logout() {
		driver.get(baseUrl + "loggedOut");
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(10));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	public void checkbox(String id, Boolean value) {
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

	public void _input(String id, String value) {
		text(String.format("%s_input", id), value);
	}

	public void text(String id, String value) {
		boolean success = false;
		while (! success) {
			WebElement element = byId(id);
			if ((element != null) && 
					element.isDisplayed() && 
					element.isEnabled() &&
					(element.getDomAttribute("disabled") == null) && 
					(element.getDomAttribute("readonly") == null)) {
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

	public void radio(String id, int index) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
				element = byXpath("//label[@for='" + id + ":" + index + "']");
				if ((element != null) && element.isDisplayed() && element.isEnabled()) {
					click(element);
					waitForAjaxResponse();
				}
			}
		}
	}

	public void selectOne(String id, int index) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
				element = byId(String.format("%s_label", id));
				if ((element != null) && element.isEnabled() && element.isDisplayed()) {
					click(element);

					// Wait for pick list drop down
					By panelXpath = By.id(String.format("%s_panel", id));
					WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
					wait.until(ExpectedConditions.and(ExpectedConditions.presenceOfElementLocated(panelXpath),
														ExpectedConditions.visibilityOfElementLocated(panelXpath),
														ExpectedConditions.attributeToBe(panelXpath, "opacity", "1")));

					// Value here should be an index in the drop down starting from 0
					element = byId(String.format("%s_%s", id, String.valueOf(index)));
					if ((element != null) && element.isEnabled()) { // NB it may not be displayed
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
				}
			}
		}
	}

	public void button(String id, boolean ajax, boolean confirm) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			String classes = element.getDomAttribute("class");
			if ((classes == null) || (! classes.contains("ui-state-disabled"))) {
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
		}
	}

	public void redirectButton(String id, boolean confirm) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Look for prime faces disabled style
			if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
				String viewState = getViewState();
				click(element);
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

	public void lookupDescription(String id, int row) {
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
				WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
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

	public void lookupDescription(String id, String search) {
		WebElement element = byId(id);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			_input(id, search);

			// Wait for pick list drop down - can be a span or div depending on theme
			By xpath = By.xpath(String.format("//*[@id='%s_panel']", id));
			WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
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

	public void dataGridButton(String dataGridId, String buttonId, boolean ajax) {
		// check data grid is present
		WebElement element = byId(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = byId(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on data grid button
				if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
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

	public void dataGridSelect(String dataGridId, int row) {
		// check list grid is present
		WebElement element = byId(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click(element);
			waitForAjaxResponse();
		}
	}

	public void listGridButton(String listGridId, String buttonId, boolean ajax) {
		// check list grid is present
		WebElement element = byId(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = byId(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on list grid button
				if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
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

	public void listGridSelect(String listGridId, int row) {
		// check list grid is present
		WebElement element = byId(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click(element);
			waitForAjaxResponse();
		}
	}

	public boolean verifySuccess() {
		WebElement messages = byId("messages");
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getDomProperty("innerHTML");
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

	public void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	public boolean verifyFailure(String messageToCheck) {
		WebElement messages = byId("messages");
		if ((messages != null) && messages.isDisplayed()) {
			String innerHTML = messages.getDomProperty("innerHTML");
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
		WebElement element = byId("wheelOfDeath_start");
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

		WebElement element = byName("jakarta.faces.ViewState");
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

	public WebElement byId(String id) {
		return by(i -> driver.findElement(By.id(i)),id);
	}

	public WebElement byXpath(String xpath) {
		return by(x -> driver.findElement(By.xpath(x)), xpath);
	}

	public WebElement byName(String name) {
		return by(n -> driver.findElement(By.name(n)), name);
	}

	/*
	By.linkText(linkText)
	By.partialLinkText(linkText)
	By.tagName(name)
	 */

	private static long MAX_WAIT = 1000L;
	private static long WAIT = 50L;

	private static @Nullable WebElement by(@Nonnull Function<String, WebElement> function, @Nonnull String search) {
		for (long l = 0; l <= MAX_WAIT; l += WAIT) {
			try {
				try {
					WebElement result = function.apply(search);
					result.isDisplayed(); // check for stale element
					result.isEnabled(); // check for stale element
					return result;
				}
				catch (NoSuchElementException | StaleElementReferenceException e) {
					if (l > MAX_WAIT) {
						throw e;
					}
					Thread.sleep(WAIT);
				}
			}
			catch (@SuppressWarnings("unused") InterruptedException e) {
				// do nothing here
			}
		}
		return null;
	}
}

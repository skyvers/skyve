package util.sail;

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

import util.sail.BrowserConfiguration.Browsers;

public class SmartClientSelenide extends CrossBrowserSelenium {
	private String baseUrl;
	
	@Override
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		super.startBrowser(configuration);
		WebDriverRunner.setWebDriver(driver);
		this.baseUrl = configuration.getBaseUrl();
	}
	
	public void get(String url) {
		open(baseUrl + url);
	}

public static void main(String[] arg) {
	SmartClientSelenide scs = new SmartClientSelenide();
	scs.startBrowser(new BrowserConfiguration()
						.browser(Browsers.chrome)
						.baseUrl("http://localhost:8080/skyve/"));
	scs.login("admin", "admin");
while (true) {
	SelenideElement poo = scs.locate("//VLayout[ID=\"details\"]/member[0]/member[2]/member[0]/member[0]");
//	SelenideElement poo = scs.locate("//VLayout[ID=\"details\"]/member[0]/member[2]/member[2]/body/row[2]");
System.out.println(poo.attr("id"));
	scs.doubleClick(poo);
	waitUntilIdle();
	poo = scs.locate("//:IButton[title=\"  Cancel\"]");
System.out.println(poo.attr("id"));
	scs.click(poo);
	waitUntilIdle();
}
//	scs.stopBrowser();
}

	public SelenideElement locate(String locator) {
System.out.println(locator);
		try {
			WebElement element = Selenide.executeJavaScript("return isc.AutoTest.getElement(arguments[0])", locator);
			if (element == null) {
				return pauseAndLocate(locator);
			}
			SelenideElement result = $(element);
			result.getWrappedElement().isEnabled(); // trigger a StaleElementReferenceException immediately
			return result;
		}
		catch (StaleElementReferenceException se) {
			return pauseAndLocate(locator);
		}
	}

	private SelenideElement pauseAndLocate(String locator) {
		try {
			Thread.sleep(WAIT);
		}
		catch (InterruptedException e) {
			throw new IllegalStateException("Could not wait to locate the element " + locator, e);
		}
		return locate(locator);
	}
	
	public boolean tab(String locator) {
/*
		WebElement tab = 
		SelenideElement element = $(byXpath(String.format("//a[contains(@href, '#%s')]", id)));
		if (element.exists()) {
			click(element);
			waitForAjaxResponse();
			return true;
		}
*/
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

	public boolean checkbox(String id, Boolean value) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists()) {
			element.shouldBe(visible).shouldBe(enabled);
			SelenideElement inputElement = $(byId(String.format("%s_input", id)));
			inputElement.should(exist);
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
			return true;
		}
*/
		return false;
	}

	public boolean _input(String id, String value, boolean keyPresses) {
		return text(String.format("%s_input", id), value, keyPresses);
	}

	public boolean text(String id, String value, boolean keyPresses) {
		boolean success = false;
/*
		SelenideElement element = $(byId(id));
		if (element.exists() && 
				element.isDisplayed() && 
				element.isEnabled() &&
				(element.attr("disabled") == null) && 
				(element.attr("readonly") == null)) {
			element.clear();
			if (keyPresses) {
				if (value != null) {
					element.sendKeys(value);
				}
			}
			else {
				element.val((value == null) ? "" : value);
			}
			success = true;
		}
		if (success) {
			waitForAjaxResponse();
		}
*/
		return success;
	}

	public boolean radio(String id, int index) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists() && (! disabledClass(element))) {
			element = $(byXpath("//label[@for='" + id + ":" + index + "']"));
			element.should(exist);
			click(element);
			waitForAjaxResponse();
			return true;
		}
*/
		return false;
	}

	public boolean selectOne(String id, int index) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists() && (! disabledClass(element))) {
			element.shouldBe(visible).shouldBe(enabled);
			element = $(byId(String.format("%s_label", id)));
			element.should(exist);
			click(element);

			// Wait for pick list drop down
			element = $(byId(String.format("%s_panel", id)));
			element.should(appear);

			// Value here should be an index in the drop down starting from 0
			element = $(byId(String.format("%s_%s", id, String.valueOf(index))));
			element.should(exist);
			click(element);
			waitForAjaxResponse();
			return true;
		}
*/
		return false;
	}

	public boolean button(String id, boolean ajax, boolean confirm) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists() && (! disabledClass(element))) {
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
			return true;
		}
*/
		return false;
	}

	public boolean redirectButton(String id, boolean confirm) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists() && (! disabledClass(element))) {
			String viewState = getViewState();
			click(element);
			if (confirm) {
				confirm();
			}
			waitForFullPageResponse(viewState);
			return true;
		}
*/
		return false;
	}

	public boolean lookupDescription(String id, int row) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists()) {
			// Find the drop down button
			element = $(byXpath(String.format("//span[@id='%s']/button", id)));
			element.shouldBe(visible).shouldBe(enabled);
			click(element);

			// Wait for pick list drop down - can be a span or div depending on theme
			element = $(byXpath(String.format("//*[@id='%s_panel']", id)));
			element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);

			// Select the row
			// NB could be <div><div><ul><li> or <span><ul><li>
			// in the middle means find the <ul> anywhere below
			element = $(byXpath(String.format("//*[@id='%s_panel']//ul/li[%d]", id, Integer.valueOf(row))));
			element.should(exist);
			click(element);
			waitForAjaxResponse();
			return true;
		}
*/
		return false;
	}

	public boolean lookupDescription(String id, String search) {
/*
		SelenideElement element = $(byId(id));
		if (element.exists()) {
			_input(id, search, true);

			// Wait for pick list drop down - can be a span or div depending on theme
			element = $(byXpath(String.format("//*[@id='%s_panel']", id)));
			element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);

			// Select the first row
			// NB could be <div><div><ul><li> or <span><ul><li>
			// in the middle means find the <ul> anywhere below
			element = $(byXpath(String.format("//*[@id='%s_panel']//ul/li", id)));
			element.should(exist);
			click(element);
			waitForAjaxResponse();
			return true;
		}
*/
		return false;
	}

	public void dataGridButton(String dataGridId, String buttonId, boolean ajax) {
/*
		// check data grid is present
		WebElement element = oldById(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = oldById(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on data grid button
				if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click($(element));
					if (ajax) {
						waitForAjaxResponse();
					}
					else {
						waitForFullPageResponse(viewState);
					}
				}
			}
		}
*/
	}

	public void dataGridSelect(String dataGridId, int row) {
/*
		// check list grid is present
		WebElement element = oldById(dataGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click($(element));
			waitForAjaxResponse();
		}
*/
	}

	public void listGridButton(String listGridId, String buttonId, boolean ajax) {
/*
		// check list grid is present
		WebElement element = oldById(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = oldById(buttonId);
			if ((element != null) && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on list grid button
				if (! element.getDomAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click($(element));
					if (ajax) {
						waitForAjaxResponse();
					}
					else {
						waitForFullPageResponse(viewState);
					}
				}
			}
		}
*/
	}

	public void listGridSelect(String listGridId, int row) {
/*
		// check list grid is present
		WebElement element = oldById(listGridId);
		if ((element != null) && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click($(element));
			waitForAjaxResponse();
		}
*/
	}

	public boolean verifySuccess() {
/*
		WebElement messages = oldById("messages");
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
*/
		return true;
	}

	public void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	public boolean verifyFailure(String messageToCheck) {
/*
		WebElement messages = oldById("messages");
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
*/
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
		$(byId("confirmOK")).should(exist).shouldBe(visible);
		click($(byId("confirmOK")));
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
			}
			catch (InterruptedException e) {
				throw new IllegalStateException("Could not wait until system is responsive", e);
			}
		}
		if (! done) {
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
		}
		catch (InterruptedException e) {
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
System.out.println("Click " + element);
//		WebElement wrappedElement = element.getWrappedElement();
		try {
			try {
				// Scroll the element into view on the page and see if the element can be made visible
				if (! element.is(visible, Duration.ofMillis(250))) {
					element.scrollIntoView(true);
				}
				
				clickIt(element, doubleClick);
			}
			// This could occur when the control is behind a floating element
			catch (@SuppressWarnings("unused") WebDriverException e) {
				JavascriptExecutor js = (JavascriptExecutor) driver;
				try {
					// Scroll to the top of the page and see if the element can be made visible
					trace("    Could not click on the element - scroll to the top of the page and try again");
					js.executeScript("javascript:window.scrollTo(0, 0)");
					clickIt(element, doubleClick);
				}
				catch (@SuppressWarnings("unused") WebDriverException e1) {
					try {
						// Scroll to the bottom of the page and try again in case the floating element is at the bottom
						trace("    Could not click on the element - scroll to the bottom of the page and try again");
						js.executeScript("javascript:window.scrollTo(0, 999999)");
						clickIt(element, doubleClick);
					}
					catch (@SuppressWarnings("unused") WebDriverException e2) {
						// Scroll the element into view on the page using javascript and see if the element can be made visible
						trace("    Could not click on the element - scroll to the element and try again");
						js.executeScript("arguments[0].scrollIntoView(true);", element);
						clickIt(element, doubleClick);
					}
				}
			}
		}
		catch (StaleElementReferenceException ser) {
			String id = element.attr("id");
			if (id != null) {
				click($(byId(id)), doubleClick);
			}
		}
	}
	
	private static void clickIt(SelenideElement element, boolean doubleClick) {
		if (doubleClick) {
			element.doubleClick();
		}
		else {
			element.click();
		}
	}
}

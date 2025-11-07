package util.sail;

import static com.codeborne.selenide.Condition.appear;
import static com.codeborne.selenide.Condition.enabled;
import static com.codeborne.selenide.Condition.exist;
import static com.codeborne.selenide.Condition.hidden;
import static com.codeborne.selenide.Condition.visible;
import static com.codeborne.selenide.Selectors.byId;
import static com.codeborne.selenide.Selectors.byName;
import static com.codeborne.selenide.Selectors.byXpath;
import static com.codeborne.selenide.Selenide.$;

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
import org.openqa.selenium.support.ui.WebDriverWait;

import com.codeborne.selenide.SelenideElement;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.commands.impl.PrimeFacesButtonCommand;
import util.sail.commands.impl.PrimeFacesCheckboxCommand;
import util.sail.commands.impl.PrimeFacesDataGridButtonCommand;
import util.sail.commands.impl.PrimeFacesDataGridSelectCommand;
import util.sail.commands.impl.PrimeFacesListGridButtonCommand;
import util.sail.commands.impl.PrimeFacesListGridSelectCommand;
import util.sail.commands.impl.PrimeFacesLookupDescriptionByRowCommand;
import util.sail.commands.impl.PrimeFacesLookupDescriptionBySearchCommand;
import util.sail.commands.impl.PrimeFacesRadioCommand;
import util.sail.commands.impl.PrimeFacesSelectOneCommand;
import util.sail.commands.impl.PrimeFacesTabCommand;
import util.sail.commands.impl.PrimeFacesTextCommand;

/**
 * Selenide implementation for the PrimeFaces framework,
 * binding PrimeFaces-specific command types to provide typed, framework-specific UI automation capabilities.
 * 
 * @author mike
 */
public class PrimeFacesSelenide extends Selenide<
	PrimeFacesButtonCommand,
	PrimeFacesCheckboxCommand,
	PrimeFacesDataGridButtonCommand,
	PrimeFacesDataGridSelectCommand,
	PrimeFacesListGridButtonCommand,
	PrimeFacesListGridSelectCommand,
	PrimeFacesLookupDescriptionByRowCommand,
	PrimeFacesLookupDescriptionBySearchCommand,
	PrimeFacesRadioCommand,
	PrimeFacesTabCommand,
	PrimeFacesTextCommand,
	PrimeFacesSelectOneCommand> {
	
	@Override
	public boolean button(PrimeFacesButtonCommand command) {
		String id = command.id();
		boolean confirm = command.confirm();
		boolean ajax = command.ajax();

		SelenideElement element = $(byId(id));
		if (element.exists() && (!disabledClass(element))) {
			String viewState = getViewState();
			click(element);

			if (confirm) {
				confirm();
			}

			if (ajax) {
				waitForAjaxResponse();
			} else {
				waitForFullPageResponse(viewState);
			}

			return true;
		}

		return false;
	}

	@Override
	public boolean checkbox(PrimeFacesCheckboxCommand command) {
		String id = command.id();
		Boolean value = command.value();

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
			} else {
				for (int i = 0, l = 2; i < l; i++) { // try at most twice
					if (checkboxValue == null || (value.booleanValue() != checkboxValue.booleanValue())) {
						click(element);
						waitForAjaxResponse();
						checkboxValue = (Boolean) ((JavascriptExecutor) driver).executeScript(js);
					}
				}

				if (checkboxValue == null || (value.booleanValue() != checkboxValue.booleanValue())) {
					throw new IllegalStateException("Could not set checkbox to " + value);
				}
			}

			return true;
		}

		return false;
	}

	@Override
	public boolean dataGridButton(PrimeFacesDataGridButtonCommand command) {
		String dataGridId = command.dataGridId();
		String buttonId = command.buttonId();
		boolean ajax = command.ajax();

		// check data grid is present
		WebElement element = oldById(dataGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = oldById(buttonId);
			if (element != null && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on data grid button
				if (!element.getDomAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click($(element));

					if (ajax) {
						waitForAjaxResponse();
					} else {
						waitForFullPageResponse(viewState);
					}

					return true;
				}
			}
		}

		return false;
	}

	@Override
	public boolean dataGridSelect(PrimeFacesDataGridSelectCommand command) {
		String dataGridId = command.dataGridId();
		int row = command.row();

		// check list grid is present
		WebElement element = oldById(dataGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click($(element));

			waitForAjaxResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean listGridButton(PrimeFacesListGridButtonCommand command) {
		String listGridId = command.listGridId();
		String buttonId = command.buttonId();
		boolean ajax = command.ajax();

		// check list grid is present
		WebElement element = oldById(listGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = oldById(buttonId);
			if (element != null && element.isDisplayed() && element.isEnabled()) {
				// Look for prime faces disabled style on list grid button
				if (!element.getDomAttribute("class").contains("ui-state-disabled")) {
					// All good, continue with the button click
					String viewState = getViewState();
					click($(element));

					if (ajax) {
						waitForAjaxResponse();
					} else {
						waitForFullPageResponse(viewState);
					}

					return true;
				}
			}
		}

		return false;
	}

	@Override
	public boolean listGridSelect(PrimeFacesListGridSelectCommand command) {
		String listGridId = command.listGridId();
		int row = command.row();

		// check list grid is present
		WebElement element = oldById(listGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			element = element.findElement(By.xpath(String.format(".//tr[%s]/td", String.valueOf(row + 1))));
			click($(element));

			waitForAjaxResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean lookupDescriptionByRow(PrimeFacesLookupDescriptionByRowCommand command) {
		String id = command.id();
		int row = command.row();

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

		return false;
	}

	@Override
	public boolean lookupDescriptionBySearch(PrimeFacesLookupDescriptionBySearchCommand command) {
		String id = command.id();
		String search = command.search();

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

		return false;
	}

	@Override
	public boolean radio(PrimeFacesRadioCommand command) {
		String id = command.id();
		int index = command.index();

		SelenideElement element = $(byId(id));
		if (element.exists() && !disabledClass(element)) {
			element = $(byXpath("//label[@for='" + id + ":" + index + "']"));
			element.should(exist);
			click(element);

			waitForAjaxResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean tab(PrimeFacesTabCommand command) {
		String id = command.id();

		SelenideElement element = $(byXpath(String.format("//a[contains(@href, '#%s')]", id)));
		if (element.exists()) {
			click(element);

			waitForAjaxResponse();

			return true;
		}

		return false;
	}

	@Override
	public boolean text(PrimeFacesTextCommand command) {
		String id = command.id();
		String value = command.value();
		boolean keyPresses = command.keyPresses();

		boolean success = false;

		SelenideElement element = $(byId(id));
		if (element.exists()
				&& element.isDisplayed()
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

		if (success) {
			waitForAjaxResponse();
		}

		return success;
	}

	@Override
	public boolean selectOne(PrimeFacesSelectOneCommand command) {
		String id = command.id();
		int index = command.index();

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

		return false;
	}

	@Override
	public boolean confirm() {
		// Wait for confirm dialog to appear
		$(byId("confirmOK")).should(exist).shouldBe(visible);
		click($(byId("confirmOK")));

		return true;
	}

	@Override
	public boolean verifySuccess() {
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

		return true;
	}

	@Override
	public void assertSuccess() {
		Assert.assertTrue("Not successful", verifySuccess());
	}

	@Override
	public boolean verifyFailure(String messageToCheck) {
		WebElement messages = oldById("messages");
		if (messages != null && messages.isDisplayed()) {
			String innerHTML = messages.getDomProperty("innerHTML");
			if (innerHTML.contains("ui-messages-error") || innerHTML.contains("ui-messages-fatal")) {
				if (messageToCheck == null) {
					return true;
				} else if (innerHTML.contains(messageToCheck)) {
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

	public boolean _input(String id, String value, boolean keyPresses) {
		PrimeFacesTextCommand command = new PrimeFacesTextCommand(String.format("%s_input", id), value, keyPresses);

		return text(command);
	}

	public boolean redirectButton(String id, boolean confirm) {
		SelenideElement element = $(byId(id));
		if (element.exists() && (!disabledClass(element))) {
			String viewState = getViewState();
			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse(viewState);

			return true;
		}

		return false;
	}

	@SuppressWarnings("static-method")
	public void waitForAjaxResponse() {
		// Wait until wheelOfDeath is invisible after AJAX
		$(byId("wheelOfDeath_start")).shouldBe(hidden);
	}

	public void waitForFullPageResponse(String oldViewState) {
		WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(30));
		try {
			wait.ignoring(StaleElementReferenceException.class)
					.until(d -> ((getViewState() == null) || getViewState().equals(oldViewState)) ? Boolean.FALSE : Boolean.TRUE);
		} catch (RuntimeException e) {
			System.err.println("Timed out waiting for a navigation from " + driver.getCurrentUrl() + " : oldViewState = " + oldViewState);
			throw e;
		}

		wait = new WebDriverWait(driver, Duration.ofSeconds(5));
		wait.until(d -> ((JavascriptExecutor) d).executeScript("return document.readyState").equals("complete") ? Boolean.TRUE : Boolean.FALSE);
	}

	@SuppressWarnings("static-method")
	public String getViewState() {
		return $(byName("jakarta.faces.ViewState")).attr("value");
	}

	public void click(SelenideElement element) {
		WebElement wrappedElement = element.getWrappedElement();
		try {
			// Scroll the element into view on the page and see if the element can be made visible
			if (! element.is(visible, Duration.ofMillis(250))) {
				element.scrollIntoView(true);
			}

			wrappedElement.click();
		} catch (@SuppressWarnings("unused") WebDriverException e) {
			// This could occur when the control is behind a floating element
			JavascriptExecutor js = (JavascriptExecutor) driver;
			try {
				// Scroll to the top of the page and see if the element can be made visible
				trace("    Could not click on the element - scroll to the top of the page and try again");
				js.executeScript("javascript:window.scrollTo(0, 0)");
				wrappedElement.click();
			} catch (@SuppressWarnings("unused") WebDriverException e1) {
				try {
					// Scroll to the bottom of the page and try again in case the floating element is at the bottom
					trace("    Could not click on the element - scroll to the bottom of the page and try again");
					js.executeScript("javascript:window.scrollTo(0, 999999)");
					wrappedElement.click();
				} catch (@SuppressWarnings("unused") WebDriverException e2) {
					// Scroll the element into view on the page using javascript and see if the element can be made visible
					trace("    Could not click on the element - scroll to the element and try again");
					js.executeScript("arguments[0].scrollIntoView(true);", element);
					wrappedElement.click();
				}
			}
		}
	}

	private static boolean disabledClass(SelenideElement element) {
		String styleClass = element.attr("class");
		return (styleClass != null) && styleClass.contains("ui-state-disabled");
	}
	
	public List<WebElement> oldByClass(String className) {
		try {
			return driver.findElements(By.className(className));
		} catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	public List<WebElement> oldByCss(String selector) {
		try {
			return driver.findElements(By.cssSelector(selector));
		} catch (@SuppressWarnings("unused") NoSuchElementException e) {
			return null;
		}
	}

	public WebElement oldById(String id) {
		return by(i -> driver.findElement(By.id(i)),id);
	}

	public WebElement oldByXpath(String xpath) {
		return by(x -> driver.findElement(By.xpath(x)), xpath);
	}

	public WebElement oldByName(String name) {
		return by(n -> driver.findElement(By.name(n)), name);
	}

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
				} catch (NoSuchElementException | StaleElementReferenceException e) {
					if (l > MAX_WAIT) {
						throw e;
					}

					Thread.sleep(WAIT);
				}
			} catch (@SuppressWarnings("unused") InterruptedException e) {
				// do nothing here
			}
		}

		return null;
	}
}

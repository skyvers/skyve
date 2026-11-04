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
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

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

	/** How long to wait for an ajax request or a page to complete. */
	private static final Duration RESPONSE_TIMEOUT = Duration.ofSeconds(30);

	/** Selects error and fatal messages: global messages, growls and field messages. */
	private static final String ERROR_MESSAGES_SELECTOR = "#messages .ui-messages-error, #messages .ui-messages-fatal, " +
															"#growl .ui-growl-error, #growl .ui-growl-fatal, " +
															"#pushGrowl .ui-growl-error, #pushGrowl .ui-growl-fatal, " +
															".ui-message-error, .ui-message-fatal";

	/** Selects every message: global messages, growls and field messages. */
	private static final String ALL_MESSAGES_SELECTOR = "#messages .ui-messages-info, #messages .ui-messages-warn, " +
															"#growl .ui-growl-item-container, #pushGrowl .ui-growl-item-container, " +
															ERROR_MESSAGES_SELECTOR;

	/** The rows of a data table, excluding the header and the empty message. */
	private static final String DATA_ROW_XPATH = ".//tbody[contains(@class, 'ui-datatable-data')]/tr[%d][not(contains(@class, 'ui-datatable-empty-message'))]/td";

	@Override
	public boolean button(PrimeFacesButtonCommand command) {
		String id = command.id();
		boolean confirm = command.confirm();
		boolean ajax = command.ajax();

		SelenideElement element = $(byId(id));
		if (element.exists() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
			String viewState = markPage();
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
			if (value == null) {
				Boolean checkboxValue = checkboxValue(id);
				for (int i = 0, l = 2; i < l; i++) { // try at most twice
					if (checkboxValue != null) {
						click(element);
						waitForAjaxResponse();
						checkboxValue = checkboxValue(id);
					}
				}

				if (checkboxValue != null) {
					throw new IllegalStateException("Could not set checkbox to null or unknown value");
				}
			} else {
				Boolean checkboxValue = checkboxValue(id);
				for (int i = 0, l = 2; i < l; i++) { // try at most twice
					if (checkboxValue == null || (value.booleanValue() != checkboxValue.booleanValue())) {
						click(element);
						waitForAjaxResponse();
						checkboxValue = checkboxValue(id);
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

	/**
	 * Returns a check box's value.
	 *
	 * @param id	the check box's client id
	 * @return true, false, or null for the third state of a tri-state check box
	 */
	public @Nullable Boolean checkboxValue(@Nonnull String id) {
		return (Boolean) js("return window.SKYVE.PF.getCheckboxValue(arguments[0])", id);
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
			if (element != null && element.isDisplayed() && element.isEnabled() && !element.getDomAttribute("class").contains("ui-state-disabled")) {
				// All good, continue with the button click
				String viewState = markPage();
				click($(element));

				if (ajax) {
					waitForAjaxResponse();
				} else {
					waitForFullPageResponse(viewState);
				}

				return true;
			}
		}

		return false;
	}

	@Override
	public boolean dataGridSelect(PrimeFacesDataGridSelectCommand command) {
		return selectRow(command.dataGridId(), command.row());
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
			if (element != null && element.isDisplayed() && element.isEnabled() && !element.getDomAttribute("class").contains("ui-state-disabled")) {
				// All good, continue with the button click
				String viewState = markPage();
				click($(element));

				if (ajax) {
					waitForAjaxResponse();
				} else {
					waitForFullPageResponse(viewState);
				}

				return true;
			}
		}

		return false;
	}

	@Override
	public boolean listGridSelect(PrimeFacesListGridSelectCommand command) {
		return selectRow(command.listGridId(), command.row());
	}

	/**
	 * Clicks a data row of a data table.
	 *
	 * @param gridId	the data table's client id
	 * @param row	the zero-based row
	 * @return true if the grid is displayed and the row was clicked
	 * @throws IllegalStateException	if the grid has no such row
	 */
	private boolean selectRow(@Nonnull String gridId, int row) {
		// check grid is present
		WebElement element = oldById(gridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// Find the row
			List<WebElement> cells = element.findElements(By.xpath(String.format(DATA_ROW_XPATH, Integer.valueOf(row + 1))));
			if (cells.isEmpty()) {
				throw new IllegalStateException(String.format("Grid %s has no row %d", gridId, Integer.valueOf(row)));
			}
			click($(cells.get(0)));

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
			element.should(exist, RESPONSE_TIMEOUT).shouldBe(visible);

			// Select the zero-based row
			// NB could be <div><div><ul><li> or <span><ul><li>
			// in the middle means find the <ul> anywhere below
			element = $(byXpath(String.format("(//*[@id='%s_panel']//li[contains(@class, 'ui-autocomplete-item')])[%d]", id, Integer.valueOf(row + 1))));
			if (! element.exists()) {
				throw new IllegalStateException(String.format("Lookup %s has no row %d to pick", id, Integer.valueOf(row)));
			}
			click(element);

			waitForLookupSelection(id);

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
			element.should(exist, RESPONSE_TIMEOUT).shouldBe(visible, RESPONSE_TIMEOUT);

			// Select the first match
			// NB could be <div><div><ul><li> or <span><ul><li>
			// in the middle means find the <ul> anywhere below
			element = $(byXpath(String.format("(//*[@id='%s_panel']//li[contains(@class, 'ui-autocomplete-item')])[1]", id)));
			if (! element.exists()) {
				throw new IllegalStateException(String.format("Lookup %s has no match for '%s'", id, search));
			}
			click(element);
			waitForLookupSelection(id);

			return true;
		}

		return false;
	}

	/**
	 * Waits for a lookup's drop down to close after a pick, and then for its item select request.
	 */
	private void waitForLookupSelection(@Nonnull String id) {
		$(byId(id + "_panel")).shouldBe(hidden, RESPONSE_TIMEOUT);
		waitForAjaxResponse();
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
		// An ajax response that is still re-rendering the field can make it stale: wait and try again
		for (int attempt = 1; ; attempt++) {
			try {
				return textOnce(command);
			}
			catch (StaleElementReferenceException e) {
				if (attempt >= 3) {
					throw e;
				}
				waitForAjaxResponse();
			}
		}
	}

	private boolean textOnce(PrimeFacesTextCommand command) {
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
				// Leave the field as a user would, so that its change event (and any ajax handler) fires now
				js("if (document.activeElement === arguments[0]) arguments[0].blur();", element.getWrappedElement());
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

	/**
	 * Sets a slider: its widget, its hidden input (which is submitted) and its value display.
	 *
	 * @param sliderId	the slider's client id
	 * @param value	the value
	 * @return true if the slider is on the page and enabled
	 */
	public boolean slider(@Nonnull String sliderId, @Nullable String value) {
		Boolean result = (Boolean) js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
										"if ((! w) || w.jq.hasClass('ui-state-disabled') || w.jq.slider('option', 'disabled')) return false;" +
										"var v = arguments[1];" +
										"w.setValue(v);" +
										"w.input.val(v).trigger('change');" +
										"if (w.output) w.output.text(w.cfg.displayTemplate.replace('{value}', v));" +
										"return true;",
										sliderId,
										(value == null) ? "" : value);
		waitForAjaxResponse();
		return Boolean.TRUE.equals(result);
	}

	/**
	 * Sets a text editor (rich text or HTML) to plain text.
	 *
	 * @param id	the text editor's client id
	 * @param value	the text
	 * @return true if the editor is on the page and enabled
	 */
	public boolean textEditor(@Nonnull String id, @Nullable String value) {
		SelenideElement element = $(byId(id));
		if (! element.exists()) {
			return false;
		}
		element.scrollTo();
		Boolean result = (Boolean) js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
										"if ((! w) || w.disabled) return false;" +
										"if (! w.editor) {w.render();}" +
										"w.setValue(arguments[1]);" +
										"return true;",
										id,
										(value == null) ? "" : value);
		waitForAjaxResponse();
		return Boolean.TRUE.equals(result);
	}

	/**
	 * Sets a colour picker's text value without opening its popup.
	 *
	 * @param id	the colour picker's client id (its input)
	 * @param value	the colour, e.g. {@code #FF0000}
	 * @return true if the colour picker is on the page and enabled
	 */
	public boolean colourPicker(@Nonnull String id, @Nullable String value) {
		SelenideElement element = $(byId(id));
		if ((! element.exists()) || (element.attr("disabled") != null) || (element.attr("readonly") != null)) {
			return false;
		}
		js("var e = document.getElementById(arguments[0]);" +
				"e.value = arguments[1];" +
				"e.dispatchEvent(new Event('input', {bubbles: true}));" +
				"$(e).trigger('change');" +
				"var w = PrimeFaces.getWidgetById(arguments[0]);" +
				"if (w && w.hasBehavior('change')) w.callBehavior('change');",
				id,
				(value == null) ? "" : value);
		waitForAjaxResponse();
		return true;
	}

	/**
	 * Returns the value of an input element.
	 *
	 * @param id	the input's id
	 * @return its value, or null if there is no such element
	 */
	public @Nullable String inputValue(@Nonnull String id) {
		return (String) js("var e = document.getElementById(arguments[0]); return e ? (e.value == null ? '' : String(e.value)) : null;", id);
	}

	/**
	 * Returns a text editor's text and HTML.
	 *
	 * @param id	the text editor's client id
	 * @return the plain text then the HTML, or null if there is no such editor
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> textEditorValues(@Nonnull String id) {
		return (List<String>) js("var w = PrimeFaces.getWidgetById(arguments[0]); if (! w) return null;" +
									"var html = w.input.val() || '';" +
									"var text = w.editor ? w.editor.getText() : $('<div>').html(html).text();" +
									"return [text.trim(), html];", id);
	}

	/**
	 * Returns the selected option of a select one menu (combo) as its index, value and label.
	 *
	 * @param id	the select one menu's client id
	 * @return the selected index, value and label, or null if there is no such menu
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> selectOneValues(@Nonnull String id) {
		return (List<String>) js("var s = document.getElementById(arguments[0] + '_input'); if (! s) return null;" +
									"var i = s.selectedIndex; if (i < 0) return ['', '', ''];" +
									"var o = s.options[i];" +
									"var label = (o.text || '').replace(/\\u00a0/g, ' ').trim();" +
									"return [String(i), o.value, label];", id);
	}

	/**
	 * Returns the selected option of a radio group as its index, value and label.
	 *
	 * @param id	the radio group's client id
	 * @return the selected index, value and label (empty strings when nothing is selected), or null if there is no such group
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> radioValues(@Nonnull String id) {
		return (List<String>) js("if (! document.getElementById(arguments[0])) return null;" +
									"var inputs = document.getElementsByName(arguments[0]);" +
									"for (var i = 0; i < inputs.length; i++) {" +
									"  if (inputs[i].checked) {" +
									"    var l = document.querySelector('label[for=\"' + inputs[i].id + '\"]');" +
									"    return [String(i), inputs[i].value, l ? l.textContent.trim() : ''];" +
									"  }" +
									"}" +
									"return ['', '', ''];", id);
	}

	/**
	 * Returns the label (or else the title) that an element shows, such as a button's text.
	 *
	 * @param id	the element's id
	 * @return the label with whitespace collapsed, or null if there is no such element
	 */
	public @Nullable String label(@Nonnull String id) {
		return (String) js("var e = document.getElementById(arguments[0]); if (! e) return null;" +
								"var t = e.querySelector('.ui-button-text');" +
								"var s = ((t ? t.textContent : e.textContent) || '').replace(/\\s+/g, ' ').trim();" +
								"return s || e.title || '';", id);
	}

	@Override
	public boolean confirm() {
		// Wait for confirm dialog to appear
		$(byId("confirmOK")).should(exist).shouldBe(visible);
		click($(byId("confirmOK")));

		return true;
	}

	/**
	 * Returns the text of each error or fatal message on the page: global messages, growls and field messages.
	 *
	 * @return the messages; empty when there are none
	 */
	public @Nonnull List<String> errorMessages() {
		return messages(ERROR_MESSAGES_SELECTOR);
	}

	@SuppressWarnings("unchecked")
	private @Nonnull List<String> messages(@Nonnull String selector) {
		List<String> result = (List<String>) js("var out = [];" +
													"document.querySelectorAll(arguments[0]).forEach(function(e) {" +
													"  var t = (e.innerText || e.textContent || '').replace(/\\s+/g, ' ').trim();" +
													"  if (t && (out.indexOf(t) < 0)) out.push(t);" +
													"});" +
													"return out;", selector);
		return (result == null) ? new ArrayList<>() : result;
	}

	@Override
	public @Nonnull String describeMessages() {
		List<String> messages = messages(ALL_MESSAGES_SELECTOR);
		return messages.isEmpty() ? "no messages" : String.join(" | ", messages);
	}

	@Override
	public boolean verifySuccess() {
		return errorMessages().isEmpty();
	}

	@Override
	public void assertSuccess() {
		if (! verifySuccess()) {
			throw new AssertionError("Not successful: " + describeMessages());
		}
	}

	@Override
	public boolean verifyFailure(String messageToCheck) {
		for (String message : errorMessages()) {
			if ((messageToCheck == null) || message.contains(messageToCheck)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean verifyFailure() {
		return verifyFailure(null);
	}

	@Override
	public void assertFailure(String messageToCheck) {
		if (! verifyFailure(messageToCheck)) {
			throw new AssertionError("Successful: " + describeMessages());
		}
	}

	@Override
	public void assertFailure() {
		assertFailure(null);
	}

	public boolean _input(String id, String value, boolean keyPresses) {
		PrimeFacesTextCommand command = new PrimeFacesTextCommand(String.format("%s_input", id), value, keyPresses);

		return text(command);
	}

	public boolean redirectButton(String id, boolean confirm) {
		SelenideElement element = $(byId(id));
		if (element.exists() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
			String viewState = markPage();
			click(element);

			if (confirm) {
				confirm();
			}

			waitForFullPageResponse(viewState);

			return true;
		}

		return false;
	}

	/**
	 * Waits until no PrimeFaces or jQuery ajax request is running and the ajax status indicator is hidden.
	 */
	public void waitForAjaxResponse() {
		WebDriverWait wait = new WebDriverWait(driver, RESPONSE_TIMEOUT);
		wait.ignoring(StaleElementReferenceException.class).until(d -> Boolean.valueOf(ajaxIdle()));
	}

	/**
	 * Indicates if the page has loaded and no PrimeFaces or jQuery ajax request is running.
	 *
	 * @return true when idle
	 */
	private boolean ajaxIdle() {
		return Boolean.TRUE.equals(js("if (document.readyState !== 'complete') return false;" +
										"if (window.PrimeFaces && PrimeFaces.ajax && PrimeFaces.ajax.Queue && (! PrimeFaces.ajax.Queue.isEmpty())) return false;" +
										"if (window.jQuery && (jQuery.active > 0)) return false;" +
										"var w = document.getElementById('wheelOfDeath_start');" +
										"return (! w) || (w.offsetParent === null);"));
	}

	/**
	 * Waits for a new page (or a new view state), or for the request to come back with error messages
	 * (such as validation errors), which leave the page as it was.
	 *
	 * @param oldViewState	the view state before the request
	 */
	public void waitForFullPageResponse(String oldViewState) {
		WebDriverWait wait = new WebDriverWait(driver, RESPONSE_TIMEOUT);
		try {
			wait.ignoring(StaleElementReferenceException.class)
					.until(d -> {
						String viewState = markPage();
						if ((viewState != null) && (! viewState.equals(oldViewState))) {
							return Boolean.TRUE;
						}
						return Boolean.valueOf(ajaxIdle() && messagesReplaced() && (! errorMessages().isEmpty()));
					});
		} catch (RuntimeException e) {
			System.err.println("Timed out waiting for a navigation from " + driver.getCurrentUrl() + " : oldViewState = " + oldViewState);
			throw e;
		}

		waitForAjaxResponse();
	}

	/**
	 * Marks the global messages element, so {@link #messagesReplaced()} can tell when a response has re-rendered it,
	 * and returns the view state, before a click.
	 *
	 * @return the view state
	 */
	private String markPage() {
		js("var m = document.getElementById('messages'); if (m) m.sailMark = true;");
		return getViewState();
	}

	/**
	 * Indicates if the global messages element marked by {@link #markPage()} has since been replaced (or the page has).
	 *
	 * @return true if it has been replaced
	 */
	private boolean messagesReplaced() {
		return Boolean.TRUE.equals(js("var m = document.getElementById('messages'); return (! m) || (! m.sailMark);"));
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
				element.scrollTo();
			}

			wrappedElement.click();
		} catch (@SuppressWarnings("unused") WebDriverException e) {
			// This could occur when the control is behind a floating element
			JavascriptExecutor js = (JavascriptExecutor) driver;
			try {
				// Scroll to the top of the page and see if the element can be made visible
				trace("    Could not click on the element - scroll to the top of the page and try again");
				js.executeScript("window.scrollTo(0, 0)");
				wrappedElement.click();
			} catch (@SuppressWarnings("unused") WebDriverException e1) {
				try {
					// Scroll to the bottom of the page and try again in case the floating element is at the bottom
					trace("    Could not click on the element - scroll to the bottom of the page and try again");
					js.executeScript("window.scrollTo(0, 999999)");
					wrappedElement.click();
				} catch (@SuppressWarnings("unused") WebDriverException e2) {
					// Scroll the element into view on the page using javascript and see if the element can be made visible
					trace("    Could not click on the element - scroll to the element and try again");
					js.executeScript("arguments[0].scrollIntoView(true);", wrappedElement);
					wrappedElement.click();
				}
			}
		}
	}

	private @Nullable Object js(@Nonnull String script, Object... arguments) {
		return ((JavascriptExecutor) driver).executeScript(script, arguments);
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

	private static long maxWait = 1000L;
	private static long waitInterval = 50L;

	private static @Nullable WebElement by(@Nonnull Function<String, WebElement> function, @Nonnull String search) {
		for (long l = 0; l <= maxWait; l += waitInterval) {
			try {
				try {
					WebElement result = function.apply(search);
					result.isDisplayed(); // check for stale element
					result.isEnabled(); // check for stale element

					return result;
				} catch (NoSuchElementException | StaleElementReferenceException e) {
					if (l > maxWait) {
						throw e;
					}

					Thread.sleep(waitInterval); // NOSONAR java:S2925
				}
			} catch (@SuppressWarnings("unused") InterruptedException e) {
				// do nothing here
			}
		}

		return null;
	}
}

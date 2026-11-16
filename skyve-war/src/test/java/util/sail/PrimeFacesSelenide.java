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
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
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


	/** Selects error and fatal messages: global messages, growls and field messages. */
	private static final String ERROR_MESSAGES_SELECTOR = "#messages .ui-messages-error, #messages .ui-messages-fatal, " +
															"#growl .ui-growl-error, #growl .ui-growl-fatal, " +
															"#pushGrowl .ui-growl-error, #pushGrowl .ui-growl-fatal, " +
															".ui-message-error, .ui-message-fatal";

	/** Selects every message: global messages, growls and field messages. */
	private static final String ALL_MESSAGES_SELECTOR = "#messages .ui-messages-info, #messages .ui-messages-warn, " +
															"#growl .ui-growl-item-container, #pushGrowl .ui-growl-item-container, " +
															ERROR_MESSAGES_SELECTOR;


	@Override
	public boolean button(PrimeFacesButtonCommand command) {
		return button(command.id(), command.ajax(), command.confirm() ? Boolean.TRUE : null);
	}

	/**
	 * Clicks a button, answers any confirmation or server-side question it raises, and waits for the response.
	 *
	 * @param id	the button's client id
	 * @param ajax	true to wait for ajax, false to wait for a new page (or view state)
	 * @param answer	true or null to answer positively if asked, false to answer negatively (which must be asked)
	 * @return true if the button was on the page, enabled and clicked
	 * @throws IllegalStateException	if the answer is false but nothing asked
	 */
	public boolean button(@Nonnull String id, boolean ajax, @Nullable Boolean answer) {
		SelenideElement element = $(byId(id));
		// a user can only press a button that is shown (another rendering of the same action may be)
		if (element.exists() && element.isDisplayed() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
			String viewState = markPage();
			click(element);

			declined = answerQuestions(answer);
			if (declined) {
				return true;
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

	/**
	 * Indicates if the client-side confirmation dialog shows. A page that is being replaced counts as not showing it.
	 */
	private boolean confirmationShown() {
		// No wait: PrimeFaces shows its confirmation synchronously in the click handler
		try {
			return Boolean.TRUE.equals(js("var b = document.getElementById('confirmOK'); return !! b && b.getClientRects().length > 0;"));
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return false; // the page is being replaced
		}
	}

	/**
	 * Answers the client-side confirmation and the server-side questions (in turn) that the last click raised.
	 *
	 * @param answer	true or null to answer positively, false to answer negatively
	 * @return true if the gesture was declined, so there is no response to wait for
	 * @throws IllegalStateException	if the answer is false but nothing asked
	 */
	private boolean answerQuestions(@Nullable Boolean answer) {
		boolean positive = ! Boolean.FALSE.equals(answer);
		boolean asked = false;
		// A client-side confirmation shows straight away, before any request
		if (confirmationShown()) {
			asked = true;
			trace(positive ? "    confirm" : "    decline the confirmation");
			click($(byId(positive ? "confirmOK" : "confirmCancel")));
			if (! positive) {
				return true;
			}
			// Wait for the confirmed gesture's request to start (or its page to unload): the confirmation can send it
			// after its own handling, so the page can look idle in between
			waitFor(d -> requestSent() ? Boolean.TRUE : null);
		}
		// A server-side question shows when its request comes back; answering resends the request
		for (int i = 0; i < 10; i++) {
			waitForAjaxResponse();
			if (! Boolean.TRUE.equals(js("var d = document.querySelector('[role=alertdialog][aria-labelledby=skyveQuestionTitle]');" +
											"return !! (d && d.getClientRects().length > 0);"))) {
				break;
			}
			asked = true;
			String question = (String) js("var m = document.getElementById('skyveQuestionMessage'); return m ? m.textContent : '';");
			trace((positive ? "    answer yes to " : "    answer no to ") + question);
			// Choose the answer's button: [the button, whether it aborts the gesture]
			@SuppressWarnings("unchecked")
			List<Object> choice = (List<Object>) js("var d = document.querySelector('[role=alertdialog][aria-labelledby=skyveQuestionTitle]');" +
														"var buttons = Array.from(d.querySelectorAll('.ui-dialog-buttonpane button'));" +
														"var close = d.querySelector('.ui-dialog-titlebar-close');" +
														"var abortLabel = close ? close.getAttribute('aria-label') : null;" +
														"var negative = buttons.find(function(b) {return b.textContent.trim() === abortLabel;}) || buttons[buttons.length - 1];" +
														"var positive = buttons.find(function(b) {return b !== negative;});" +
														"var chosen = arguments[0] ? (positive || negative) : negative;" +
														"return [chosen, chosen === negative && abortLabel !== null];",
														Boolean.valueOf(positive));
			if ((choice == null) || (choice.get(0) == null)) {
				throw new IllegalStateException("The question \"" + question + "\" has no button to answer it with");
			}
			click($((WebElement) choice.get(0)));
			if (Boolean.TRUE.equals(choice.get(1))) {
				return true;
			}
		}
		if ((! positive) && (! asked)) {
			throw new IllegalStateException("Expected a confirmation or question to decline, but none was shown");
		}
		return false;
	}

	@Override
	public boolean checkbox(PrimeFacesCheckboxCommand command) {
		String id = command.id();
		Boolean value = command.value();

		SelenideElement element = $(byId(id));
		if (element.exists()) {
			// Wait for the widget to be shown and enabled (a response that renders or enables it may still be applying)
			element.shouldBe(visible).shouldBe(enabled);

			SelenideElement inputElement = $(byId(String.format("%s_input", id)));
			// Wait for the check box input of the widget to be rendered
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
		WebElement element = elementById(dataGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = elementById(buttonId);
			if (element != null && element.isDisplayed() && element.isEnabled() && !element.getDomAttribute("class").contains("ui-state-disabled")) {
				// All good, continue with the button click
				String viewState = markPage();
				click($(element));

				if (answerQuestions(null)) {
					return true;
				}
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
		WebElement element = elementById(listGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = elementById(buttonId);
			if (element != null && element.isDisplayed() && element.isEnabled() && !element.getDomAttribute("class").contains("ui-state-disabled")) {
				// All good, continue with the button click
				String viewState = markPage();
				click($(element));

				if (answerQuestions(null)) {
					return true;
				}
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
	 * Zooms into a row of a list whose rows zoom when selected (it has no zoom button), and waits for the new page.
	 *
	 * @param gridId	the data table's client id
	 * @param row	the zero-based row
	 * @return true if the list is displayed and the row was clicked
	 */
	public boolean zoomRowBySelection(@Nonnull String gridId, int row) {
		js("window.sailPage = true;");
		if (! selectRow(gridId, row)) {
			return false;
		}
		// Wait for the row's edit page to replace this one and load
		waitForNewPage();
		waitForAjaxResponse();
		return true;
	}

	/**
	 * Indicates if a list grid is a Vue list grid (skyve-vue's ListGrid, mounted in the grid's container).
	 *
	 * @param gridId	the grid's client id (its container)
	 * @return true for a Vue list grid
	 */
	public boolean isVueListGrid(@Nonnull String gridId) {
		return Boolean.TRUE.equals(js("var e = document.getElementById(arguments[0]); return !! (e && e.querySelector('.skyve-vue-list-grid'));", gridId));
	}

	/**
	 * Presses a Vue list grid's New button and waits for the new record's edit page.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @return true if the grid shows an enabled New button and it was pressed
	 */
	public boolean vueListGridNew(@Nonnull String gridId) {
		// the toolbar's New button, or the floating one a list view keeps above its paginator
		WebElement button = (WebElement) js("var e = document.getElementById(arguments[0]); if (! e) return null;" +
											"var shown = function(b) {return b && (b.getClientRects().length > 0) && (! b.disabled);};" +
											"var b = e.querySelector('button.skyve-vue-list-grid-new');" +
											"if (shown(b)) return b;" +
											"b = e.querySelector('.skyve-vue-list-grid-fab:not(.skyve-vue-list-grid-fab-hidden) button');" +
											"return shown(b) ? b : null;", gridId);
		if (button == null) {
			return false;
		}
		js("window.sailPage = true;");
		click($(button));
		// Wait for the new record's edit page to replace this one and load
		waitForNewPage();
		return true;
	}

	/**
	 * Zooms into a row of a Vue list grid with its View Detail button (or by clicking the row, when the grid zooms on a
	 * row click and has no button), and waits for the row's edit page.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @param row	the zero-based row
	 * @return true if the row could be zoomed into and was
	 */
	public boolean vueListGridZoom(@Nonnull String gridId, int row) {
		WebElement target = (WebElement) js(ROWS_JS + "var r = rows[arguments[1]]; if (! r) return null;" +
											"var b = r.querySelector('td.skyve-vue-list-grid-action button');" +
											"if (b) return b.disabled ? null : b;" +
											"return t.querySelector('.skyve-vue-list-grid-zoomable') ? r.querySelector('td') : null;",
											gridId, Integer.valueOf(row));
		if (target == null) {
			return false;
		}
		js("window.sailPage = true;");
		click($(target));
		// Wait for the row's edit page to replace this one and load
		waitForNewPage();
		return true;
	}

	/**
	 * Selects a row of a Vue list grid that has a selected action, as a user would, and waits for the selected action.
	 * <p>
	 * RUNTIME BUG (report bug 7): the grid restores its selection from its session state when the page is shown again,
	 * without running the selected action, so a row can show as selected while the view's selected id binding is empty
	 * (and a click on it unselects it). A user clicks such a row twice: once to unselect it, once to select it.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @param row	the zero-based row
	 * @return true if the row is selected
	 * @throws IllegalStateException	if the grid has no such row, or the row does not show as selected after the click
	 */
	public boolean vueListGridSelect(@Nonnull String gridId, int row) {
		String selectedJs = ROWS_JS + "var r = rows[arguments[1]]; if (! r) return null;" +
								"return r.classList.contains('p-highlight') || (r.getAttribute('aria-selected') === 'true') || " +
								"  (r.getAttribute('data-p-highlight') === 'true');";
		Object selected = js(selectedJs, gridId, Integer.valueOf(row));
		if (selected == null) {
			throw new IllegalStateException(String.format("Grid %s has no row %d", gridId, Integer.valueOf(row)));
		}
		if (Boolean.TRUE.equals(selected)) {
			trace("    the row shows as selected already (restored by the grid): click it to unselect it first");
			selectRow(gridId, row);
		}
		selectRow(gridId, row);
		if (! Boolean.TRUE.equals(js(selectedJs, gridId, Integer.valueOf(row)))) {
			throw new IllegalStateException(String.format("Row %d of grid %s does not show as selected after clicking it", Integer.valueOf(row), gridId));
		}
		return true;
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
		WebElement element = elementById(gridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// Find the row's first cell (of a PrimeFaces data table or a Vue list grid)
			WebElement cell = (WebElement) js(ROWS_JS + "var r = rows[arguments[1]]; return r ? r.querySelector('td') : null;",
												gridId, Integer.valueOf(row));
			if (cell == null) {
				throw new IllegalStateException(String.format("Grid %s has no row %d", gridId, Integer.valueOf(row)));
			}
			click($(cell));

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
			// Wait for the widget to be shown and enabled (a response that renders or enables it may still be applying)
			element.shouldBe(visible).shouldBe(enabled);
			click(element);

			// Wait for pick list drop down - can be a span or div depending on theme
			element = $(byXpath(String.format("//*[@id='%s_panel']", id)));
			// Wait for the lookup's drop down panel to open with the search response
			element.should(exist).shouldBe(visible);

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
			// Wait for the lookup's drop down panel to open with the search response
			element.should(exist).shouldBe(visible);

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
	 * Picks the first row of a lookup description's drop down, in the order shown, whose text contains the given text
	 * (case-sensitively). The drop down is opened with its button, or with a search typed into the lookup.
	 *
	 * @param id	the lookup's client id
	 * @param search	the text to type to open the drop down, or null to open it with its button
	 * @param text	the text the row contains
	 * @return true if the lookup is on the page and the row was picked
	 * @throws IllegalStateException	if no row contains the text
	 */
	public boolean lookupDescriptionContaining(@Nonnull String id, @Nullable String search, @Nonnull String text) {
		if (! $(byId(id)).exists()) {
			return false;
		}
		if (search == null) {
			SelenideElement button = $(byXpath(String.format("//span[@id='%s']/button", id)));
			// Wait for the widget to be shown and enabled (a response that renders or enables it may still be applying)
			button.shouldBe(visible).shouldBe(enabled);
			click(button);
		}
		else {
			_input(id, search, true);
		}
		// Wait for the lookup's drop down panel to open with the search response
		$(byXpath(String.format("//*[@id='%s_panel']", id))).should(exist).shouldBe(visible);

		// The first row containing the text, or the rows' texts if none does
		Object row = js("var p = document.getElementById(arguments[0] + '_panel'); var wanted = arguments[1];" +
							"var items = Array.from(p.querySelectorAll('li.ui-autocomplete-item'));" +
							"return items.find(function(li) {return (li.textContent || '').indexOf(wanted) >= 0;}) || " +
							"  items.map(function(li) {return (li.textContent || '').replace(/\\s+/g, ' ').trim();}).join(' | ');", id, text);
		if (! (row instanceof WebElement element)) {
			throw new IllegalStateException(String.format("Lookup %s has no row containing '%s': its rows are %s",
															id, text, "".equals(row) ? "(none)" : row));
		}
		click($(element));
		waitForLookupSelection(id);
		return true;
	}

	/**
	 * Waits for a lookup's drop down to close after a pick, and then for its item select request.
	 */
	private void waitForLookupSelection(@Nonnull String id) {
		// Wait for the drop down to close after the pick
		$(byId(id + "_panel")).shouldBe(hidden);
		waitForAjaxResponse();
	}

	@Override
	public boolean radio(PrimeFacesRadioCommand command) {
		String id = command.id();
		int index = command.index();

		SelenideElement element = $(byId(id));
		if (element.exists() && !disabledClass(element)) {
			element = $(byXpath("//label[@for='" + id + ":" + index + "']"));
			// Wait for the item to be rendered in the opened list or panel
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

		// the tab's header link (exactly this tab: a contains() test would also match tab10 for tab1)
		SelenideElement element = $(byXpath(String.format("//a[@href='#%s']", id)));
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
			scrollToCentre(element.getWrappedElement());
			waitUntilUncovered(element.getWrappedElement());
			element.clear();

			if (keyPresses) {
				if (value != null) {
					element.sendKeys(value);
				}
			} else {
				element.val(value == null ? "" : value);
				// Leave the field as a user would (Tab), so that its change event (and any ajax handler) fires now
				element.sendKeys(Keys.TAB);
			}

			success = true;
		}

		if (success) {
			waitForAjaxResponse();
		}

		return success;
	}

	/**
	 * Returns the text to type into a PrimeFaces date picker for a value in its converter's format: the value, with a
	 * 12 hour time's AM or PM (in either case) as the picker's locale writes it. Skyve's converters write AM and PM
	 * (Locale.ENGLISH), but the picker reads only its locale's own: in en-AU and en-GB, am and pm, so a typed PM is
	 * refused and the picker empties itself. The server reads either.
	 *
	 * @param id	the date picker's client id
	 * @param value	the value, in the format of the field's converter
	 * @return the text to type
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull String datePickerText(@Nonnull String id, @Nonnull String value) {
		List<Object> amPm = (List<Object>) js("var w = PrimeFaces.getWidgetById(arguments[0]); var c = w ? w.cfg : null;" +
												"var l = c ? (c.locale || c.userLocale) : null;" +
												"return (c && (c.hourFormat === '12') && l && l.am && l.pm) ? [l.am, l.pm] : null;", id);
		int end = value.length() - 2;
		if ((amPm != null) && (end > 0) && (value.charAt(end - 1) == ' ')) {
			String marker = value.substring(end);
			if (marker.equalsIgnoreCase("AM")) {
				return value.substring(0, end) + amPm.get(0);
			}
			if (marker.equalsIgnoreCase("PM")) {
				return value.substring(0, end) + amPm.get(1);
			}
		}
		return value;
	}

	/**
	 * Replaces the text of an input as a user does: clicks into it, selects its text and types over it (or deletes it),
	 * then leaves it with Tab, so that its change event (and any ajax handler) fires once, with the new text.
	 * {@code clear()} would first fire a change event with an empty value: a date picker that holds a value can then be
	 * re-rendered by that event's response with its old value, after the new text was typed.
	 *
	 * @param id	the input's id
	 * @param value	the text, or null or empty to delete the text
	 * @return true if the input is shown, enabled and editable
	 */
	public boolean typeOver(@Nonnull String id, @Nullable String value) {
		SelenideElement element = $(byId(id));
		if ((! element.exists()) || (! element.isDisplayed()) || (! element.isEnabled()) ||
				(element.attr("disabled") != null) || (element.attr("readonly") != null)) {
			return false;
		}
		click(element);
		element.sendKeys(Keys.chord(selectAllKey(), "a"));
		element.sendKeys(((value == null) || value.isEmpty()) ? Keys.BACK_SPACE.toString() : value);
		element.sendKeys(Keys.TAB);
		waitForAjaxResponse();
		return true;
	}

	@Override
	public boolean selectOne(PrimeFacesSelectOneCommand command) {
		String id = command.id();
		int index = command.index();

		SelenideElement element = $(byId(id));
		if (element.exists() && (! disabledClass(element))) {
			// Wait for the widget to be shown and enabled (a response that renders or enables it may still be applying)
			element.shouldBe(visible).shouldBe(enabled);
			element = $(byId(String.format("%s_label", id)));
			// Wait for the item to be rendered in the opened list or panel
			element.should(exist);
			click(element);

			// Wait for pick list drop down
			element = $(byId(String.format("%s_panel", id)));
			// Wait for the combo's drop down panel to open
			element.should(appear);

			// Value here should be an index in the drop down starting from 0
			element = $(byId(String.format("%s_%s", id, String.valueOf(index))));
			// Wait for the item to be rendered in the opened list or panel
			element.should(exist);
			click(element);

			// Wait for the drop down's closing transition to end: at its end PrimeFaces hides the panel and, in the same
			// handler, focuses the menu, which scrolls the window back to it. Wait for the hidden panel, not for
			// aria-expanded to be false: an item clicked while the drop down is still opening closes it before
			// aria-expanded was ever true, so that attribute is false all through the closing transition.
			waitFor(d -> js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
								"return ((! w) || (! w.panel) || (! w.panel.is(':visible'))) ? true : null;",
								id));
			waitForAjaxResponse();

			return true;
		}

		return false;
	}

	/**
	 * Sets a slider as a user would: drags its handle to the value's position, then presses the arrow keys on the handle
	 * for any steps the drag missed (a pixel can be worth more than a step).
	 *
	 * @param sliderId	the slider's client id
	 * @param value	the value
	 * @return true if the slider is on the page and enabled
	 * @throws IllegalStateException	if the slider cannot be set to the value (e.g. it is out of range or off its steps)
	 */
	public boolean slider(@Nonnull String sliderId, @Nullable String value) {
		@SuppressWarnings("unchecked")
		List<Number> range = (List<Number>) js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
												"if ((! w) || w.jq.hasClass('ui-state-disabled') || w.jq.slider('option', 'disabled')) return null;" +
												"var o = w.jq.slider('option');" +
												"return [o.min, o.max, o.step || 1, (o.orientation === 'vertical') ? 1 : 0];", sliderId);
		if (range == null) {
			return false;
		}
		double min = range.get(0).doubleValue();
		double max = range.get(1).doubleValue();
		double step = range.get(2).doubleValue();
		boolean vertical = (range.get(3).intValue() == 1);
		double target = ((value == null) || value.isBlank()) ? min : Double.parseDouble(value.trim());

		WebElement slider = driver.findElement(By.id(sliderId));
		WebElement handle = slider.findElement(By.cssSelector(".ui-slider-handle"));
		scrollToCentre(handle);
		waitUntilUncovered(handle);
		org.openqa.selenium.Rectangle track = slider.getRect();
		double fraction = (max > min) ? Math.max(0, Math.min(1, (target - min) / (max - min))) : 0;
		@SuppressWarnings("unchecked")
		List<Number> origin = (List<Number>) js("var r = arguments[0].getBoundingClientRect(); return [r.left, r.top, r.width, r.height];", slider);
		double left = origin.get(0).doubleValue();
		double top = origin.get(1).doubleValue();
		double width = origin.get(2).doubleValue();
		double height = origin.get(3).doubleValue();
		int x = (int) Math.round(vertical ? (left + (width / 2)) : (left + (fraction * width)));
		int y = (int) Math.round(vertical ? (top + ((1 - fraction) * height)) : (top + (height / 2)));
		trace(String.format("    drag the slider handle to %s (track %dx%d)", value, Integer.valueOf(track.getWidth()), Integer.valueOf(track.getHeight())));
		new org.openqa.selenium.interactions.Actions(driver).clickAndHold(handle).moveToLocation(x, y).release().perform();

		// The drag lands on the nearest pixel: press the arrow keys for the steps it missed
		for (int presses = 0; presses < 1000; presses++) {
			double current = ((Number) js("return PrimeFaces.getWidgetById(arguments[0]).jq.slider('value');", sliderId)).doubleValue();
			long steps = Math.round((target - current) / step);
			if (steps == 0) {
				break;
			}
			handle.sendKeys((steps > 0) ? Keys.ARROW_RIGHT : Keys.ARROW_LEFT);
		}
		waitForAjaxResponse();
		double current = ((Number) js("return PrimeFaces.getWidgetById(arguments[0]).jq.slider('value');", sliderId)).doubleValue();
		if (Math.abs(current - target) > (step / 2)) {
			throw new IllegalStateException(String.format("Could not set slider %s to %s (range %s to %s, step %s): it shows %s",
															sliderId, value, Double.valueOf(min), Double.valueOf(max), Double.valueOf(step), Double.valueOf(current)));
		}
		return true;
	}

	/**
	 * Sets a text editor (rich text or HTML) to plain text as a user would: clicks into it, selects all its text and
	 * types the value over it. The editor saves the text into its submitted input as it changes, and reports the change
	 * when it loses the focus (the next step's click).
	 *
	 * @param id	the text editor's client id
	 * @param value	the text
	 * @return true if the editor is on the page and enabled
	 */
	public boolean textEditor(@Nonnull String id, @Nullable String value) {
		SelenideElement element = $(byId(id));
		if ((! element.exists()) || (! element.isDisplayed()) || Boolean.TRUE.equals(js("var w = PrimeFaces.getWidgetById(arguments[0]); return (! w) || w.disabled;", id))) {
			return false;
		}
		// Wait for the editor to render (it is deferred until it is shown)
		WebElement editor = waitFor(d -> {
			List<WebElement> editors = d.findElements(By.cssSelector("[id='" + id + "'] .ql-editor"));
			return editors.isEmpty() ? null : editors.get(0);
		});
		click($(editor));
		editor.sendKeys(Keys.chord(selectAllKey(), "a"));
		if ((value == null) || value.isEmpty()) {
			editor.sendKeys(Keys.BACK_SPACE);
		}
		else {
			editor.sendKeys(value);
		}
		waitForAjaxResponse();
		return true;
	}

	/**
	 * Returns the key that, with "a", selects all: Command on a Mac browser, Control elsewhere.
	 */
	private @Nonnull Keys selectAllKey() {
		Object platform = js("return navigator.platform || '';");
		return String.valueOf(platform).toLowerCase().startsWith("mac") ? Keys.COMMAND : Keys.CONTROL;
	}

	/**
	 * Sets a colour picker as a keyboard user would: types the colour into its text field and leaves it with Tab.
	 * The field gets the focus without a click, as a click opens the colour popup (which takes the focus, and would
	 * then have to be closed with a click elsewhere, or with Escape, which undoes the change).
	 *
	 * @param id	the colour picker's client id (its input)
	 * @param value	the colour, e.g. {@code #FF0000}
	 * @return true if the colour picker is on the page and enabled
	 */
	public boolean colourPicker(@Nonnull String id, @Nullable String value) {
		SelenideElement element = $(byId(id));
		if ((! element.exists()) || (! element.isDisplayed()) || (element.attr("disabled") != null) || (element.attr("readonly") != null)) {
			return false;
		}
		scrollToCentre(element.getWrappedElement());
		waitUntilUncovered(element.getWrappedElement());
		element.clear();
		element.sendKeys(((value == null) ? "" : value), Keys.TAB);
		waitForAjaxResponse();
		return true;
	}

	/**
	 * Makes the given items (by label) the members of a pick list (list membership); every other item becomes a candidate.
	 * As a user would: for each item in the wrong list, clicks it, then clicks the Add or Remove button.
	 *
	 * @param id	the pick list's client id
	 * @param members	the labels of the items to make members
	 * @return the labels that are not items of the pick list (empty when all were found), or null if the pick list is not on the page
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> pickList(@Nonnull String id, @Nonnull List<String> members) {
		List<String> missing = (List<String>) js("var w = PrimeFaces.getWidgetById(arguments[0]); if ((! w) || (! w.sourceList)) return null;" +
													"var wanted = arguments[1];" +
													"var label = function(li) {return (li.getAttribute('data-item-label') || li.textContent || '').replace(/\\s+/g, ' ').trim();};" +
													"var all = w.sourceList.children('li.ui-picklist-item').toArray().concat(w.targetList.children('li.ui-picklist-item').toArray());" +
													"var labels = all.map(label);" +
													"var missing = wanted.filter(function(x) {return labels.indexOf(x) < 0;});" +
													"return missing.length ? missing.concat(['(the items are: ' + labels.join(' | ') + ')']) : [];", id, members);
		if ((missing == null) || (! missing.isEmpty())) {
			return missing;
		}
		for (int moves = 0; moves < 1000; moves++) {
			// the next item in the wrong list, and the button that moves it: [item, button] or null when none is left
			List<WebElement> move = (List<WebElement>) js("var w = PrimeFaces.getWidgetById(arguments[0]); var wanted = arguments[1];" +
															"var label = function(li) {return (li.getAttribute('data-item-label') || li.textContent || '').replace(/\\s+/g, ' ').trim();};" +
															"var out = w.targetList.children('li.ui-picklist-item').toArray().find(function(li) {return wanted.indexOf(label(li)) < 0;});" +
															"if (out) return [out, w.jq.find('.ui-picklist-button-remove')[0]];" +
															"var inn = w.sourceList.children('li.ui-picklist-item').toArray().find(function(li) {return wanted.indexOf(label(li)) >= 0;});" +
															"return inn ? [inn, w.jq.find('.ui-picklist-button-add')[0]] : null;", id, members);
			if (move == null) {
				break;
			}
			WebElement item = move.get(0);
			String label = item.getText().replaceAll("\\s+", " ").trim();
			trace("    move " + label);
			click($(item));
			click($(move.get(1)));
			// Wait for the (animated) transfer to finish and be saved into the pick list's submitted input
			waitFor(d -> Boolean.TRUE.equals(js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
													"return (w.jq.find(':animated').length === 0) && " +
													"(w.targetInput.children().length === w.targetList.children('li.ui-picklist-item').length);", id)) ? Boolean.TRUE : null);
		}
		// Check the outcome, and wait for any request the transfers started
		List<String> current = pickListMembers(id);
		if ((current == null) || (! new java.util.HashSet<>(current).equals(new java.util.HashSet<>(members)))) {
			throw new IllegalStateException("The pick list " + id + " has members " + current + " instead of " + members);
		}
		waitForAjaxResponse();
		return missing;
	}

	/**
	 * Moves a member of a pick list (an ordered list membership) to another position, as a user reorders members: clicks
	 * the member, then its pick list's move up or move down button (the target controls, which Skyve shows only for an
	 * ordered membership) once for each position, waiting for each move to finish.
	 *
	 * @param id	the pick list's client id
	 * @param row	the zero-based member to move, when {@code text} is {@code null}
	 * @param text	text the label of the member to move contains (the first such member), or {@code null}
	 * @param to	the zero-based position to move it to
	 * @throws IllegalStateException if the pick list is not on the page or has no member ordering, the member is not
	 * 			found, the position is past the last member, or the member is not at the position afterwards
	 */
	@SuppressWarnings("unchecked")
	public void pickListMove(@Nonnull String id, int row, @Nullable String text, int to) {
		Map<String, Object> plan = (Map<String, Object>) js("var w = PrimeFaces.getWidgetById(arguments[0]); if ((! w) || (! w.targetList)) return null;" +
															"var label = function(li) {return (li.getAttribute('data-item-label') || li.textContent || '').replace(/\\s+/g, ' ').trim();};" +
															"var items = w.targetList.children('li.ui-picklist-item').toArray();" +
															"var up = w.jq.find('.ui-picklist-target-controls .ui-picklist-button-move-up')[0];" +
															"var down = w.jq.find('.ui-picklist-target-controls .ui-picklist-button-move-down')[0];" +
															"if ((! up) || (! down)) return {error: 'it is not an ordered membership: its pick list has no member ordering'};" +
															"var from = arguments[1], text = arguments[2], to = arguments[3];" +
															"if (text !== null) {from = -1; for (var i = 0; (i < items.length) && (from < 0); i++) {if (label(items[i]).indexOf(text) >= 0) from = i;}" +
															"  if (from < 0) return {error: 'no member\\'s label contains \\'' + text + '\\': the members are ' + items.map(label).join(' | ')};}" +
															"if ((from < 0) || (from >= items.length)) return {error: 'it has no member ' + from + ' (it has ' + items.length + ' members)'};" +
															"if (to >= items.length) return {error: 'position ' + to + ' is past its last member (it has ' + items.length + ' members)'};" +
															"return {from: from, item: items[from], label: label(items[from]), up: up, down: down};",
															id, Integer.valueOf(row), text, Integer.valueOf(to));
		if (plan == null) {
			throw new IllegalStateException("Cannot move a member of pick list " + id + ": it is not on the page");
		}
		Object error = plan.get("error");
		if (error != null) {
			throw new IllegalStateException("Cannot move a member of pick list " + id + ": " + error);
		}
		int from = ((Number) plan.get("from")).intValue();
		String label = String.valueOf(plan.get("label"));
		if (from == to) {
			trace("    '" + label + "' is already at position " + to);
			return;
		}
		click($((WebElement) plan.get("item")));
		SelenideElement button = $((WebElement) plan.get((to < from) ? "up" : "down"));
		for (int i = 0, l = Math.abs(to - from); i < l; i++) {
			click(button);
			// Waits for the pick list's (animated) move to finish and be saved into its submitted input
			waitFor(d -> Boolean.TRUE.equals(js("var w = PrimeFaces.getWidgetById(arguments[0]); return w.jq.find(':animated').length === 0;", id)) ? Boolean.TRUE : null);
		}
		waitForAjaxResponse();
		List<String> members = pickListMembers(id);
		if ((members == null) || (members.size() <= to) || (! label.equals(members.get(to)))) {
			throw new IllegalStateException("Moved '" + label + "' of pick list " + id + " to position " + to + ", but its members are " + members);
		}
	}

	/**
	 * Returns the labels of a pick list's members, in order.
	 *
	 * @param id	the pick list's client id
	 * @return the labels, or null if the pick list is not on the page
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> pickListMembers(@Nonnull String id) {
		return (List<String>) js("var w = PrimeFaces.getWidgetById(arguments[0]); if ((! w) || (! w.targetList)) return null;" +
									"return w.targetList.children('li.ui-picklist-item').toArray().map(function(li) {" +
									"  return (li.getAttribute('data-item-label') || li.textContent || '').replace(/\\s+/g, ' ').trim();});", id);
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
	 * Returns the options of a select one menu (combo), in order, from its underlying select (the empty first option
	 * included, when there is one).
	 *
	 * @param id	the select one menu's client id
	 * @return the options' [value, label] pairs, or null if there is no such menu
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<List<String>> selectOneOptions(@Nonnull String id) {
		return (List<List<String>>) js("var s = document.getElementById(arguments[0] + '_input'); if (! s) return null;" +
											"return Array.from(s.options).map(function(o) {return [o.value, (o.text || '').replace(/\\u00a0/g, ' ').trim()];});", id);
	}

	/**
	 * Returns the options of a radio group, in order.
	 *
	 * @param id	the radio group's client id
	 * @return the options' [value, label] pairs, or null if there is no such group
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<List<String>> radioOptions(@Nonnull String id) {
		return (List<List<String>>) js("if (! document.getElementById(arguments[0])) return null;" +
											"return Array.from(document.getElementsByName(arguments[0])).map(function(i) {" +
											"  var l = document.querySelector('label[for=\"' + i.id + '\"]'); return [i.value, l ? l.textContent.trim() : ''];});", id);
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

	/** The data rows of a data table (the empty message row excluded), from its client id. */
	/** Defines {@code rows}: the data rows of the grid {@code arguments[0]}, a PrimeFaces data table or a Vue list grid. */
	private static final String ROWS_JS = "var t = document.getElementById(arguments[0]); if (! t) return null;" +
											"var body = t.querySelector('tbody.ui-datatable-data') || t.querySelector('tbody.p-datatable-tbody');" +
											"var rows = body ? Array.from(body.children).filter(function(r) {" +
											"  return r.tagName === 'TR' && (! r.classList.contains('ui-datatable-empty-message')) && " +
											"    (! r.classList.contains('p-datatable-emptymessage'));}) : [];";

	/**
	 * Finds the first data row of a grid with a cell whose text contains the given text (case-sensitively).
	 *
	 * @param gridId	the data table's client id
	 * @param text	the text
	 * @return the zero-based row, -1 if no row matches, or -2 if the grid is not on the page
	 */
	public int rowContaining(@Nonnull String gridId, @Nonnull String text) {
		Number result = (Number) js(ROWS_JS +
									"for (var i = 0; i < rows.length; i++) {" +
									"  var cells = rows[i].querySelectorAll('td');" +
									"  for (var j = 0; j < cells.length; j++) {if ((cells[j].textContent || '').indexOf(arguments[1]) >= 0) return i;}" +
									"}" +
									"return -1;", gridId, text);
		return (result == null) ? -2 : result.intValue();
	}

	/**
	 * Drags a row of an ordered data grid by its drag handle to another position, as a user reorders rows: presses the
	 * mouse on the handle, moves it over the row at the position (its upper half to drop the row before it when moving
	 * up, its lower half to drop it after it when moving down) and releases it. PrimeFaces then sends the move to the
	 * server ({@code rowReorder}), which reorders the collection and numbers its ordinals.
	 *
	 * @param gridId	the data table's client id
	 * @param from		the zero-based row to move
	 * @param to		the zero-based position to move it to
	 * @throws IllegalStateException if the grid is not on the page or not ordered, a row is out of range, or the row
	 * 			is not at the position after the drag
	 */
	public void dataGridMove(@Nonnull String gridId, int from, int to) {
		@SuppressWarnings("unchecked")
		Map<String, Object> plan = (Map<String, Object>) js(ROWS_JS + ROW_SIGNATURE_JS +
											"if (! body || ! body.classList.contains('ui-sortable')) return {error: 'it is not an ordered grid, so its rows cannot be dragged'};" +
											"var n = rows.length, f = arguments[1], to = arguments[2];" +
											"if (f < 0 || f >= n) return {error: 'it has no row ' + f + ' (it has ' + n + ' rows)'};" +
											"if (to >= n) return {error: 'position ' + to + ' is past its last row (it has ' + n + ' rows)'};" +
											"var handle = rows[f].querySelector('.fa-sort') || rows[f].querySelector('td');" +
											"return {handle: handle, target: rows[to], signature: signature(rows[f])};",
											gridId, Integer.valueOf(from), Integer.valueOf(to));
		if (plan == null) {
			throw new IllegalStateException("Cannot move row " + from + " of " + gridId + ": the grid is not on the page");
		}
		Object error = plan.get("error");
		if (error != null) {
			throw new IllegalStateException("Cannot move row " + from + " of " + gridId + ": " + error);
		}
		if (from == to) {
			trace("    Row " + from + " is already at position " + to);
			return;
		}
		WebElement handle = (WebElement) plan.get("handle");
		WebElement target = (WebElement) plan.get("target");
		scrollToCentre(target);
		scrollToCentre(handle);
		waitUntilUncovered(handle);
		int quarter = Math.max(1, target.getSize().getHeight() / 4);
		int direction = (to < from) ? -1 : 1;
		// Press on the handle, move past jQuery UI's drag threshold so that the sort starts, then over the half of the
		// target row that puts the row on the far side of it, and release there. Each move is a pointer event that the
		// sortable tracks.
		new org.openqa.selenium.interactions.Actions(driver)
				.moveToElement(handle)
				.clickAndHold()
				.moveByOffset(0, 5 * direction)
				.moveByOffset(0, 5 * direction)
				.moveToElement(target, 0, quarter * direction)
				.moveByOffset(0, direction)
				.release()
				.perform();
		// Waits for the rowReorder ajax request the drop sends, and its response
		waitForAjaxResponse();
		Object moved = js(ROWS_JS + ROW_SIGNATURE_JS + "var r = rows[arguments[1]]; return r ? signature(r) : null;", gridId, Integer.valueOf(to));
		if (! String.valueOf(plan.get("signature")).equals(moved)) {
			throw new IllegalStateException("Dragged row " + from + " of " + gridId + " to position " + to + ", but the grid does not show it there");
		}
	}

	/** Defines {@code signature(row)}: what a row shows, its cells' text and its inputs' values. */
	private static final String ROW_SIGNATURE_JS = "var signature = function(r) {return Array.from(r.querySelectorAll('td')).map(function(c) {" +
													"  return (c.textContent || '') + Array.from(c.querySelectorAll('input,textarea,select')).map(function(i) {return i.value;}).join('|');" +
													"}).join('||');};";

	/**
	 * Counts the data rows of a grid, optionally only those with a cell containing the text.
	 *
	 * @param gridId	the data table's client id
	 * @param text	the text, or null to count every row
	 * @return the count, or -1 if the grid is not on the page
	 */
	public int countRows(@Nonnull String gridId, @Nullable String text) {
		Number result = (Number) js(ROWS_JS +
									"var wanted = arguments[1];" +
									"if (wanted === null) return rows.length;" +
									"return rows.filter(function(r) {return Array.from(r.querySelectorAll('td')).some(function(c) {" +
									"  return (c.textContent || '').indexOf(wanted) >= 0;});}).length;", gridId, text);
		return (result == null) ? -1 : result.intValue();
	}

	/**
	 * Asks a grid (a PrimeFaces data table, or the panel around one) to load its rows from the server again, with its
	 * current filters. A data repeater (no data table widget) is left as it is.
	 *
	 * @param gridId	the data table's client id
	 */
	public void refreshGrid(@Nonnull String gridId) {
		if (isVueListGrid(gridId)) {
			// A Vue list grid refetches its rows quietly when it is told it was rendered again (as after an ajax update)
			js("var app = window[arguments[0]]; if (app && app.skyveConfig) app.skyveConfig.renderedAt = Date.now();", gridId);
			// Wait for the refetch to start (its spinner shows from the fetch's debounce), then for its rows
			try {
				waitFor(d -> Boolean.TRUE.equals(js("var e = document.getElementById(arguments[0]);" +
													"return !! (e && e.querySelector('.skyve-vue-list-grid-spinner-active'));", gridId)) ? Boolean.TRUE : null);
			}
			catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException e) {
				// it has fetched already
			}
			waitForAjaxResponse();
			return;
		}
		// The grid's id can be a panel around the data table: reload the data table (filter() fetches it from the server)
		js("var e = document.getElementById(arguments[0]); if (! e) return;" +
				"var t = e.classList.contains('ui-datatable') ? e : e.querySelector('.ui-datatable');" +
				"var w = t ? PrimeFaces.getWidgetById(t.id) : null;" +
				"if (w && (typeof w.filter === 'function')) w.filter();", gridId);
		waitForAjaxResponse();
	}

	/**
	 * Defines {@code grid} (the Vue list grid in the container {@code arguments[0]}), {@code col} (its column definition
	 * with the field {@code arguments[1]}) and {@code th} (the header cell of that column, found by its title, or null when
	 * the column is not shown). Returns null if there is no such grid.
	 */
	private static final String VUE_COLUMN_JS = "var c = document.getElementById(arguments[0]); var grid = c ? c.querySelector('.skyve-vue-list-grid') : null;" +
													"if (! grid) return null;" +
													"var app = window[arguments[0]]; var field = arguments[1];" +
													"var col = ((app && app.skyveConfig) ? app.skyveConfig.columns : []).find(function(x) {return x.field === field;}) || null;" +
													"var th = col ? (Array.from(grid.querySelectorAll('.p-datatable-thead th')).find(function(h) {" +
													"  var t = h.querySelector('.p-column-title'); return t && (t.textContent.trim() === String(col.header).trim());}) || null) : null;";

	/**
	 * Describes a column of a Vue list grid, from the grid's column definitions.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @param field	the column's field (its sanitised binding)
	 * @return [type, title, shown, filterable, sortable, options], where type is the grid's column type (such as text,
	 * 			numeric or enum), shown is false when the column is not in the grid's shown columns, and options are an enum
	 * 			column's [code, label] pairs (empty for other columns); or null if the grid has no such column (or is not on
	 * 			the page)
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<Object> vueListGridColumn(@Nonnull String gridId, @Nonnull String field) {
		return (List<Object>) js(VUE_COLUMN_JS + "if (! col) return null;" +
									"var options = (col.enumValues || []).map(function(v) {return [String(v.value), String(v.label)];});" +
									"return [String(col.type), String(col.header), !! th, !! col.filterable, !! col.sortable, options];", gridId, field);
	}

	/**
	 * How a value is entered in a Vue list grid's column filter menu.
	 */
	public enum VueFilterInput {
		/** Typed into a text input (text and numeric columns). */
		text,
		/** Chosen from a drop down by its label (enum columns). */
		option,
		/** A tri-state check box clicked until it shows True or False (boolean columns). */
		check,
		/** Typed into a calendar's input in the calendar's format, then Enter (date, date time, time and timestamp columns). */
		calendar
	}

	/**
	 * Filters a column of a Vue list grid as a user does, in the column's filter menu: chooses the match mode, types the
	 * value and presses Apply. A filter already on the column is cleared first (its menu's Clear), so the new one replaces
	 * it. With no value, the column's filter is cleared. Waits for each fetch the grid makes.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @param field	the column's field (its sanitised binding); the column must be shown and filterable
	 * @param matchMode	the label of the match mode to choose, such as Contains; ignored when the value is null
	 * @param value	the value as the filter's input takes it (see {@link VueFilterInput}): the text to type, the option's
	 * 				label, true or false, or the date or time in the calendar's format; or null to clear the column's filter
	 * @param input	how the value is entered
	 * @throws IllegalStateException	if the column has no filter menu, or its menu lacks the match mode or the input,
	 * 									or the input does not take the value, or the column does not show a filter afterwards
	 */
	public void vueListGridFilter(@Nonnull String gridId,
									@Nonnull String field,
									@Nullable String matchMode,
									@Nullable String value,
									@Nonnull VueFilterInput input) {
		waitForAjaxResponse();
		WebElement button = vueFilterButton(gridId, field);
		if (vueFilterActive(button)) {
			trace("    clear the column's filter");
			clearVueFilter(button);
			if (value == null) {
				return;
			}
			button = vueFilterButton(gridId, field);
		}
		else if (value == null) {
			trace("    the column has no filter to clear");
			return;
		}
		if (Boolean.TRUE.equals(js("var c = document.getElementById(arguments[0]);" +
										"return Array.from(c.querySelectorAll('.skyve-vue-list-grid-toolbar .p-dropdown-label')).some(function(l) {" +
										"  return l.textContent.trim() === 'Match Any';});", gridId))) {
			throw new IllegalStateException(String.format("List grid %s matches any of its filters (its toolbar shows Match Any), so a filter would not be ANDed with the others",
															gridId));
		}

		String overlayId = openVueFilterOverlay(button);
		WebElement dropdown = (WebElement) js("var o = document.getElementById(arguments[0]);" +
												"return o.querySelector('.p-column-filter-constraint .p-column-filter-matchmode-dropdown');", overlayId);
		if (dropdown == null) {
			throw new IllegalStateException(String.format("The filter menu of column %s of list grid %s offers no match modes", field, gridId));
		}
		String shownMode = dropdown.findElement(By.cssSelector(".p-dropdown-label")).getText().trim();
		if (! shownMode.equals(matchMode)) {
			trace("    choose " + matchMode);
			chooseVueDropdownOption(dropdown, Objects.requireNonNull(matchMode));
		}
		enterVueFilterValue(overlayId, String.format("column %s of list grid %s", field, gridId), value, input);
		WebElement apply = (WebElement) js("return document.getElementById(arguments[0]).querySelector('[data-pc-name=filterapplybutton]');", overlayId);
		long fetches = markVueListGridFetch();
		click($(apply));
		// Wait for Apply to close the filter menu (after its closing transition)
		waitFor(d -> (js("return document.getElementById(arguments[0]);", overlayId) == null) ? Boolean.TRUE : null);
		waitForVueListGridFetch(fetches);
		if (! vueFilterActive(vueFilterButton(gridId, field))) {
			throw new IllegalStateException(String.format("Column %s of list grid %s does not show a filter after Apply", field, gridId));
		}
	}

	/**
	 * Enters the value into the first constraint of an open filter menu.
	 *
	 * @param overlayId	the filter menu's id
	 * @param what	the column, for the messages
	 * @param value	the value as the input takes it
	 * @param input	how the value is entered
	 * @throws IllegalStateException	if the menu has no such input, or the input does not take the value
	 */
	private void enterVueFilterValue(@Nonnull String overlayId, @Nonnull String what, @Nonnull String value, @Nonnull VueFilterInput input) {
		String selector = switch (input) {
			case text -> "input.p-column-filter";
			case option -> ".p-dropdown:not(.p-column-filter-matchmode-dropdown)";
			case check -> ".p-tristatecheckbox";
			case calendar -> ".p-calendar input";
		};
		WebElement element = (WebElement) js("return document.getElementById(arguments[0]).querySelector('.p-column-filter-constraint ' + arguments[1]);",
												overlayId, selector);
		if (element == null) {
			throw new IllegalStateException(String.format("The filter menu of %s has no %s input", what, input));
		}
		switch (input) {
			case text -> {
				trace("    type " + value);
				click($(element));
				element.sendKeys(Keys.chord(selectAllKey(), "a"));
				element.sendKeys(value);
			}
			case option -> {
				trace("    choose " + value);
				chooseVueDropdownOption(element, value);
			}
			case check -> {
				// The check box cycles through Not Selected, True and False; its status says which it shows
				String wanted = Boolean.parseBoolean(value) ? "True" : "False";
				for (int clicks = 0; clicks <= 3; clicks++) {
					String shown = String.valueOf(js("var s = arguments[0].querySelector('[role=status]'); return s ? s.textContent.trim() : '';", element));
					if (wanted.equals(shown)) {
						break;
					}
					if (clicks == 3) {
						throw new IllegalStateException(String.format("The filter check box of %s shows %s, not %s, after 3 clicks", what, shown, wanted));
					}
					trace("    click the check box (it shows " + shown + ")");
					click($(element));
				}
			}
			case calendar -> {
				trace("    type " + value + " into the calendar, then Enter");
				click($(element));
				element.sendKeys(Keys.chord(selectAllKey(), "a"));
				element.sendKeys(value, Keys.ENTER);
				// Wait for the calendar's panel to close: Enter closes it once the typed text is a valid date or time
				try {
					waitFor(d -> (js("return document.querySelector('.p-datepicker:not(.p-datepicker-inline)');") == null) ? Boolean.TRUE : null);
				}
				catch (org.openqa.selenium.TimeoutException e) {
					throw new IllegalStateException(String.format("The filter calendar of %s did not take '%s' (it shows '%s')",
																	what, value, element.getDomProperty("value")), e);
				}
			}
		}
	}

	/**
	 * Clears every column filter of a Vue list grid as a user does: Clear in each filtered column's filter menu.
	 * Waits for each fetch the grid makes.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @return the number of filters cleared
	 */
	public int vueListGridClearFilters(@Nonnull String gridId) {
		int result = 0;
		for (int i = 0; i < 1000; i++) {
			waitForAjaxResponse();
			WebElement button = (WebElement) js("var c = document.getElementById(arguments[0]); var grid = c ? c.querySelector('.skyve-vue-list-grid') : null;" +
												"return grid ? grid.querySelector('.p-datatable-thead .p-column-filter-menu-button-active') : null;", gridId);
			if (button == null) {
				break;
			}
			clearVueFilter(button);
			result++;
		}
		return result;
	}

	/**
	 * Sorts a Vue list grid by a column alone, as a user does: clicks the column's title until the column shows the
	 * direction and no other column shows a sort (a click without a modifier key sorts by that column alone).
	 * Waits for each fetch the grid makes.
	 *
	 * @param gridId	the grid's client id (its container)
	 * @param field	the column's field (its sanitised binding); the column must be shown and sortable
	 * @param descending	true for descending, false for ascending
	 * @throws IllegalStateException	if the column is not shown, or does not show the sort after three clicks
	 */
	@SuppressWarnings("unchecked")
	public void vueListGridSort(@Nonnull String gridId, @Nonnull String field, boolean descending) {
		String wanted = descending ? "descending" : "ascending";
		for (int clicks = 0; clicks <= 3; clicks++) {
			waitForAjaxResponse();
			// [the column's title, its sort, whether another column shows a sort]
			List<Object> state = (List<Object>) js(VUE_COLUMN_JS + "if (! th) return null;" +
														"var others = Array.from(grid.querySelectorAll('.p-datatable-thead th')).some(function(h) {" +
														"  var s = h.getAttribute('aria-sort'); return (h !== th) && s && (s !== 'none');});" +
														"return [th.querySelector('.p-column-title'), th.getAttribute('aria-sort'), others];", gridId, field);
			if (state == null) {
				throw new IllegalStateException(String.format("Column %s of list grid %s is not shown", field, gridId));
			}
			if (wanted.equals(state.get(1)) && (! Boolean.TRUE.equals(state.get(2)))) {
				return;
			}
			if (clicks == 3) {
				break;
			}
			trace("    click the column's title (it shows " + state.get(1) + ")");
			long fetches = markVueListGridFetch();
			click($((WebElement) state.get(0)));
			waitForVueListGridFetch(fetches);
		}
		throw new IllegalStateException(String.format("Column %s of list grid %s does not show a sort %s, alone, after 3 clicks on its title",
														field, gridId, wanted));
	}

	/**
	 * Finds the filter menu button of a shown column of a Vue list grid.
	 *
	 * @throws IllegalStateException	if the column is not shown or has no filter menu
	 */
	private @Nonnull WebElement vueFilterButton(@Nonnull String gridId, @Nonnull String field) {
		WebElement result = (WebElement) js(VUE_COLUMN_JS + "return th ? th.querySelector('.p-column-filter-menu-button') : null;", gridId, field);
		if (result == null) {
			throw new IllegalStateException(String.format("Column %s of list grid %s is not shown or has no filter menu", field, gridId));
		}
		return result;
	}

	private static boolean vueFilterActive(@Nonnull WebElement filterButton) {
		String styleClass = filterButton.getDomAttribute("class");
		return (styleClass != null) && styleClass.contains("p-column-filter-menu-button-active");
	}

	/**
	 * Opens a column's filter menu with its button.
	 *
	 * @return the menu's (overlay's) id
	 */
	private @Nonnull String openVueFilterOverlay(@Nonnull WebElement filterButton) {
		String result = Objects.requireNonNull(filterButton.getDomAttribute("aria-controls"), "filter menu id");
		click($(filterButton));
		// Wait for the filter menu to open and its opening transition to end
		waitFor(d -> js("var o = document.getElementById(arguments[0]);" +
							"return (o && (o.getClientRects().length > 0) && (! /-enter-/.test(o.className))) ? true : null;", result));
		return result;
	}

	/**
	 * Clears a column's filter with Clear in its filter menu, and waits for the grid's fetch.
	 */
	private void clearVueFilter(@Nonnull WebElement filterButton) {
		String overlayId = openVueFilterOverlay(filterButton);
		WebElement clear = (WebElement) js("return document.getElementById(arguments[0]).querySelector('[data-pc-name=filterclearbutton]');", overlayId);
		long fetches = markVueListGridFetch();
		click($(clear));
		// Wait for Clear to close the filter menu (after its closing transition)
		waitFor(d -> (js("return document.getElementById(arguments[0]);", overlayId) == null) ? Boolean.TRUE : null);
		waitForVueListGridFetch(fetches);
	}

	/**
	 * Chooses an option of a PrimeVue drop down by its label, as a user does: opens it and clicks the option.
	 *
	 * @throws IllegalStateException	if the drop down has no option with the label
	 */
	private void chooseVueDropdownOption(@Nonnull WebElement dropdown, @Nonnull String label) {
		WebElement trigger = dropdown.findElement(By.cssSelector(".p-dropdown-label"));
		String listId = Objects.requireNonNull(trigger.getDomAttribute("aria-controls"), "drop down list id");
		click($(trigger));
		// Wait for the drop down's options to show and its opening transition to end: the option, or the labels if none matches
		Object option = waitFor(d -> js("var l = document.getElementById(arguments[0]); var p = l ? l.closest('.p-dropdown-panel') : null;" +
											"if ((! p) || (p.getClientRects().length === 0) || /-enter-/.test(p.className)) return null;" +
											"var items = Array.from(l.querySelectorAll('li.p-dropdown-item')); var wanted = arguments[1];" +
											"return items.find(function(i) {return (i.getAttribute('aria-label') || i.textContent.trim()) === wanted;}) || " +
											"  items.map(function(i) {return i.textContent.trim();}).join(', ');", listId, label));
		if (! (option instanceof WebElement element)) {
			throw new IllegalStateException(String.format("The drop down has no option %s: its options are %s", label, option));
		}
		click($(element));
		// Wait for the drop down's options to close (after their closing transition)
		waitFor(d -> (js("return document.getElementById(arguments[0]);", listId) == null) ? Boolean.TRUE : null);
		String shown = trigger.getText().trim();
		if (! label.equals(shown)) {
			throw new IllegalStateException(String.format("The drop down shows %s after choosing %s", shown, label));
		}
	}

	/**
	 * Counts the list fetches (smartlist requests) the page has completed, counting from the first call on the page,
	 * after waiting for the page to be idle. Pass the result to {@link #waitForVueListGridFetch(long)} after a gesture.
	 *
	 * @return the number of list fetches completed so far
	 */
	private long markVueListGridFetch() {
		waitForAjaxResponse();
		// A resource timing entry is made for each fetch when its response has been received
		Number result = (Number) js("if (! window.sailListFetchObserver) {" +
										"  window.sailListFetches = 0;" +
										"  window.sailListFetchObserver = new PerformanceObserver(function(list) {list.getEntries().forEach(function(e) {" +
										"    if (/\\/smartlist(\\?|$)/.test(e.name)) window.sailListFetches++;});});" +
										"  window.sailListFetchObserver.observe({type: 'resource'});" +
										"}" +
										"return window.sailListFetches;");
		return (result == null) ? 0 : result.longValue();
	}

	/**
	 * Waits for a Vue list grid's fetch after a gesture: a list fetch completed since the mark, and no grid shows its
	 * spinner (it shows from the gesture until the fetched rows are shown).
	 *
	 * @param mark	the count from {@link #markVueListGridFetch()} before the gesture
	 */
	private void waitForVueListGridFetch(long mark) {
		// Wait for the grid's fetch to complete and its rows to be shown
		waitFor(d -> Boolean.TRUE.equals(js("return (window.sailListFetches > arguments[0]) && " +
												"(! document.querySelector('.skyve-vue-list-grid-spinner-active'));", Long.valueOf(mark))) ? Boolean.TRUE : null);
		waitForAjaxResponse();
	}

	/**
	 * Filters a column of a PrimeFaces data table as a user does: shows the filters with the table's filter toggle if
	 * they are hidden, then types the value into the column's filter, replacing its text. With no value, the filter's
	 * text is deleted. Waits for the table's filter request (sent after its filter delay) and its response.
	 *
	 * @param columnId	the column's client id (its header cell)
	 * @param toggleId	the client id of the table's filter toggle, or null if it has none
	 * @param value	the value, or null to clear the column's filter
	 * @throws IllegalStateException	if the column has no text filter, or it is hidden and cannot be shown
	 */
	public void facesListGridFilter(@Nonnull String columnId, @Nullable String toggleId, @Nullable String value) {
		waitForAjaxResponse();
		WebElement input = facesFilterInput(columnId, toggleId);
		String current = input.getDomProperty("value");
		String wanted = (value == null) ? "" : value;
		if (wanted.equals(current)) {
			trace("    the column's filter is already '" + wanted + "'");
			return;
		}
		typeFacesFilter(input, value);
	}

	/**
	 * Filters a column of a PrimeFaces data table by choosing an option of its combo filter (a select one menu), as a
	 * user does, showing the filters with the table's filter toggle first if they are hidden. Waits for the filter
	 * request the choice sends, and its response.
	 *
	 * @param menuId	the combo filter's client id
	 * @param toggleId	the client id of the table's filter toggle, or null if it has none
	 * @param index	the zero-based option, the empty first option (0) clearing the filter
	 * @throws IllegalStateException	if the combo cannot be shown or chosen from
	 */
	public void facesListGridChooseFilter(@Nonnull String menuId, @Nullable String toggleId, int index) {
		waitForAjaxResponse();
		showFacesFilter(menuId, toggleId);
		List<String> current = selectOneValues(menuId);
		if ((current != null) && String.valueOf(index).equals(current.get(0))) {
			trace("    the column's filter shows that option already");
			return;
		}
		markPage();
		if (! selectOne(new PrimeFacesSelectOneCommand(menuId, index))) {
			throw new IllegalStateException(String.format("The combo filter %s is disabled", menuId));
		}
		// Wait for the filter request that the combo's change sends, then for its response
		waitFor(d -> requestSent() ? Boolean.TRUE : null);
		waitForAjaxResponse();
	}

	/**
	 * Filters a column of a PrimeFaces data table with its tri-state check box filter, as a user does: clicks it until it
	 * shows the value (it cycles through unset, checked and crossed), showing the filters with the table's filter toggle
	 * first if they are hidden. Waits for the filter request each click sends, and its response.
	 *
	 * @param checkId	the check box filter's client id
	 * @param toggleId	the client id of the table's filter toggle, or null if it has none
	 * @param value	true (checked), false (crossed), or null to clear the filter (unset)
	 * @throws IllegalStateException	if the check box does not show the value after three clicks
	 */
	public void facesListGridCheckFilter(@Nonnull String checkId, @Nullable String toggleId, @Nullable Boolean value) {
		waitForAjaxResponse();
		showFacesFilter(checkId, toggleId);
		// the check box's input holds 0 (unset), 1 (checked) or 2 (crossed)
		String wanted = (value == null) ? "0" : (value.booleanValue() ? "1" : "2");
		for (int clicks = 0; clicks <= 3; clicks++) {
			String shown = inputValue(checkId + "_input");
			if (wanted.equals(shown)) {
				return;
			}
			if (clicks == 3) {
				break;
			}
			trace("    click the check box filter");
			WebElement box = Objects.requireNonNull(elementById(checkId)).findElement(By.cssSelector(".ui-chkbox-box"));
			markPage();
			click($(box));
			// Wait for the filter request that the check box's change sends, then for its response
			waitFor(d -> requestSent() ? Boolean.TRUE : null);
			waitForAjaxResponse();
		}
		throw new IllegalStateException(String.format("The check box filter %s does not show %s after 3 clicks", checkId, value));
	}

	/**
	 * Clears every column filter of a PrimeFaces data table as a user does: deletes the text of each filter that has
	 * some, chooses the empty option of each combo filter and unsets each check box filter, showing the filters with
	 * the table's filter toggle first if they are hidden. Waits for each filter request.
	 *
	 * @param gridId	the data table's client id
	 * @param toggleId	the client id of the table's filter toggle, or null if it has none
	 * @return the number of filters cleared
	 */
	public int facesListGridClearFilters(@Nonnull String gridId, @Nullable String toggleId) {
		int result = 0;
		for (int i = 0; i < 1000; i++) {
			waitForAjaxResponse();
			// [kind, id] of the first filter with a value: a text filter's column, a combo filter or a check box filter
			@SuppressWarnings("unchecked")
			List<String> filter = (List<String>) js("var t = document.getElementById(arguments[0]); if (! t) return null;" +
														"var i = Array.from(t.querySelectorAll('thead th input.ui-column-filter')).find(function(x) {return x.value !== '';});" +
														"if (i) return ['text', i.closest('th').id];" +
														"var s = Array.from(t.querySelectorAll('thead th .ui-column-customfilter select')).find(function(x) {return x.value !== '';});" +
														"if (s) return ['combo', s.id.replace(/_input$/, '')];" +
														"var c = Array.from(t.querySelectorAll('thead th .ui-column-customfilter .ui-chkbox input')).find(function(x) {return x.value !== '0';});" +
														"return c ? ['check', c.id.replace(/_input$/, '')] : null;", gridId);
			if (filter == null) {
				break;
			}
			String id = filter.get(1);
			switch (filter.get(0)) {
				case "text" -> typeFacesFilter(facesFilterInput(id, toggleId), null);
				case "combo" -> facesListGridChooseFilter(id, toggleId, 0);
				default -> facesListGridCheckFilter(id, toggleId, null);
			}
			result++;
		}
		return result;
	}

	/**
	 * Sorts a PrimeFaces data table by a column alone, as a user does: clicks the column's title until the column shows
	 * the direction and no other column shows a sort (a click without a modifier key sorts by that column alone).
	 * Waits for each sort request.
	 *
	 * @param gridId	the data table's client id
	 * @param columnId	the column's client id (its header cell)
	 * @throws IllegalStateException	if the column is not on the page, the table has no rows (PrimeFaces does not sort
	 * 									an empty table), or the column does not show the sort after three clicks
	 */
	@SuppressWarnings("unchecked")
	public void facesListGridSort(@Nonnull String gridId, @Nonnull String columnId, boolean descending) {
		String wanted = descending ? "ui-icon-triangle-1-s" : "ui-icon-triangle-1-n";
		for (int clicks = 0; clicks <= 3; clicks++) {
			waitForAjaxResponse();
			// [the column's title, whether it is sorted the wanted way, whether another column shows a sort, the row count]
			List<Object> state = (List<Object>) js(ROWS_JS + "var h = document.getElementById(arguments[1]); if (! h) return null;" +
														"var icon = h.querySelector('.ui-sortable-column-icon');" +
														"var sorted = h.classList.contains('ui-state-active') && !! icon && icon.classList.contains(arguments[2]);" +
														"var others = Array.from(t.querySelectorAll('thead th.ui-sortable-column')).some(function(x) {" +
														"  return (x !== h) && x.classList.contains('ui-state-active');});" +
														"return [h.querySelector('.ui-column-title'), sorted, others, rows.length];", gridId, columnId, wanted);
			if ((state == null) || (state.get(0) == null)) {
				throw new IllegalStateException(String.format("Column %s of data table %s is not on the page or cannot be sorted", columnId, gridId));
			}
			if (Boolean.TRUE.equals(state.get(1)) && (! Boolean.TRUE.equals(state.get(2)))) {
				return;
			}
			if (clicks == 3) {
				break;
			}
			if (((Number) state.get(3)).intValue() == 0) {
				throw new IllegalStateException(String.format("Data table %s has no rows: PrimeFaces does not sort an empty data table", gridId));
			}
			trace(Boolean.TRUE.equals(state.get(2)) ? "    click the column's title (another column shows a sort)" : "    click the column's title");
			markPage();
			click($((WebElement) state.get(0)));
			// Wait for the sort request to be sent, then for its response
			waitFor(d -> requestSent() ? Boolean.TRUE : null);
			waitForAjaxResponse();
		}
		throw new IllegalStateException(String.format("Column %s of data table %s does not show a sort %s, alone, after 3 clicks on its title",
														columnId, gridId, descending ? "descending" : "ascending"));
	}

	/**
	 * Finds a data table column's text filter, showing the table's filters with its toggle first if they are hidden.
	 */
	private @Nonnull WebElement facesFilterInput(@Nonnull String columnId, @Nullable String toggleId) {
		WebElement result = elementById(columnId + ":filter");
		if ((result == null) || (! "input".equalsIgnoreCase(result.getTagName()))) {
			throw new IllegalStateException(String.format("Column %s of the data table has no text filter", columnId));
		}
		showFacesFilter(columnId + ":filter", toggleId);
		return result;
	}

	/**
	 * Shows a data table's filters with its filter toggle, if the filter element is hidden.
	 *
	 * @throws IllegalStateException	if the filter is not on the page, or is hidden and the table has no toggle
	 */
	private void showFacesFilter(@Nonnull String filterId, @Nullable String toggleId) {
		WebElement filter = elementById(filterId);
		if (filter == null) {
			throw new IllegalStateException(String.format("The data table filter %s is not on the page", filterId));
		}
		if (! filter.isDisplayed()) {
			WebElement toggle = (toggleId == null) ? null : elementById(toggleId);
			if ((toggle == null) || (! toggle.isDisplayed())) {
				throw new IllegalStateException(String.format("The data table filter %s is hidden and the table has no filter toggle to show it", filterId));
			}
			trace("    show the filters");
			click($(toggle));
			// Wait for the toggle to show the filters
			waitFor(d -> filter.isDisplayed() ? Boolean.TRUE : null);
		}
	}

	/**
	 * Replaces the text of a data table's column filter (or deletes it, for a null value) with key presses, so that
	 * the table's key up handler filters, then waits for the filter request and its response.
	 */
	private void typeFacesFilter(@Nonnull WebElement input, @Nullable String value) {
		trace((value == null) ? "    delete the filter's text" : "    type " + value);
		scrollToCentre(input);
		waitUntilUncovered(input);
		markPage();
		// Select the text and type over it: clear() would fire a change event, and a filter request, of its own
		click($(input));
		input.sendKeys(Keys.chord(selectAllKey(), "a"));
		input.sendKeys((value == null) ? Keys.BACK_SPACE.toString() : value);
		// Wait for the filter request, which the data table sends after its filter delay, then for its response
		waitFor(d -> requestSent() ? Boolean.TRUE : null);
		waitForAjaxResponse();
	}

	/**
	 * Returns the text of the messages (global, growl and field) of a severity.
	 *
	 * @param severity	info, warning or error (error includes fatal), or null for every severity
	 * @return the messages; empty when there are none
	 */
	public @Nonnull List<String> messagesOfType(@Nullable String severity) {
		if (severity == null) {
			return messages(ALL_MESSAGES_SELECTOR);
		}
		String pf = "warning".equals(severity) ? "warn" : severity;
		StringBuilder selector = new StringBuilder(128);
		selector.append(".ui-messages-").append(pf).append(", .ui-growl-").append(pf).append(", .ui-message-").append(pf);
		if ("error".equals(severity)) {
			selector.append(", .ui-messages-fatal, .ui-growl-fatal, .ui-message-fatal");
		}
		return messages(selector.toString());
	}

	/**
	 * Describes whether an element is on the page, shown and enabled.
	 * An element is enabled unless it, its {@code _input} or an ancestor within its widget is disabled.
	 *
	 * @param id	the element's id
	 * @return {shown, enabled}; shown is false when the element is not on the page
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<Boolean> widgetState(@Nonnull String id) {
		return (List<Boolean>) js("var e = document.getElementById(arguments[0]) || document.getElementById(arguments[0] + '_input');" +
									"if (! e) return [false, false];" +
									"var shown = (e.offsetParent !== null) || (e.getClientRects().length > 0);" +
									"if ((! shown) && e.type === 'hidden') {var p = e.parentElement; shown = !! p && ((p.offsetParent !== null) || (p.getClientRects().length > 0));}" +
									"var input = document.getElementById(arguments[0] + '_input') || e;" +
									"var disabled = e.disabled || input.disabled || e.classList.contains('ui-state-disabled') || " +
									"  !! e.querySelector('.ui-state-disabled:not(.ui-fileupload-upload):not(.ui-fileupload-cancel)') || " +
									"  input.readOnly && (input.tagName === 'INPUT' || input.tagName === 'TEXTAREA') && input.type !== 'hidden' && ! input.classList.contains('ui-chkbox') && ! /_input$/.test(input.id);" +
									"return [shown, shown && ! disabled];", id);
	}

	/**
	 * Describes whether a tab's header is shown and enabled.
	 *
	 * @param panelId	the tab panel's client id
	 * @return {shown, enabled}
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<Boolean> tabState(@Nonnull String panelId) {
		return (List<Boolean>) js("var href = '#' + arguments[0], a = null;" +
									"var as = document.querySelectorAll('a[href]'); for (var i = 0; i < as.length; i++) {if (as[i].getAttribute('href') === href) {a = as[i]; break;}}" +
									"if (! a) return [false, false];" +
									"var li = a.closest('li') || a;" +
									"var shown = (li.offsetParent !== null) || (li.getClientRects().length > 0);" +
									"return [shown, shown && ! li.classList.contains('ui-state-disabled')];", panelId);
	}

	/**
	 * Finds a menu entry: a module's header, or an item under optional groups.
	 *
	 * @param moduleTitle	the module's title as shown
	 * @param groups	the group names as shown, outermost first (may be empty)
	 * @param item	the item name as shown, or null for the module itself
	 * @param open	true to expand the module and groups and click the item
	 * @return true if the entry is in the menu
	 */
	public boolean menu(@Nonnull String moduleTitle, @Nonnull List<String> groups, @Nullable String item, boolean open) {
		String find = "var title = arguments[0], groups = arguments[1], item = arguments[2];" +
						"var text = function(e) {return (e.textContent || '').replace(/\\s+/g, ' ').trim();};" +
						"var header = Array.from(document.querySelectorAll('.ui-panelmenu-header')).find(function(h) {return text(h) === title;});" +
						"if (! header) return null;" +
						"if (arguments[3] === 'header') return header;" +
						"var scope = header.nextElementSibling;" +
						"for (var g = 0; g < groups.length; g++) {" +
						"  var parent = Array.from(scope.querySelectorAll('li.ui-menu-parent')).find(function(li) {" +
						"    var a = li.querySelector(':scope > a'); return a && text(a) === groups[g];});" +
						"  if (! parent) return null;" +
						"  if (arguments[3] === 'group' + g) return parent.querySelector(':scope > a');" +
						"  scope = parent.querySelector(':scope > ul');" +
						"}" +
						"if (item === null) return header;" +
						"return Array.from(scope.querySelectorAll('li.ui-menuitem:not(.ui-menu-parent) > a.ui-menuitem-link')).find(function(a) {return text(a) === item;}) || null;";
		WebElement target = (WebElement) js(find, moduleTitle, groups, item, "item");
		if ((target == null) || (! open)) {
			return (target != null);
		}
		// A small window hides the menu in a side bar: open it as a user would
		WebElement toggle = (WebElement) js("var s = document.getElementById('sidebar');" +
												"return (s && s.classList.contains('inactive')) ? s.querySelector('a.toggle') : null;");
		if (toggle != null) {
			trace("    open the side bar menu");
			clickToggle(toggle);
			// Wait for the side bar to slide fully into the window
			waitFor(d -> js("var s = document.getElementById('sidebar'), r = s.getBoundingClientRect();" +
								"return ((! s.classList.contains('inactive')) && (r.left >= 0)) ? true : null;"));
		}
		// Expand the module and each group, then click the item
		WebElement header = (WebElement) js(find, moduleTitle, groups, item, "header");
		// The menu can restore its expanded panels, so look at what is open rather than at the markup's state
		if ((header != null) && Boolean.TRUE.equals(js("var c = arguments[0].nextElementSibling; return (! c) || getComputedStyle(c).display === 'none';", header))) {
			click($(header));
			// Wait for the module's panel to finish sliding open
			waitFor(d -> js("var c = arguments[0].nextElementSibling;" +
								"return (c && (! jQuery(c).is(':animated')) && getComputedStyle(c).display !== 'none') ? true : null;", header));
		}
		for (int g = 0, l = groups.size(); g < l; g++) {
			WebElement group = (WebElement) js(find, moduleTitle, groups, item, "group" + g);
			if ((group != null) && Boolean.TRUE.equals(js("var u = arguments[0].parentElement.querySelector(':scope > ul');" +
															"return (! u) || getComputedStyle(u).display === 'none';", group))) {
				click($(group));
				// Wait for the group's list to finish sliding open
				waitFor(d -> js("var u = arguments[0].parentElement.querySelector(':scope > ul');" +
									"return (u && (! jQuery(u).is(':animated')) && getComputedStyle(u).display !== 'none') ? true : null;", group));
			}
		}
		target = (WebElement) js(find, moduleTitle, groups, item, "item");
		if (target == null) {
			return false;
		}
		js("window.sailPage = true;");
		click($(target));
		// Wait for the item's page to replace this one and load
		waitForNewPage();
		waitForAjaxResponse();
		return true;
	}

	/**
	 * Indicates if the page is a SmartClient page.
	 *
	 * @return true for SmartClient, false for PrimeFaces (or any other page)
	 */
	public boolean smartClientShowing() {
		return Boolean.TRUE.equals(js("return !! (window.isc && window.isc.BizUtil);"));
	}

	/**
	 * Switches a PrimeFaces session to SmartClient as a user would: opens the header's user menu (the avatar) and
	 * chooses Switch mode, then waits for SmartClient to load. For tests of switching back with {@code <switchMode />}.
	 *
	 * @throws IllegalStateException	if the user menu has no Switch mode item (the user cannot switch), or SmartClient does not load
	 */
	public void switchToSmartClient() {
		click($(byId("avatar")));
		// Wait for the user menu (the avatar's overlay panel) to open
		WebElement item = waitFor(d -> (WebElement) js("var o = document.querySelector('.profile-overlay');" +
														"if ((! o) || (o.getClientRects().length === 0) || (getComputedStyle(o).visibility === 'hidden')) return null;" +
														"return Array.from(o.querySelectorAll('a.ui-menuitem-link')).find(function(a) {return a.textContent.trim() === 'Switch mode';}) || o;"));
		if (! "a".equalsIgnoreCase(item.getTagName())) {
			throw new IllegalStateException("The PrimeFaces user menu has no Switch mode item: this user cannot switch mode");
		}
		js("window.sailPage = true;");
		click($(item));
		// Wait for the page to reload as SmartClient
		waitForNewPage();
		// Wait for SmartClient's scripts to load (they define isc.BizUtil)
		try {
			waitFor(d -> smartClientShowing() ? Boolean.TRUE : null);
		}
		catch (org.openqa.selenium.TimeoutException e) {
			throw new IllegalStateException("Switch mode was chosen but SmartClient did not load", e);
		}
	}

	/**
	 * Switches a SmartClient session to PrimeFaces as a user would: opens the header's user menu and chooses Switch Mode.
	 *
	 * @throws IllegalStateException	if the user menu has no Switch Mode item (the user cannot switch)
	 */
	public void switchFromSmartClient() {
		// Wait for SmartClient to draw its header's user menu button (the avatar)
		WebElement avatar = waitFor(d -> (WebElement) js("var c = isc.Canvas._canvasList.find(function(x) {return x && x._userMenu && x.isDrawn && x.isDrawn();});" +
															"return c ? c.getClipHandle() : null;"));
		click($(avatar));
		// Wait for the user menu to be shown
		waitFor(d -> js("var c = isc.Canvas._canvasList.find(function(x) {return x && x._userMenu;});" +
							"return (c && c._userMenu.isVisible() && c._userMenu.isDrawn()) ? true : null;"));
		WebElement item = (WebElement) js("var m = isc.Canvas._canvasList.find(function(x) {return x && x._userMenu;})._userMenu;" +
											"var cells = Array.from(m.getClipHandle().querySelectorAll('td'));" +
											"return cells.find(function(td) {return (td.textContent || '').trim() === 'Switch Mode';}) || null;");
		if (item == null) {
			throw new IllegalStateException("The SmartClient user menu has no Switch Mode item: this user cannot switch mode");
		}
		js("window.sailPage = true;");
		click($(item));
		// Wait for the PrimeFaces page to replace the SmartClient one
		waitForNewPage();
	}

	/** Defines {@code links}: the displayed links ({@code <a>}) of the page, in document order. */
	private static final String LINKS_JS = "var links = Array.from(document.querySelectorAll('a')).filter(function(a) {" +
												"  return (a.getClientRects().length > 0) && (getComputedStyle(a).visibility !== 'hidden');});";

	/**
	 * Clicks the first displayed link ({@code <a>}) of the page whose visible text contains the text (case-sensitive),
	 * as a user does, then waits for what it leads to: a new page that has loaded, or an ajax request and its response.
	 *
	 * @param text	the text
	 * @return true if a link was clicked, false if no displayed link contains the text
	 * @throws IllegalStateException	if the link opens another window (follow it with {@code <navigateLink window="new"/>})
	 */
	public boolean followLinkContaining(@Nonnull String text) {
		return followLinkContaining(text, false);
	}

	/**
	 * Clicks the first displayed link ({@code <a>}) of the page whose visible text contains the text, or with
	 * {@code exact}, whose visible text (trimmed, white space collapsed) equals it, both case-sensitive; then waits as
	 * {@link #followLinkContaining(String)} does.
	 *
	 * @param text	the text
	 * @param exact	true to match the link's whole text
	 * @return true if a link was clicked, false if no displayed link matches
	 * @throws IllegalStateException	if the link opens another window (follow it with {@code <navigateLink window="new"/>})
	 */
	public boolean followLinkContaining(@Nonnull String text, boolean exact) {
		waitForAjaxResponse();
		WebElement link = (WebElement) js(LINKS_JS + "var t = arguments[0], exact = arguments[1];" +
											"return links.find(function(a) {var x = (a.innerText || '');" +
											"  return exact ? (x.replace(/\\s+/g, ' ').trim() === t) : (x.indexOf(t) >= 0);}) || null;",
											text, Boolean.valueOf(exact));
		if (link == null) {
			return false;
		}
		followLink(link, text);
		return true;
	}

	/**
	 * Clicks a link, then waits for what it leads to: a new page that has loaded, or an ajax request and its response.
	 *
	 * @param link	the link
	 * @param text	the text it was found by, for the messages
	 * @throws IllegalStateException	if the link opens another window (follow it with {@code <navigateLink window="new"/>})
	 */
	private void followLink(@Nonnull WebElement link, @Nonnull String text) {
		String target = link.getDomAttribute("target");
		if ((target != null) && (! target.isEmpty()) && (! "_self".equals(target)) && (! "_top".equals(target)) && (! "_parent".equals(target))) {
			throw new IllegalStateException(String.format("The link containing '%s' opens another window (target %s): follow it with <navigateLink window=\"new\" />", text, target));
		}
		trace("    click " + link.getDomAttribute("href"));
		markPage();
		js("window.sailPage = true;");
		click($(link));
		// Wait for the link's new page to load, or for the ajax request it sent
		String outcome = waitFor(d -> {
			try {
				return (String) ((JavascriptExecutor) d).executeScript(
						"if (! window.sailPage) return (document.readyState === 'complete') ? 'page' : null;" +
						"if (window.sailUnloading) return null;" +
						"return (window.sailAjaxStarts > window.sailAjaxStartMark) ? 'ajax' : null;");
			}
			catch (@SuppressWarnings("unused") WebDriverException e) {
				return null; // between pages
			}
		});
		trace("    the link led to " + ("page".equals(outcome) ? "a new page" : "an ajax request"));
		waitForAjaxResponse();
	}

	/**
	 * Returns the visible texts of the page's displayed links, for a failure.
	 *
	 * @return the texts, at most 30
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<String> linkTexts() {
		List<String> result = (List<String>) js(LINKS_JS + "return links.map(function(a) {return (a.innerText || '').replace(/\\s+/g, ' ').trim();})" +
													".filter(function(t) {return t;}).slice(0, 30);");
		return (result == null) ? new ArrayList<>() : result;
	}

	/**
	 * Clicks the first displayed link ({@code <a>}) of the page whose visible text contains the text, or with
	 * {@code exact}, whose visible text (trimmed, white space collapsed) equals it, both case-sensitive, as
	 * {@link #followLinkContaining(String, boolean)} finds it; then follows it into the new tab or window it opens (see
	 * {@link Selenide#openNewWindow(Runnable, String)}).
	 *
	 * @param text	the text
	 * @param exact	true to match the link's whole text
	 * @return the handle of the window the link was clicked in, or null if no displayed link matches
	 * @throws IllegalStateException	if the link opens no new window
	 */
	public @Nullable String followLinkContainingIntoNewWindow(@Nonnull String text, boolean exact) {
		waitForAjaxResponse();
		WebElement link = (WebElement) js(LINKS_JS + "var t = arguments[0], exact = arguments[1];" +
											"return links.find(function(a) {var x = (a.innerText || '');" +
											"  return exact ? (x.replace(/\\s+/g, ' ').trim() === t) : (x.indexOf(t) >= 0);}) || null;",
											text, Boolean.valueOf(exact));
		if (link == null) {
			return null;
		}
		trace("    click " + link.getDomAttribute("href") + " into a new window");
		return openNewWindow(() -> click($(link)), "The link containing '" + text + "'");
	}

	/**
	 * Waits for the page to load and, for a PrimeFaces page, for its ajax requests and Vue list grids (see
	 * {@link #waitForAjaxResponse()}). Any other page, such as a PDF, is left once it has loaded.
	 */
	@Override
	public void waitForPageToSettle() {
		super.waitForPageToSettle();
		if (Boolean.TRUE.equals(js("return !!window.PrimeFaces;"))) {
			waitForAjaxResponse();
		}
	}

	/**
	 * Reads the text a cell of a grid shows: the cell of the row under the column header that has the id (a PrimeFaces
	 * data table's column) or else the title (a Vue list grid's column). A cell's input (an inline grid) reads as its
	 * value; the column title that a responsive table (or a data grid shown as cards) repeats in its cells is left out.
	 *
	 * @param gridId	the grid's client id (a data table, or a Vue list grid's container)
	 * @param row	the zero-based row
	 * @param headerId	the column header's id, or null to find it by its title
	 * @param headerTitle	the column header's title, used when there is no id
	 * @return [text] when the cell is found, or [null, why not]
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<String> cellText(@Nonnull String gridId, int row, @Nullable String headerId, @Nullable String headerTitle) {
		waitForAjaxResponse();
		List<String> result = (List<String>) js(ROWS_JS + "if (! t) return [null, 'the grid is not on the page'];" +
												"var r = rows[arguments[1]];" +
												"if (! r) return [null, 'the grid has ' + rows.length + ' rows'];" +
												"var th = arguments[2] ? document.getElementById(arguments[2]) : null;" +
												"var title = arguments[3];" +
												"var heads = Array.from(t.querySelectorAll('thead th'));" +
												"if ((! th) && (title !== null)) th = heads.find(function(h) {" +
												"  var x = h.querySelector('.p-column-title, .ui-column-title'); return x && (x.textContent.trim() === title);}) || null;" +
												"if (! th) return [null, 'the grid shows no such column: its columns are ' + heads.map(function(h) {return (h.innerText || '').trim();}).filter(function(x) {return x;}).join(', ')];" +
												"var td = r.cells[th.cellIndex];" +
												"if (! td) return [null, 'the row has no cell under the column'];" +
												"var input = td.querySelector('input:not([type=hidden]), textarea, select');" +
												"if (input) return [String(input.value)];" +
												"var text = td.innerText || '';" +
												"td.querySelectorAll('.ui-column-title, .skyve-card-cell-label').forEach(function(x) {if (x.innerText) text = text.replace(x.innerText, '');});" +
												"return [text.trim()];",
												gridId, Integer.valueOf(row), headerId, headerTitle);
		return (result == null) ? List.of() : result;
	}

	/**
	 * Reads the title the page shows for its view (the template's title).
	 *
	 * @return the title, or null if the page shows none
	 */
	public @Nullable String viewTitle() {
		waitForAjaxResponse();
		return (String) js("var t = Array.from(document.querySelectorAll('.skyve-title-text')).find(function(x) {return x.getClientRects().length > 0;});" +
							"return t ? t.innerText : null;");
	}

	/**
	 * Finds the first displayed link ({@code <a>}) in a row of a grid whose visible text contains the text (case-sensitive).
	 *
	 * @param gridId	the grid's client id
	 * @param row	the zero-based row
	 * @param text	the text
	 * @return [the link or null, whether it is disabled, the texts of the row's displayed links]
	 */
	@SuppressWarnings("unchecked")
	private @Nonnull List<Object> rowLink(@Nonnull String gridId, int row, @Nonnull String text) {
		List<Object> result = (List<Object>) js(ROWS_JS + "var r = rows[arguments[1]]; if (! r) return [null, false, []];" +
												"var links = Array.from(r.querySelectorAll('a')).filter(function(a) {" +
												"  return (a.getClientRects().length > 0) && (getComputedStyle(a).visibility !== 'hidden');});" +
												"var t = arguments[2];" +
												"var link = links.find(function(a) {return (a.innerText || '').indexOf(t) >= 0;}) || null;" +
												"var disabled = !! link && (link.classList.contains('ui-state-disabled') || (link.getAttribute('aria-disabled') === 'true') || link.hasAttribute('disabled'));" +
												"return [link, disabled, links.map(function(a) {return (a.innerText || '').replace(/\\s+/g, ' ').trim();})];",
												gridId, Integer.valueOf(row), text);
		return (result == null) ? java.util.Arrays.asList(null, Boolean.FALSE, List.of()) : result;
	}

	/**
	 * Clicks the first displayed link in a row of a grid whose visible text contains the text, as a user does. For a link
	 * that runs an action, answers its confirmation and the questions it raises, then waits for the response; for any
	 * other link, waits for the page or ajax request it leads to.
	 *
	 * @param gridId	the grid's client id
	 * @param row	the zero-based row
	 * @param text	the link's text
	 * @param action	true if the link runs an action
	 * @param answer	true or null to answer positively if asked, false to answer negatively (which must be asked)
	 * @return true if the action was declined, so it did not run
	 * @throws IllegalStateException	if the row has no such displayed link (the message lists the row's links), the link is
	 * 									disabled, or the answer is false but nothing asked
	 */
	public boolean clickRowLink(@Nonnull String gridId, int row, @Nonnull String text, boolean action, @Nullable Boolean answer) {
		waitForAjaxResponse();
		List<Object> found = rowLink(gridId, row, text);
		if (! (found.get(0) instanceof WebElement link)) {
			throw new IllegalStateException(String.format("Row %d of grid %s has no displayed link containing '%s': its links are %s",
															Integer.valueOf(row), gridId, text, found.get(2)));
		}
		if (Boolean.TRUE.equals(found.get(1))) {
			throw new IllegalStateException(String.format("The link containing '%s' in row %d of grid %s is disabled", text, Integer.valueOf(row), gridId));
		}
		if (! action) {
			followLink(link, text);
			return false;
		}
		trace("    click the link " + (link.getText()));
		markPage();
		click($(link));
		declined = answerQuestions(answer);
		if (declined) {
			return true;
		}
		waitForAjaxResponse();
		return false;
	}

	/**
	 * Runs a script (the SAIL {@code <execute>} step), then waits for any request or new page it started.
	 *
	 * @param script	the JavaScript
	 */
	public void runScript(@Nonnull String script) {
		markPage();
		js(script);
		waitForAjaxResponse();
		if (Boolean.TRUE.equals(js("return !! window.sailUnloading;"))) {
			// Wait for the page the script navigated to (its window has no marker)
			waitFor(d -> {
				try {
					return Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
							"return (window.sailUnloading === undefined) && document.readyState === 'complete';")) ? Boolean.TRUE : null;
				}
				catch (@SuppressWarnings("unused") WebDriverException e) {
					return null; // between pages
				}
			});
			waitForAjaxResponse();
		}
	}

	/**
	 * Finds the element of a content widget that opens its upload page.
	 *
	 * @param contentId	the content widget's client id
	 * @return the element, or null if the widget is not on the page or cannot upload
	 */
	public @Nullable WebElement contentUploadTrigger(@Nonnull String contentId) {
		return (WebElement) js("var c = document.getElementById(arguments[0]); if (! c) return null;" +
								"return Array.from(c.querySelectorAll('[onclick]')).find(function(e) {" +
								"  return /Overlay'\\)\\.show\\(\\)/.test(e.getAttribute('onclick'));}) || null;", contentId);
	}

	/**
	 * Returns the content id that a content widget holds.
	 *
	 * @param contentId	the content widget's client id
	 * @return the content id (empty when there is none), or null if the widget is not on the page
	 */
	public @Nullable String contentValue(@Nonnull String contentId) {
		return (String) js("var c = document.getElementById(arguments[0]); if (! c) return null;" +
							"var h = c.querySelector('input[type=hidden][id$=_hidden]'); return h ? (h.value || '') : '';", contentId);
	}

	/**
	 * Uploads a file through Skyve's upload page: clicks the element that opens it, chooses the file in its frame,
	 * presses Upload, and waits for the page to close it.
	 *
	 * @param triggerId	the id of the element that opens the upload page
	 * @param file	the file to upload
	 * @return true if the trigger was on the page and the upload completed
	 */
	public boolean upload(@Nonnull String triggerId, @Nonnull java.io.File file) {
		WebElement trigger = uploadTrigger(triggerId);
		if (trigger == null) {
			return false;
		}
		return upload(trigger, file);
	}

	/**
	 * Finds an enabled element that opens an upload page, such as an upload action's button.
	 *
	 * @param triggerId	the element's id
	 * @return the element, or null if it is not on the page or is disabled
	 */
	@SuppressWarnings("static-method") // an instance method, as the other gestures are
	public @Nullable WebElement uploadTrigger(@Nonnull String triggerId) {
		SelenideElement trigger = $(byId(triggerId));
		if ((! trigger.exists()) || disabledClass(trigger) || (trigger.attr("disabled") != null)) {
			return null;
		}
		return trigger.getWrappedElement();
	}

	/**
	 * Uploads a file through Skyve's upload page, opened by clicking the given element.
	 *
	 * @param trigger	the element that opens the upload page
	 * @param file	the file to upload
	 * @return true when the upload completed
	 */
	public boolean upload(@Nonnull WebElement trigger, @Nonnull java.io.File file) {
		upload(trigger, file, false);
		return true;
	}

	/**
	 * Uploads a file through Skyve's upload page, opened by clicking the given element, when the application may refuse it.
	 * A refusal shown on the upload page is read, then the upload page is closed with its close icon, as a user does
	 * after reading it.
	 *
	 * @param trigger	the element that opens the upload page
	 * @param file	the file to upload
	 * @return what the upload page said when it refused the file, or null if it took it
	 */
	public @Nullable String uploadOrRefusal(@Nonnull WebElement trigger, @Nonnull java.io.File file) {
		return upload(trigger, file, true);
	}

	/**
	 * Uploads a file through Skyve's upload page.
	 *
	 * @param trigger	the element that opens the upload page
	 * @param file	the file to upload
	 * @param refusable	true to return a refusal (and close the upload page), false to fail with it
	 * @return the refusal, or null if the upload page took the file
	 * @throws IllegalStateException	if the upload page refuses the file and it is not refusable
	 */
	private @Nullable String upload(@Nonnull WebElement trigger, @Nonnull java.io.File file, boolean refusable) {
		click($(trigger));
		// Wait for the upload page's frame to be put in the page
		WebElement frame = waitFor(d -> {
			for (WebElement f : d.findElements(By.tagName("iframe"))) {
				String src = f.getDomAttribute("src");
				String live = (String) js("return arguments[0].src;", f);
				if (((src != null) && src.contains("upload.xhtml")) || ((live != null) && live.contains("upload.xhtml"))) {
					return f;
				}
			}
			return null;
		});
		trace("    upload page opened");
		driver.switchTo().frame(frame);
		try {
			// Wait for the upload page to load its file input
			WebElement input = waitFor(d -> {
				List<WebElement> inputs = d.findElements(By.cssSelector("input[type=file]"));
				return inputs.isEmpty() ? null : inputs.get(0);
			});
			input.sendKeys(file.getAbsolutePath());
			trace("    file chosen");
			// Wait for the shown Upload button to be ready: an image opens the image editor, whose Upload button works
			// once its cropper is ready (it then uploads by itself); another file enables the file upload's own Upload
			// button. Or the page takes the file by itself (no Upload button or file input any more).
			Optional<WebElement> upload = waitFor(d -> {
				Object button = ((JavascriptExecutor) d).executeScript(
						"var shown = function(e) {return e && (e.getClientRects().length > 0) && (getComputedStyle(e).visibility !== 'hidden');};" +
						"var editor = document.getElementById('cropperUpload');" +
						"if (shown(editor)) {" +
						"  var crop = document.getElementById('crop');" +
						"  var box = editor.parentNode.querySelector('#cropperImageContainer .cropper-container .cropper-crop-box');" +
						"  return (shown(crop) && (! crop.disabled) && box && (box.offsetWidth > 0)) ? crop : null;" +
						"}" +
						"var b = document.querySelector('.ui-fileupload-upload');" +
						"if ((! b) || (! document.querySelector('input[type=file]'))) return 'none';" +
						"return (shown(b) && (! b.disabled) && (! b.classList.contains('ui-state-disabled'))) ? b : null;");
				if ("none".equals(button)) {
					return Optional.<WebElement>empty();
				}
				return (button instanceof WebElement element) ? Optional.of(element) : null;
			});
			if (upload.isPresent()) {
				click($(upload.get()));
			}
		}
		finally {
			driver.switchTo().defaultContent();
		}
		trace("    upload pressed");
		// Wait for the upload's response: the page closes the upload page when the upload has succeeded; if it does not
		// (a refusal, or a Skyve without runtime issue P3's fix), the upload page's growl says how it went. The growl of a
		// success shows while the page closes, so a page that is closing is waited for until it has closed. The response
		// shows the growl and starts the close in one go, so the growl is read first, then the page's state in one script:
		// a growl with the page shown and not closing means that the page stays open.
		List<String> outcome = waitFor(d -> {
			List<String> result = uploadPageOutcome(frame);
			String state = uploadPageState(frame);
			if (UPLOAD_PAGE_CLOSED.equals(state)) {
				return List.of("closed", "");
			}
			if (result.isEmpty() || UPLOAD_PAGE_CLOSING.equals(state)) {
				return null;
			}
			return result;
		});
		if ("error".equals(outcome.get(0))) {
			if (! refusable) {
				throw new IllegalStateException("The upload failed: " + outcome.get(1));
			}
			trace("    the upload page refused the file: " + outcome.get(1) + " - close it with its close icon");
			closeUploadPage(frame, outcome.get(1));
			waitForAjaxResponse();
			return outcome.get(1);
		}
		if ("info".equals(outcome.get(0))) {
			// A Skyve without runtime issue P3's fix (such as master, which Apps' A/B runs drive with this harness) leaves
			// an action upload's page open after it succeeds. A user closes it with its close icon, so press that; with no
			// close icon the step fails. The kitchen sinks' Skyve closes the page itself, so this is not reached there.
			trace("    the upload page stayed open, saying: " + outcome.get(1) + " - close it with its close icon");
			closeUploadPage(frame, outcome.get(1));
		}
		waitForAjaxResponse();
		return null;
	}

	/**
	 * Closes the upload page with its close icon, as a user does, and waits for it to close.
	 *
	 * @param frame	the upload page's frame
	 * @param said	what the upload page says, for the failure
	 * @throws IllegalStateException	if the upload page is still open and has no close icon
	 */
	private void closeUploadPage(@Nonnull WebElement frame, @Nonnull String said) {
		WebElement close = (WebElement) js("var o = arguments[0].closest('.ui-overlaypanel, .ui-dialog');" +
											"var c = o ? o.querySelector('.ui-overlaypanel-close, .ui-dialog-titlebar-close') : null;" +
											"return (c && c.getClientRects().length > 0) ? c : null;",
											frame);
		if (close == null) {
			// The page may be closing or closed by itself since the growl was read: only a page that is still open fails
			if (! UPLOAD_PAGE_OPEN.equals(uploadPageState(frame))) {
				trace("    the upload page is closing by itself after all");
				// Wait for the upload page to close (its overlay's hide effect)
				waitFor(d -> uploadPageClosed(frame) ? Boolean.TRUE : null);
				return;
			}
			throw new IllegalStateException("The upload page stayed open after \"" + said + "\" and has no close icon a user could press");
		}
		click($(close));
		// Wait for the upload page to close (its overlay's hide effect)
		waitFor(d -> uploadPageClosed(frame) ? Boolean.TRUE : null);
	}

	/** {@link #uploadPageState(WebElement)}: the upload page is shown and not closing. */
	private static final String UPLOAD_PAGE_OPEN = "open";
	/** {@link #uploadPageState(WebElement)}: the upload page is hidden, or its frame no longer shows it. */
	private static final String UPLOAD_PAGE_CLOSED = "closed";
	/** {@link #uploadPageState(WebElement)}: the upload page's overlay is running its hide transition. */
	private static final String UPLOAD_PAGE_CLOSING = "closing";

	/**
	 * Returns the upload page's state, read in one script so that it is consistent: {@link #UPLOAD_PAGE_CLOSED} when its
	 * frame is gone, hidden, or no longer shows the upload page; {@link #UPLOAD_PAGE_CLOSING} while its overlay runs its
	 * hide transition (PrimeFaces marks an overlay being hidden with {@code ui-connected-overlay-exit} until it has hidden
	 * it; a dialog hides at once); otherwise {@link #UPLOAD_PAGE_OPEN}.
	 *
	 * @param frame	the upload page's frame
	 * @return the state
	 */
	private @Nonnull String uploadPageState(@Nonnull WebElement frame) {
		try {
			String result = (String) js("var f = arguments[0], o = f.closest('.ui-overlaypanel, .ui-dialog'), src = f.src;" +
											"if ((! f.isConnected) || (f.getClientRects().length === 0) || (o && getComputedStyle(o).display === 'none') ||" +
											"    (! src) || (src.indexOf('upload.xhtml') < 0)) return 'closed';" +
											"return (o && o.classList.contains('ui-connected-overlay-exit')) ? 'closing' : 'open';", frame);
			return (result == null) ? UPLOAD_PAGE_CLOSED : result;
		}
		catch (@SuppressWarnings({"unused", "java:S1166"}) StaleElementReferenceException e) {
			return UPLOAD_PAGE_CLOSED;
		}
	}

	private boolean uploadPageClosed(@Nonnull WebElement frame) {
		return UPLOAD_PAGE_CLOSED.equals(uploadPageState(frame));
	}

	/**
	 * Reads the upload page's growl: {severity, text}, or empty if it shows none yet.
	 */
	@SuppressWarnings("unchecked")
	private @Nonnull List<String> uploadPageOutcome(@Nonnull WebElement frame) {
		try {
			driver.switchTo().frame(frame);
			// an error, followed by the problems the page lists (an upload action's refusal is in its problems table)
			List<String> result = (List<String>) js("var p = document.querySelector('#problems table');" +
														"var problems = p ? p.innerText.replace(/\\s+/g, ' ').trim() : '';" +
														"var e = Array.from(document.querySelectorAll('.ui-growl-error, .ui-growl-fatal, .ui-messages-error')).find(function(x) {" +
														"  return x.getClientRects().length > 0 && (x.innerText || '').trim();});" +
														"if (e) return ['error', e.innerText.replace(/\\s+/g, ' ').trim() + (problems ? ' ' + problems : '')];" +
														"if (problems) return ['error', problems];" +
														"var i = document.querySelector('.ui-growl-info');" +
														"if (i) return ['info', i.innerText.replace(/\\s+/g, ' ').trim()];" +
														"return [];");
			return (result == null) ? new ArrayList<>() : result;
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return new ArrayList<>();
		}
		finally {
			driver.switchTo().defaultContent();
		}
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
		String result = messages.isEmpty() ? "no messages" : String.join(" | ", messages);
		// the page's script errors since it was marked, which can stop a response being applied
		Object errors = js("return (window.sailScriptErrors && window.sailScriptErrors.length) ? window.sailScriptErrors.join(' | ') : null;");
		return (errors == null) ? result : result + " (page script errors: " + errors + ")";
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
		return redirectButton(id, confirm ? Boolean.TRUE : null);
	}

	/**
	 * Clicks a button that leads to another page, answers any confirmation or question, and waits for the page.
	 *
	 * @param id	the button's client id
	 * @param answer	true or null to answer positively if asked, false to answer negatively (which must be asked)
	 * @return true if the button was on the page, enabled and clicked
	 */
	public boolean redirectButton(@Nonnull String id, @Nullable Boolean answer) {
		SelenideElement element = $(byId(id));
		// a user can only press a button that is shown (another rendering of the same action may be)
		if (element.exists() && element.isDisplayed() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
			String viewState = markPage();
			click(element);

			declined = answerQuestions(answer);
			if (! declined) {
				waitForFullPageResponse(viewState);
			}

			return true;
		}

		return false;
	}

	/**
	 * Indicates if the last {@link #button(String, boolean, Boolean)} or {@link #redirectButton(String, Boolean)} was declined,
	 * so there was no response.
	 *
	 * @return true if declined
	 */
	public boolean wasDeclined() {
		return declined;
	}

	/** Whether the last button was declined. */
	private boolean declined = false;

	/**
	 * Waits until no PrimeFaces or jQuery ajax request is running and the ajax status indicator is hidden.
	 */
	public void waitForAjaxResponse() {
		// Wait for the PrimeFaces ajax queue to empty and its partial responses to be applied
		waitFor(d -> ajaxIdle() ? Boolean.TRUE : null);
	}

	/**
	 * Returns false from the enclosing script while a Vue list grid is not mounted in its container yet (the page's
	 * {@code SKYVE.listgrid(...)} call mounts it after the container renders) or is fetching its rows (its paginator
	 * spinner is active, from the fetch's debounce until the rows arrive).
	 */
	private static final String VUE_GRIDS_BUSY_JS = "if (document.querySelector('.skyve-vue-list-grid-spinner-active')) return false;" +
														"var scripts = document.getElementsByTagName('script');" +
														"for (var i = 0; i < scripts.length; i++) {" +
														"  var text = scripts[i].textContent || '';" +
														"  if (text.indexOf('SKYVE.listgrid(') < 0) continue;" +
														"  var m = /\"containerId\"\\s*:\\s*\"([^\"]+)\"/.exec(text);" +
														"  var c = m ? document.getElementById(m[1]) : null;" +
														"  if (c && (! c.querySelector('.skyve-vue-list-grid'))) return false;" +
														"}";

	/**
	 * Indicates if the page has loaded (and its template has set up its layout) and no PrimeFaces or jQuery ajax request is running.
	 *
	 * @return true when idle
	 */
	private boolean ajaxIdle() {
		return Boolean.TRUE.equals(js("if (document.readyState !== 'complete') return false;" +
										// the template's page script has finished setting up the layout (side bar, breakpoints)
										"if (document.body && document.body.classList.contains('is-preload')) return false;" +
										"if (window.PrimeFaces && PrimeFaces.ajax && PrimeFaces.ajax.Queue && (! PrimeFaces.ajax.Queue.isEmpty())) return false;" +
										"if (window.jQuery && (jQuery.active > 0)) return false;" +
										VUE_GRIDS_BUSY_JS +
										"var w = document.getElementById('wheelOfDeath_start');" +
										"return (! w) || (w.offsetParent === null);"));
	}

	/**
	 * Waits for the page marked by {@link #markPage()} (or {@code window.sailPage}) to be replaced by a new page that has loaded.
	 */
	private void waitForNewPage() {
		// Wait for the next page to replace this one (its window has no marker) and finish loading
		waitFor(d -> {
			try {
				return Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
						"return (! window.sailPage) && document.readyState === 'complete';")) ? Boolean.TRUE : null;
			}
			catch (@SuppressWarnings("unused") WebDriverException e) {
				return null; // between pages
			}
		});
		waitForAjaxResponse();
	}

	/**
	 * Waits for the response to a click: a new page (or view state), or a response that brings back error messages
	 * (validation, which Skyve's PrimeFaces views do on the server), which leaves the page as it was.
	 *
	 * @param oldViewState	the view state before the request
	 */
	public void waitForFullPageResponse(String oldViewState) {
		// Wait for the response to the click: a new page or view state, or a response with new errors
		// (errors that were on the page before the click, see markPage(), do not count; nor does a page that is unloading)
		waitFor(d -> {
			String viewState = getViewState();
			if ((viewState != null) && (! viewState.equals(oldViewState))) {
				return Boolean.TRUE;
			}
			if ((! ajaxIdle()) || (! newErrorsShown())) {
				return null;
			}
			return responseArrived() ? Boolean.TRUE : null;
		});

		waitForAjaxResponse();
	}

	/**
	 * Indicates if error messages are shown that were not on the page when {@link #markPage()} last ran, and the page is
	 * not unloading.
	 */
	private boolean newErrorsShown() {
		try {
			return Boolean.TRUE.equals(js("if (window.sailUnloading) return false;" +
											"return Array.from(document.querySelectorAll(arguments[0])).some(function(e) {" +
											"  return (e.sailSeen !== (e.firstChild || true)) && (e.innerText || e.textContent || '').trim().length > 0;});",
											ERROR_MESSAGES_SELECTOR));
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return false; // between pages
		}
	}

	/**
	 * Counts ajax requests and notes page unloads from now on, and marks the error messages already shown, so
	 * {@link #responseArrived()}, {@link #requestSent()} and {@link #newErrorsShown()} can tell what the next click did,
	 * and returns the view state, before a click.
	 *
	 * @return the view state
	 */
	private String markPage() {
		js("if (! window.sailHooked) {window.sailHooked = true; window.sailAjax = 0; window.sailAjaxStarts = 0; window.sailScriptErrors = [];" +
				"  window.addEventListener('error', function(e) {window.sailScriptErrors.push(String(e.message));});" +
				"  window.addEventListener('beforeunload', function() {window.sailUnloading = true;});" +
				"  if (window.jQuery) {" +
				"    jQuery(document).on('pfAjaxComplete', function() {window.sailAjax++;});" +
				"    jQuery(document).on('pfAjaxStart', function() {window.sailAjaxStarts++;});}}" +
				"window.sailAjaxMark = window.sailAjax; window.sailAjaxStartMark = window.sailAjaxStarts; window.sailUnloading = false;" +
				// the error messages shown now are an earlier step's: the response to the next click brings its own
				// (a message counts as new when it is a new element, or its content has been rendered again)
				"document.querySelectorAll(arguments[0]).forEach(function(e) {e.sailSeen = e.firstChild || true;});",
				ERROR_MESSAGES_SELECTOR);
		return getViewState();
	}

	/**
	 * Indicates if an ajax response has arrived since {@link #markPage()}, or the page has been replaced.
	 */
	private boolean responseArrived() {
		return Boolean.TRUE.equals(js("return (window.sailAjaxMark === undefined) || (window.sailAjax > window.sailAjaxMark);"));
	}

	/**
	 * Indicates if a request has started since {@link #markPage()}, or the page is unloading or has been replaced.
	 */
	private boolean requestSent() {
		try {
			return Boolean.TRUE.equals(js("return (window.sailAjaxStartMark === undefined) || (window.sailAjaxStarts > window.sailAjaxStartMark) || " +
											"(window.sailAjax > window.sailAjaxMark) || (window.sailUnloading === true);"));
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return true; // between pages
		}
	}

	/**
	 * Returns the page's Faces view state, without waiting.
	 *
	 * @return the view state, or null if the page has none (e.g. between pages)
	 */
	public @Nullable String getViewState() {
		try {
			return (String) js("var e = document.getElementsByName('jakarta.faces.ViewState')[0]; return e ? e.value : null;");
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return null; // between pages
		}
	}

	/**
	 * Clicks an element as a user would: scrolls it to the middle of the window (so a fixed header or banner cannot
	 * cover it, even in a small window), waits until nothing else is over it, then clicks it with WebDriver.
	 * There is no script fallback: a click a user could not make fails, saying why.
	 *
	 * @param element	the element to click
	 * @throws IllegalStateException	if the element cannot take a user's click
	 */
	public void click(SelenideElement element) {
		WebElement wrappedElement = element.getWrappedElement();
		scrollToCentre(wrappedElement);
		waitUntilUncovered(wrappedElement);
		try {
			wrappedElement.click();
		} catch (WebDriverException e) {
			String why = String.valueOf(e.getMessage());
			int newline = why.indexOf('\n');
			throw new IllegalStateException("A user could not click " + describeForClick(wrappedElement) + ": " +
												((newline > 0) ? why.substring(0, newline) : why), e);
		}
	}

	/**
	 * Clicks the editorial template's side bar toggle (the menu button of a small window) with the pointer.
	 * <p>
	 * A per-widget case: the toggle hangs outside its (off-canvas) side bar's box, so WebDriver's element click calls it
	 * "not interactable" although it is shown and on top. A user clicks it where it is drawn, so press the pointer at the
	 * centre of its part inside the window, after checking that the toggle is what is there.
	 */
	private void clickToggle(@Nonnull WebElement toggle) {
		// Wait for the template to lay the toggle out (it has no size until its breakpoint styles apply)
		waitFor(d -> js("var r = arguments[0].getBoundingClientRect(); return (r.width > 0 && r.height > 0) ? true : null;", toggle));
		@SuppressWarnings("unchecked")
		List<Number> point = (List<Number>) js("var e = arguments[0], r = e.getBoundingClientRect();" +
												"var x = (Math.max(r.left, 0) + Math.min(r.right, window.innerWidth)) / 2;" +
												"var y = (Math.max(r.top, 0) + Math.min(r.bottom, window.innerHeight)) / 2;" +
												"var t = document.elementFromPoint(x, y);" +
												"return (t === e || e.contains(t)) ? [Math.round(x), Math.round(y)] : null;", toggle);
		if (point == null) {
			throw new IllegalStateException("A user could not click the side bar menu toggle " + describeForClick(toggle));
		}
		new org.openqa.selenium.interactions.Actions(driver).moveToLocation(point.get(0).intValue(), point.get(1).intValue()).click().perform();
	}

	/**
	 * Describes an element, where it is and what is over its centre, for a click failure.
	 */
	private @Nonnull String describeForClick(@Nonnull WebElement element) {
		try {
			return String.valueOf(js("var e = arguments[0], r = e.getBoundingClientRect();" +
										"var t = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);" +
										"var d = function(x) {return x ? '<' + x.tagName.toLowerCase() + (x.id ? ' id=' + x.id : '') + " +
										"  (x.className && x.className.baseVal === undefined ? ' class=\"' + x.className + '\"' : '') + '>' : 'nothing';};" +
										"return d(e) + ' at [' + Math.round(r.left) + ',' + Math.round(r.top) + ' ' + Math.round(r.width) + 'x' + Math.round(r.height) + " +
										"  '] in a ' + window.innerWidth + 'x' + window.innerHeight + ' window scrolled to ' + Math.round(window.scrollY) + ' of ' + " +
										"  (document.documentElement.scrollHeight - window.innerHeight) + ', under ' + d(t) + (t ? ' (' + getComputedStyle(t).position + ')' : '') + " +
										"  '; the fixed action bar is ' + (function() {var b = document.querySelector('.skyve-action-bar-wrapper'); " +
										"    if (! b) return 'absent'; var r2 = b.getBoundingClientRect(); return getComputedStyle(b).position + ' at ' + Math.round(r2.top) + '+' + Math.round(r2.height);})();", element));
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return String.valueOf(element);
		}
	}

	/**
	 * Defines <code>centre(e)</code>: scrollIntoView, then make sure the window itself scrolled the element to its middle
	 * (scrollIntoView can leave the window as it was, e.g. after an ajax update replaced part of the page).
	 */
	private static final String CENTRE_SCRIPT = "var centre = function(e) {e.scrollIntoView({block: 'center', inline: 'nearest'});" +
													"var r = e.getBoundingClientRect(), mid = r.top + r.height / 2;" +
													"if ((mid < window.innerHeight * 0.25) || (mid > window.innerHeight * 0.75)) {" +
													"  window.scrollBy(0, mid - window.innerHeight / 2);}};";

	/**
	 * Scrolls an element to the middle of the window, away from fixed headers, banners and footers.
	 *
	 * @param element	the element
	 */
	public void scrollToCentre(@Nonnull WebElement element) {
		try {
			js(CENTRE_SCRIPT + "centre(arguments[0]);", element);
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			// not scrollable (e.g. hidden); the click reports the problem
		}
	}

	/**
	 * Waits until the element's centre is not covered by another element, e.g. while a header settles or an overlay
	 * animates away.
	 */
	private void waitUntilUncovered(@Nonnull WebElement element) {
		try {
			// Wait for nothing to be over the element's centre (a header settling, an overlay closing).
			// While it is covered, centre it again: the page can scroll itself after the first centring
			// (e.g. a select menu focusing its input late), leaving the element under the fixed header.
			// In a Vue list grid, also wait for the element to stop moving: the grid lays its toolbar out again over the
			// next animation frames when it is shown (e.g. its tab is selected), so it must be in the same place twice.
			waitFor(d -> js(CENTRE_SCRIPT + "var e = arguments[0], r = e.getBoundingClientRect();" +
								"if ((r.width === 0) && (r.height === 0)) return true;" +
								"if (e.closest && e.closest('.skyve-vue-list-grid')) {" +
								"  var at = [r.left, r.top, r.width, r.height].join(',');" +
								"  if (e.sailAt !== at) {e.sailAt = at; return null;}" +
								"}" +
								"var t = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);" +
								"if ((t === null) || (t === e) || e.contains(t) || t.contains(e) || (t.tagName === 'LABEL' && t.htmlFor === e.id)) return true;" +
								"centre(e); return null;",
								element));
		}
		catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException | StaleElementReferenceException e) {
			// still covered (or gone): the click or typing reports the problem
		}
	}

	private @Nullable Object js(@Nonnull String script, Object... arguments) {
		return ((JavascriptExecutor) driver).executeScript(script, arguments);
	}

	private static boolean disabledClass(SelenideElement element) {
		String styleClass = element.attr("class");
		return (styleClass != null) && styleClass.contains("ui-state-disabled");
	}

	/**
	 * Finds an element by id, without waiting.
	 *
	 * @param id	the element's id
	 * @return the element, or null if it is not on the page
	 */
	public @Nullable WebElement elementById(@Nonnull String id) {
		return by(i -> driver.findElement(By.id(i)), id);
	}

	private static @Nullable WebElement by(@Nonnull Function<String, WebElement> function, @Nonnull String search) {
		// No wait: callers look for elements of a response that has already been waited for
		try {
			WebElement result = function.apply(search);
			result.isDisplayed(); // check for stale element
			return result;
		} catch (@SuppressWarnings("unused") NoSuchElementException | StaleElementReferenceException e) {
			return null;
		}
	}
}

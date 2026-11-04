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
		if (element.exists() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
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
			Boolean aborted = (Boolean) js("var d = document.querySelector('[role=alertdialog][aria-labelledby=skyveQuestionTitle]');" +
											"var buttons = Array.from(d.querySelectorAll('.ui-dialog-buttonpane button'));" +
											"var close = d.querySelector('.ui-dialog-titlebar-close');" +
											"var abortLabel = close ? close.getAttribute('aria-label') : null;" +
											"var negative = buttons.find(function(b) {return b.textContent.trim() === abortLabel;}) || buttons[buttons.length - 1];" +
											"var positive = buttons.find(function(b) {return b !== negative;});" +
											"var chosen = arguments[0] ? (positive || negative) : negative;" +
											"chosen.click();" +
											"return chosen === negative && abortLabel !== null;",
											Boolean.valueOf(positive));
			if (Boolean.TRUE.equals(aborted)) {
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
		WebElement element = oldById(dataGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// data grid button is present
			element = oldById(buttonId);
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
		WebElement element = oldById(listGridId);
		if (element != null && element.isDisplayed() && element.isEnabled()) {
			// list grid button is present
			element = oldById(buttonId);
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
			scrollToCentre(element.getWrappedElement());
			waitUntilUncovered(element.getWrappedElement());
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
		scrollToCentre(element.getWrappedElement());
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
	 * Makes the given items (by label) the members of a pick list (list membership); every other item becomes a candidate.
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
													"if (missing.length) return missing.concat(['(the items are: ' + labels.join(' | ') + ')']);" +
													"w.unselectAll();" +
													"var out = w.targetList.children('li.ui-picklist-item').toArray().filter(function(li) {return wanted.indexOf(label(li)) < 0;});" +
													"if (out.length) {out.forEach(function(li) {w.selectItem($(li));}); w.remove();}" +
													"w.unselectAll();" +
													"var inn = w.sourceList.children('li.ui-picklist-item').toArray().filter(function(li) {return wanted.indexOf(label(li)) >= 0;});" +
													"if (inn.length) {inn.forEach(function(li) {w.selectItem($(li));}); w.add();}" +
													"return [];", id, members);
		if ((missing != null) && missing.isEmpty()) {
			// Wait for the (animated) transfer to finish and save the members into the pick list's submitted input
			waitFor(d -> {
				List<String> current = pickListMembers(id);
				return ((current != null) && new java.util.HashSet<>(current).equals(new java.util.HashSet<>(members)) &&
						Boolean.TRUE.equals(js("var w = PrimeFaces.getWidgetById(arguments[0]);" +
												"return (w.jq.find(':animated').length === 0) && " +
												"(w.targetInput.children().length === w.targetList.children('li.ui-picklist-item').length);", id))) ?
							Boolean.TRUE : null;
			});
			waitForAjaxResponse();
		}
		return missing;
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
	private static final String ROWS_JS = "var t = document.getElementById(arguments[0]); if (! t) return null;" +
											"var body = t.querySelector('tbody.ui-datatable-data');" +
											"var rows = body ? Array.from(body.children).filter(function(r) {" +
											"  return r.tagName === 'TR' && (! r.classList.contains('ui-datatable-empty-message'));}) : [];";

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
	 * Asks a lazily loaded grid (a list grid) to load its rows again.
	 *
	 * @param gridId	the data table's client id
	 */
	public void refreshGrid(@Nonnull String gridId) {
		js("var w = PrimeFaces.getWidgetById(arguments[0]); if (w && w.cfg && w.cfg.lazy && w.filter) w.filter();", gridId);
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
		// Expand the module and each group, then click the item
		WebElement header = (WebElement) js(find, moduleTitle, groups, item, "header");
		if ((header != null) && (header.getDomAttribute("class") != null) && (! header.getDomAttribute("class").contains("ui-state-active"))) {
			click($(header));
			// Wait for the module's panel to finish sliding open
			waitFor(d -> js("var c = arguments[0].nextElementSibling;" +
								"return (c && (! jQuery(c).is(':animated')) && getComputedStyle(c).display !== 'none') ? true : null;", header));
		}
		for (int g = 0, l = groups.size(); g < l; g++) {
			WebElement group = (WebElement) js(find, moduleTitle, groups, item, "group" + g);
			WebElement expandedFlag = (group == null) ? null : group.findElement(By.cssSelector(".ui-menuitem-text"));
			if ((expandedFlag != null) && (! "true".equals(expandedFlag.getDomAttribute("aria-expanded")))) {
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
		SelenideElement trigger = $(byId(triggerId));
		if ((! trigger.exists()) || disabledClass(trigger) || (trigger.attr("disabled") != null)) {
			return false;
		}
		return upload(trigger.getWrappedElement(), file);
	}

	/**
	 * Uploads a file through Skyve's upload page, opened by clicking the given element.
	 *
	 * @param trigger	the element that opens the upload page
	 * @param file	the file to upload
	 * @return true when the upload completed
	 */
	public boolean upload(@Nonnull WebElement trigger, @Nonnull java.io.File file) {
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
			// Press Upload once it is enabled, unless the page has taken the file by itself
			// Wait for the upload widget to accept the file (its Upload button enables), then press it
			waitFor(d -> {
				List<WebElement> buttons = d.findElements(By.cssSelector(".ui-fileupload-upload"));
				if (buttons.isEmpty() || d.findElements(By.cssSelector("input[type=file]")).isEmpty()) {
					return Boolean.TRUE;
				}
				WebElement button = buttons.get(0);
				String styleClass = button.getDomAttribute("class");
				if (button.isEnabled() && ((styleClass == null) || (! styleClass.contains("ui-state-disabled")))) {
					((JavascriptExecutor) d).executeScript("arguments[0].click();", button);
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			});
		}
		finally {
			driver.switchTo().defaultContent();
		}
		trace("    upload pressed");
		// The page closes the upload page when the upload has been processed. If it does not (the tablet UX/UI),
		// the upload page's growl says how it went: close the upload page on success, fail otherwise.
		// Wait for the upload's response: the page closes the upload page, or the upload page's growl reports it
		List<String> outcome = waitFor(d -> {
			if (uploadPageClosed(frame)) {
				return List.of("closed", "");
			}
			List<String> result = uploadPageOutcome(frame);
			return result.isEmpty() ? null : result;
		});
		if ("error".equals(outcome.get(0))) {
			throw new IllegalStateException("The upload failed: " + outcome.get(1));
		}
		if ("info".equals(outcome.get(0))) {
			trace("    the upload page says: " + outcome.get(1) + " - close it");
			js("var o = arguments[0].closest('.ui-overlaypanel, .ui-dialog');" +
					"var w = o ? PrimeFaces.getWidgetById(o.id) : null;" +
					"if (w && w.hide) w.hide(); else if (o) {var c = o.querySelector('.ui-overlaypanel-close, .ui-dialog-titlebar-close'); if (c) c.click();}",
					frame);
		}
		waitForAjaxResponse();
		return true;
	}

	private boolean uploadPageClosed(@Nonnull WebElement frame) {
		try {
			String live = (String) js("return arguments[0].src;", frame);
			Boolean shown = (Boolean) js("var o = arguments[0].closest('.ui-overlaypanel, .ui-dialog');" +
											"return arguments[0].getClientRects().length > 0 && ((! o) || getComputedStyle(o).display !== 'none');", frame);
			return (! Boolean.TRUE.equals(shown)) || (live == null) || (! live.contains("upload.xhtml"));
		}
		catch (@SuppressWarnings({"unused", "java:S1166"}) StaleElementReferenceException e) {
			return true;
		}
	}

	/**
	 * Reads the upload page's growl: {severity, text}, or empty if it shows none yet.
	 */
	@SuppressWarnings("unchecked")
	private @Nonnull List<String> uploadPageOutcome(@Nonnull WebElement frame) {
		try {
			driver.switchTo().frame(frame);
			List<String> result = (List<String>) js("var e = Array.from(document.querySelectorAll('.ui-growl-error, .ui-growl-fatal, .ui-messages-error')).find(function(x) {" +
														"  return x.getClientRects().length > 0 && (x.innerText || '').trim();});" +
														"if (e) return ['error', e.innerText.replace(/\\s+/g, ' ').trim()];" +
														"var p = document.querySelector('#problems table');" +
														"if (p) return ['error', p.innerText.replace(/\\s+/g, ' ').trim()];" +
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
		if (element.exists() && (!disabledClass(element)) && (element.attr("disabled") == null)) {
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
	 * (validation), which leaves the page as it was. When client-side validation stops the request before it is sent,
	 * there is no response to wait for.
	 *
	 * @param oldViewState	the view state before the request
	 */
	public void waitForFullPageResponse(String oldViewState) {
		try {
			// Wait for the response to the click: a new page or view state, or errors from the server or client-side validation
			waitFor(d -> {
				String viewState = getViewState();
				if ((viewState != null) && (! viewState.equals(oldViewState))) {
					return Boolean.TRUE;
				}
				if ((! ajaxIdle()) || errorMessages().isEmpty()) {
					return null;
				}
				return (responseArrived() || nothingSent()) ? Boolean.TRUE : null;
			});
		} catch (RuntimeException e) {
			System.err.println("Timed out waiting for a navigation from " + driver.getCurrentUrl() + " : oldViewState = " + oldViewState);
			throw e;
		}

		waitForAjaxResponse();
	}

	/**
	 * Counts ajax requests and notes page unloads from now on, so {@link #responseArrived()} and {@link #nothingSent()}
	 * can tell what the next click did, and returns the view state, before a click.
	 *
	 * @return the view state
	 */
	private String markPage() {
		js("if (! window.sailHooked) {window.sailHooked = true; window.sailAjax = 0; window.sailAjaxStarts = 0;" +
				"  window.addEventListener('beforeunload', function() {window.sailUnloading = true;});" +
				"  if (window.jQuery) {" +
				"    jQuery(document).on('pfAjaxComplete', function() {window.sailAjax++;});" +
				"    jQuery(document).on('pfAjaxStart', function() {window.sailAjaxStarts++;});}}" +
				"window.sailAjaxMark = window.sailAjax; window.sailAjaxStartMark = window.sailAjaxStarts; window.sailUnloading = false;");
		return getViewState();
	}

	/**
	 * Indicates if an ajax response has arrived since {@link #markPage()}, or the page has been replaced.
	 */
	private boolean responseArrived() {
		return Boolean.TRUE.equals(js("return (window.sailAjaxMark === undefined) || (window.sailAjax > window.sailAjaxMark);"));
	}

	/**
	 * Indicates if the click since {@link #markPage()} sent nothing: no ajax request started and the page is not unloading.
	 */
	private boolean nothingSent() {
		return Boolean.TRUE.equals(js("return (window.sailAjaxStartMark !== undefined) && (window.sailAjaxStarts === window.sailAjaxStartMark) && " +
										"(! window.sailUnloading) && (! (window.PrimeFaces && PrimeFaces.ajax && PrimeFaces.ajax.Queue && (! PrimeFaces.ajax.Queue.isEmpty())));"));
	}

	@SuppressWarnings("static-method")
	public String getViewState() {
		return $(byName("jakarta.faces.ViewState")).attr("value");
	}

	/**
	 * Clicks an element as a user would: scrolls it to the middle of the window (so a fixed header or banner cannot
	 * cover it, even in a small window), waits until nothing else is over it, then clicks.
	 * Falls back to a script click if another element still takes the click.
	 *
	 * @param element	the element to click
	 */
	public void click(SelenideElement element) {
		WebElement wrappedElement = element.getWrappedElement();
		scrollToCentre(wrappedElement);
		waitUntilUncovered(wrappedElement);
		try {
			wrappedElement.click();
		} catch (@SuppressWarnings("unused") WebDriverException e) {
			// Something (a floating element or an animation) still covers it
			trace("    Could not click on the element - scroll it to the middle again and retry");
			scrollToCentre(wrappedElement);
			waitUntilUncovered(wrappedElement);
			try {
				wrappedElement.click();
			} catch (@SuppressWarnings("unused") WebDriverException e1) {
				trace("    Could not click on the element - click it with script");
				js("arguments[0].click();", wrappedElement);
			}
		}
	}

	/**
	 * Scrolls an element to the middle of the window, away from fixed headers, banners and footers.
	 *
	 * @param element	the element
	 */
	public void scrollToCentre(@Nonnull WebElement element) {
		try {
			js("arguments[0].scrollIntoView({block: 'center', inline: 'center'});", element);
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
			// Wait for nothing to be over the element's centre (a header settling, an overlay closing)
			waitFor(d -> js("var e = arguments[0], r = e.getBoundingClientRect();" +
								"if ((r.width === 0) && (r.height === 0)) return true;" +
								"var t = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);" +
								"return ((t === null) || (t === e) || e.contains(t) || t.contains(e) || (t.tagName === 'LABEL' && t.htmlFor === e.id)) ? true : null;",
								element));
		}
		catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException | StaleElementReferenceException e) {
			// still covered (or gone): the click falls back, or reports the problem
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

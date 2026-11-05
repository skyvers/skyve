package util.sail;

import static com.codeborne.selenide.Condition.exist;
import static com.codeborne.selenide.Condition.visible;
import static com.codeborne.selenide.Selenide.$;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.Locator.Kind;

import com.codeborne.selenide.SelenideElement;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.commands.impl.SmartClientButtonCommand;
import util.sail.commands.impl.SmartClientCheckboxCommand;
import util.sail.commands.impl.SmartClientDataGridButtonCommand;
import util.sail.commands.impl.SmartClientDataGridSelectCommand;
import util.sail.commands.impl.SmartClientListGridButtonCommand;
import util.sail.commands.impl.SmartClientListGridSelectCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionByRowCommand;
import util.sail.commands.impl.SmartClientLookupDescriptionBySearchCommand;
import util.sail.commands.impl.SmartClientRadioCommand;
import util.sail.commands.impl.SmartClientSelectOneCommand;
import util.sail.commands.impl.SmartClientTabCommand;
import util.sail.commands.impl.SmartClientTextCommand;

/**
 * Selenide implementation for the SmartClient framework,
 * binding SmartClient-specific command types to enable typed, framework-specific UI automation.
 * <p>
 * Elements are found through {@code util/sail/smartclient-sail.js}, a helper this class injects into
 * each page. It resolves SAIL locators (see {@link Locator}) against the live SmartClient view and then
 * uses SmartClient's {@code isc.AutoTest} locators, so no component IDs are re-derived in Java.
 * Widgets are scrolled into view with SmartClient's own {@code Canvas.scrollIntoView()}.
 *
 * @author simeonsolomou
 */
public class SmartClientSelenide extends Selenide<
	SmartClientButtonCommand,
	SmartClientCheckboxCommand,
	SmartClientDataGridButtonCommand,
	SmartClientDataGridSelectCommand,
	SmartClientListGridButtonCommand,
	SmartClientListGridSelectCommand,
	SmartClientLookupDescriptionByRowCommand,
	SmartClientLookupDescriptionBySearchCommand,
	SmartClientRadioCommand,
	SmartClientTabCommand,
	SmartClientTextCommand,
	SmartClientSelectOneCommand> {

	private static final String HELPER_RESOURCE = "util/sail/smartclient-sail.js";
	private static final String HELPER = loadHelper();

	private static final String DIALOG_OK = Locator.sail(null, Kind.dialog, "ok", null, null, null, null);

	// The reason the last locate() or verify call failed, for messages
	private String lastProblem;

	/**
	 * An element found for a locator, with what SmartClient says about the widget.
	 *
	 * @param element	the element
	 * @param shown		whether the widget and its ancestors are drawn and visible
	 * @param disabled	whether the widget is disabled or read only
	 * @param locator	the AutoTest locator the element was found with
	 */
	public record Found(@Nonnull SelenideElement element, boolean shown, boolean disabled, @Nonnull String locator) {
	}

	/**
	 * Reads the browser helper script from the test classpath.
	 *
	 * @return the script
	 */
	private static @Nonnull String loadHelper() {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(HELPER_RESOURCE)) {
			if (stream == null) {
				throw new IllegalStateException(HELPER_RESOURCE + " is not on the test classpath");
			}
			return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
		}
		catch (IOException e) {
			throw new IllegalStateException("Could not read " + HELPER_RESOURCE, e);
		}
	}

	/**
	 * Waits for an asynchronous event with the harness's one wait mechanism ({@link Selenide#waitFor}), turning a timeout
	 * into a failure that says what was waited for and what the browser shows.
	 *
	 * @param <T>		the result type
	 * @param what		the asynchronous event waited for, for the timeout message
	 * @param condition	the condition: {@code null} or {@code false} keeps waiting; an exception it throws ends the wait
	 * @return the condition's result
	 * @throws IllegalStateException if the condition does not hold within {@link Selenide#TIMEOUT}
	 */
	private <T> @Nonnull T until(@Nonnull String what, @Nonnull java.util.function.Function<org.openqa.selenium.WebDriver, T> condition) {
		try {
			return waitFor(condition);
		}
		catch (org.openqa.selenium.TimeoutException e) {
			throw new IllegalStateException(String.format("Waited %d s for %s%s (browser state %s)",
															Long.valueOf(TIMEOUT.toSeconds()),
															what,
															(lastProblem == null) ? "" : ": " + lastProblem,
															state()), e);
		}
	}

	/**
	 * Signs in and waits for the outcome (see {@link Selenide#login(String, String, String)}).
	 * Signing in does not switch the UX/UI: {@code <switchMode/>} does.
	 */
	@Override
	public void login(@Nullable String customer, @Nonnull String username, @Nonnull String password) {
		super.login(customer, username, password);
		if (getLoginError() != null) {
			trace(String.format("    Sign in as %s failed: %s", username, getLoginError()));
		}
	}

	/**
	 * Makes the signed-in session use the SmartClient UX/UI, as a user would: desktop browsers are routed to
	 * PrimeFaces by default, so this clicks the PrimeFaces "Switch mode" menu item, which sets the session's
	 * UX/UI to {@code desktop} and reloads the page. Does nothing if SmartClient is already showing.
	 *
	 * @throws IllegalStateException if the page has no "Switch mode" control or SmartClient does not load
	 */
	public void switchToSmartClient() {
		if (isSmartClientPage()) {
			trace("    SmartClient is already showing");
			return;
		}
		// Real clicks, as a user would: the avatar opens the user menu, whose Switch Mode submenu holds "Switch mode"
		SelenideElement avatar = $("#avatar");
		if (! avatar.exists()) {
			throw new IllegalStateException(String.format(
					"Signed in, but %s (%s) is not SmartClient and has no user menu with Switch mode: can this user switch modes?",
					driver.getCurrentUrl(), driver.getTitle()));
		}
		avatar.shouldBe(visible).click();
		// Waits for the user menu's overlay panel to open (PrimeFaces shows it after the click, with an animation)
		SelenideElement overlay = $(".profile-overlay").shouldBe(visible);
		SelenideElement item = overlay.$$("a.ui-menuitem-link").findBy(com.codeborne.selenide.Condition.exactText("Switch mode"));
		if (! item.exists()) {
			throw new IllegalStateException("The user menu has no Switch mode item: this user cannot switch modes");
		}
		item.shouldBe(visible).click();
		trace("    Switched the session to the SmartClient UX/UI");

		// Waits for the reload the mode switch makes (an ajax request, then location.reload()) to bring up SmartClient
		until("SmartClient to load after switching mode", d -> Boolean.valueOf(isSmartClientPage()));
		waitForFullPageResponse();
	}

	/**
	 * Returns whether the current page runs the SmartClient desktop harness.
	 *
	 * @return {@code true} if {@code isc.BizUtil} is defined
	 */
	private boolean isSmartClientPage() {
		try {
			return Boolean.TRUE.equals(script("return document.readyState === 'complete' && !!(window.isc && window.isc.BizUtil && window.isc.WindowStack)"));
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			// The page is reloading
			return false;
		}
	}

	@Override
	public boolean button(SmartClientButtonCommand command) {
		Found found = require(command.locator());
		if (found.disabled()) {
			trace("    The button is disabled");
			return false;
		}
		requireShown(found, "button");
		SelenideElement element = found.element();
		prepare(element);
		click(element);

		waitForFullPageResponse();
		answer(command.confirm(), command.locator());

		return true;
	}

	/**
	 * Answers the confirmation questions an action asks, client or server side, one after another.
	 * {@code null} or {@code true} answers yes (or OK) to any question that shows; {@code false} answers
	 * no (or cancel), and then a question must show.
	 *
	 * @param confirm	how to answer
	 * @param what		what was clicked, for the message
	 * @return {@code true} if a question was answered
	 * @throws IllegalStateException if {@code confirm} is false and no question showed
	 */
	public boolean answer(@Nullable Boolean confirm, @Nonnull String what) {
		boolean yes = ! Boolean.FALSE.equals(confirm);
		int answered = 0;
		// an action can ask more than once (a server-side question can follow the client's confirmation)
		while (answered < 5) {
			waitForFullPageResponse();
			if (! "ask".equals(dialogKind())) {
				break;
			}
			String message = dialogMessage();
			Found button = findNow(Locator.sail(null, Kind.dialog, yes ? "yes" : "no", null, null, null, null));
			if (button == null) {
				button = findNow(Locator.sail(null, Kind.dialog, yes ? "ok" : "cancel", null, null, null, null));
			}
			if (button == null) {
				throw new IllegalStateException("A question is showing but it has no " + (yes ? "Yes or OK" : "No or Cancel") + " button: " + message);
			}
			trace("    Answer " + (yes ? "yes" : "no") + " to: " + message);
			click(button.element());
			answered++;
		}
		waitForFullPageResponse();
		if ((! yes) && (answered == 0)) {
			throw new IllegalStateException("Expected a confirmation to answer no to after clicking " + what + ", but none showed");
		}
		return answered > 0;
	}

	@Override
	public boolean checkbox(SmartClientCheckboxCommand command) {
		String locator = command.locator();
		Boolean value = command.value();

		Found found = require(locator);
		if (found.disabled()) {
			trace("    The checkbox is disabled");
			return false;
		}
		SelenideElement element = found.element();
		prepare(element);

		Boolean current = checkboxValue(locator, element);
		int attempts = 0;
		final int maxAttempts = 3; // tri-state checkboxes cycle through 3 states

		// Attempt until desired state is reached or max attempts reached
		while (! Objects.equals(current, value) && attempts < maxAttempts) {
			attempts++;
			click(element);
			waitForFullPageResponse();
			element = require(locator).element();
			current = checkboxValue(locator, element);
		}

		return Objects.equals(current, value);
	}

	/**
	 * Reads a checkbox's state from SmartClient, falling back to its ARIA state.
	 *
	 * @param locator	the checkbox locator
	 * @param element	the checkbox element
	 * @return the state, or {@code null} if it is unset
	 */
	private @Nullable Boolean checkboxValue(@Nonnull String locator, @Nonnull SelenideElement element) {
		if (locator.startsWith(Locator.SAIL_PREFIX)) {
			String value = value(locator);
			if ("true".equals(value)) {
				return Boolean.TRUE;
			}
			if ("false".equals(value)) {
				return Boolean.FALSE;
			}
			return null;
		}
		String aria = element.getAttribute("aria-checked");
		if ("true".equalsIgnoreCase(aria)) {
			return Boolean.TRUE;
		}
		if ("false".equalsIgnoreCase(aria)) {
			return Boolean.FALSE;
		}
		return null;
	}

	@Override
	public boolean dataGridButton(SmartClientDataGridButtonCommand command) {
		return toolButton(command.locator(), command.confirm());
	}

	@Override
	public boolean dataGridSelect(SmartClientDataGridSelectCommand command) {
		return row(command.locator(), command.row());
	}

	@Override
	public boolean listGridButton(SmartClientListGridButtonCommand command) {
		return toolButton(command.locator(), command.confirm());
	}

	@Override
	public boolean listGridSelect(SmartClientListGridSelectCommand command) {
		return row(command.locator(), command.row());
	}

	/**
	 * Clicks a grid tool button.
	 *
	 * @param locator	the button locator
	 * @param confirm	how to answer a confirmation that follows (see {@link #answer(Boolean, String)})
	 * @return {@code true} if clicked, {@code false} if the button is disabled
	 */
	private boolean toolButton(@Nonnull String locator, @Nullable Boolean confirm) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The grid button is disabled");
			return false;
		}
		requireShown(found, "grid button");
		SelenideElement element = found.element();
		prepare(element);
		click(element);

		waitForFullPageResponse();
		answer(confirm, locator);

		return true;
	}

	/**
	 * Strips the AutoTest suffix from a SAIL locator, leaving the widget's spec.
	 *
	 * @param locator	a SAIL locator
	 * @return the locator of the widget itself
	 */
	public static @Nonnull String root(@Nonnull String locator) {
		return locator.replaceFirst("\\}/.*$", "}");
	}

	/**
	 * Finds the first row of a grid that shows the text in one of its cells, waiting for the rows to load.
	 *
	 * @param locator	a locator of the grid's rows
	 * @param text		the text
	 * @return the zero-based row
	 * @throws IllegalStateException if no row shows the text
	 */
	public int rowContaining(@Nonnull String locator, @Nonnull String text) {
		String grid = root(locator);
		require(grid);
		waitForFullPageResponse();
		// Waits for the grid's rows to arrive (a list grid fetches them after it is drawn)
		Number row = until("the rows of " + grid + " to load", d -> {
			Map<String, Object> result = map(script("return _sail.rowIndex(arguments[0], arguments[1])", grid, text));
			if (result.get("row") instanceof Number found) {
				if (found.intValue() < 0) {
					throw new IllegalStateException(String.format("No row of %s contains '%s' (%s rows)", grid, text, result.get("rows")));
				}
				return found;
			}
			lastProblem = String.valueOf(result.get("error"));
			return null;
		});
		trace(String.format("    Row %d contains '%s'", Integer.valueOf(row.intValue()), text));
		return row.intValue();
	}

	/**
	 * Counts a grid's rows, or the rows that show the text.
	 *
	 * @param locator	a locator of the grid's rows
	 * @param text		the text, or {@code null} to count every row
	 * @return the count, or -1 while the rows are loading
	 */
	public int countRows(@Nonnull String locator, @Nullable String text) {
		String grid = root(locator);
		require(grid);
		waitForFullPageResponse();
		// Waits for the grid's rows to arrive (a list grid fetches them after it is drawn, or after a refresh)
		Number count = until("the rows of " + grid + " to load", d -> {
			Map<String, Object> result = map(script("return _sail.countRows(arguments[0], arguments[1])", grid, text));
			Object error = result.get("error");
			if (error != null) {
				throw new IllegalStateException("Could not count the rows of " + grid + ": " + error);
			}
			return Boolean.TRUE.equals(result.get("loaded")) ? (Number) result.get("count") : null;
		});
		return count.intValue();
	}

	/**
	 * Counts a grid's rows (those showing the text, if given) until the count holds, fetching a list grid's rows again
	 * between counts, for at most the script's timeout.
	 *
	 * @param locator		a locator of the grid's rows
	 * @param text			the text, or {@code null}
	 * @param holds			whether a count holds
	 * @param timeoutMillis	the script's timeout
	 * @return the last count
	 */
	public int awaitRows(@Nonnull String locator,
							@Nullable String text,
							@Nonnull java.util.function.IntPredicate holds,
							long timeoutMillis) {
		int[] count = {countRows(locator, text)};
		if (holds.test(count[0]) || (timeoutMillis <= 0)) {
			return count[0];
		}
		try {
			// The script asked to wait for the server to change the rows (a job finishing, say). A list grid only sees the
			// change when it fetches again, so refetch and recount, once a second, until the count holds or the time is up.
			waitFor(Duration.ofMillis(timeoutMillis), Duration.ofSeconds(1), d -> {
				refreshGrid(locator);
				count[0] = countRows(locator, text);
				return Boolean.valueOf(holds.test(count[0]));
			});
		}
		catch (@SuppressWarnings("unused") org.openqa.selenium.TimeoutException e) {
			// the caller reports the last count
		}
		return count[0];
	}

	/**
	 * Fetches a list grid's rows again (data grids show their bean's rows and are left alone).
	 *
	 * @param locator	a locator of the grid's rows
	 */
	public void refreshGrid(@Nonnull String locator) {
		script("return _sail.refreshGrid(arguments[0])", root(locator));
		waitForFullPageResponse();
	}

	/**
	 * Waits for a list grid's rows to be loaded, and returns its filter and sort state.
	 *
	 * @param grid	the list grid's SAIL locator, without a suffix
	 * @return the state: {@code advanced}, {@code criteria}, {@code filtered} and {@code sort}
	 */
	private @Nonnull Map<String, Object> listState(@Nonnull String grid) {
		require(grid);
		waitForFullPageResponse();
		// Waits for the list grid's rows to arrive (it fetches them after it is drawn)
		return until("the rows of " + grid + " to load", d -> {
			Map<String, Object> state = map(script("return _sail.listState(arguments[0])", grid));
			if (state.get("error") != null) {
				lastProblem = String.valueOf(state.get("error"));
				return null;
			}
			return Boolean.TRUE.equals(state.get("loaded")) ? state : null;
		});
	}

	/**
	 * Waits for a list grid to show the rows a filter or sort gesture asked for: the page has settled, the grid's
	 * criteria or sort have changed from what they were before the gesture (when the gesture changes them) and its rows
	 * have arrived. A grid that has all the rows filters and sorts them itself, without a fetch.
	 *
	 * @param grid		the list grid's SAIL locator, without a suffix
	 * @param key		{@code criteria} or {@code sort}
	 * @param before	the value of the key before the gesture
	 * @param change	whether the gesture changes the value
	 * @param what		the event, for the timeout message
	 */
	private void awaitList(@Nonnull String grid, @Nonnull String key, @Nullable Object before, boolean change, @Nonnull String what) {
		waitForFullPageResponse();
		// Waits for the grid to apply the gesture: its criteria (or sort) change, then its rows arrive
		until(what, d -> {
			waitForFullPageResponse();
			Map<String, Object> state = map(script("return _sail.listState(arguments[0])", grid));
			if (state.get("error") != null) {
				lastProblem = String.valueOf(state.get("error"));
				return null;
			}
			if (change && Objects.equals(before, state.get(key))) {
				lastProblem = "the grid still has its " + key + " " + before;
				return null;
			}
			lastProblem = "the grid's rows are loading";
			return Boolean.TRUE.equals(state.get("loaded")) ? Boolean.TRUE : null;
		});
		lastProblem = null;
	}

	/**
	 * Filters a column of a list grid as a user does, or clears the column's filter, and waits for the rows.
	 * <p>
	 * In simple filtering, a column whose filter row cell applies the operator (substring for text, equality for
	 * anything else), or a column being cleared, is filtered in the filter row: click the cell, replace its text, press the
	 * row's Filter button. Any other operator is applied with the grid's advanced filter: the grid's toggle switches to
	 * advanced filtering (keeping the filter row's filters as clauses), the clauses are set as a saved snapshot sets them,
	 * and the advanced filter's Filter button is pressed. A grid in advanced filtering stays in it.
	 *
	 * @param locator	a locator of the list grid's rows
	 * @param column	the column's field name (its sanitised binding)
	 * @param value		the value, or {@code null} to clear the column's filter
	 * @param operator	the SAIL operator: contains, startsWith, equals, notEquals, lessThan or greaterThan
	 */
	public void filterListGrid(@Nonnull String locator, @Nonnull String column, @Nullable String value, @Nonnull String operator) {
		String grid = root(locator);
		Object before = listState(grid).get("criteria");
		// Waits for the grid's filter row or advanced filter to be drawn
		Map<String, Object> plan = until("the filter of " + grid + " to show", d -> {
			Map<String, Object> result = map(script("return _sail.listFilterPlan(arguments[0], arguments[1], arguments[2], arguments[3])",
														grid, column, value, operator));
			Object error = result.get("error");
			if (error != null) {
				if (! Boolean.TRUE.equals(result.get("retry"))) {
					throw new IllegalStateException("Cannot filter " + grid + ": " + error);
				}
				lastProblem = String.valueOf(error);
				return null;
			}
			return result;
		});
		boolean change = "row".equals(plan.get("mode")) ? filterInRow(plan, column, value) : filterInAdvanced(grid, plan, operator);
		awaitList(grid, "criteria", before, change, "the list grid to fetch its rows with the new filter");
	}

	/**
	 * Types a column's filter into the grid's filter row and presses the row's Filter button.
	 *
	 * @param plan		the browser helper's plan: the cell's input, the Filter button and the cell's current text
	 * @param column	the column, for the trace
	 * @param value		the value, or {@code null} to clear the cell
	 * @return whether the filter changes
	 */
	private boolean filterInRow(@Nonnull Map<String, Object> plan, @Nonnull String column, @Nullable String value) {
		String current = String.valueOf(plan.get("current"));
		trace(String.format("    In the filter row, %s column %s", (value == null) ? "clear" : "type '" + value + "' into", column));
		SelenideElement input = $((WebElement) plan.get("input"));
		prepare(input);
		click(input);
		// select what the cell holds (through the DOM) so that the keys replace it
		script("if (arguments[0].select) arguments[0].select();", input.toWebElement());
		new org.openqa.selenium.interactions.Actions(driver).sendKeys(Keys.BACK_SPACE).perform();
		if ((value != null) && (! value.isEmpty())) {
			new org.openqa.selenium.interactions.Actions(driver).sendKeys(value).perform();
		}
		trace("    Press the filter row's Filter button");
		SelenideElement button = $((WebElement) plan.get("button"));
		prepare(button);
		click(button);
		return ! current.equals((value == null) ? "" : value);
	}

	/**
	 * Filters with the grid's advanced filter: switch the grid to it if needed, set its clauses and press its Filter button.
	 *
	 * @param grid		the list grid's SAIL locator, without a suffix
	 * @param plan		the browser helper's plan: the toggle (when the grid is in simple filtering) and the criteria
	 * @param operator	the operator, for the trace
	 * @return {@code true}: the criteria change
	 */
	private boolean filterInAdvanced(@Nonnull String grid, @Nonnull Map<String, Object> plan, @Nonnull String operator) {
		if (plan.get("toggle") instanceof WebElement toggle) {
			trace("    Switch the grid to advanced filtering, for operator " + operator);
			SelenideElement element = $(toggle);
			prepare(element);
			click(element);
			// switching fetches the rows with the advanced filter's (empty) criteria
			waitForFullPageResponse();
		}
		trace("    Set the advanced filter's clauses to " + plan.get("criteria"));
		Map<String, Object> result = map(script("return _sail.listAdvancedCriteria(arguments[0], arguments[1])", grid, plan.get("criteria")));
		if (! (result.get("button") instanceof WebElement filter)) {
			throw new IllegalStateException("Cannot filter " + grid + " with the advanced filter: " + result.get("error"));
		}
		trace("    Press the advanced filter's Filter button");
		SelenideElement element = $(filter);
		prepare(element);
		click(element);
		return true;
	}

	/**
	 * Clears every filter of a list grid with its toolbar's Clear button, and waits for the rows.
	 *
	 * @param locator	a locator of the list grid's rows
	 */
	public void clearListGridFilters(@Nonnull String locator) {
		String grid = root(locator);
		Map<String, Object> state = listState(grid);
		Map<String, Object> result = map(script("return _sail.listClearButton(arguments[0])", grid));
		if (! (result.get("button") instanceof WebElement clear)) {
			throw new IllegalStateException("Cannot clear the filters of " + grid + ": " + result.get("error"));
		}
		if (Boolean.TRUE.equals(result.get("disabled"))) {
			throw new IllegalStateException("The Clear filter button of " + grid + " is disabled");
		}
		trace("    Press the list grid's Clear filter button");
		SelenideElement element = $(clear);
		prepare(element);
		click(element);
		awaitList(grid, "criteria", state.get("criteria"), Boolean.TRUE.equals(state.get("filtered")),
					"the list grid to fetch its rows without filters");
	}

	/**
	 * Sorts a list grid by one column, as a user does: click the column's header until the grid is sorted by that
	 * column alone, in the direction wanted. The first click sorts by the column alone; the next turns its direction.
	 *
	 * @param locator		a locator of the list grid's rows
	 * @param column		the column's field name (its sanitised binding)
	 * @param descending	the direction
	 */
	public void sortListGrid(@Nonnull String locator, @Nonnull String column, boolean descending) {
		String grid = root(locator);
		for (int clicks = 0; clicks < 3; clicks++) {
			Object before = listState(grid).get("sort");
			Map<String, Object> plan = map(script("return _sail.listSortPlan(arguments[0], arguments[1])", grid, column));
			if (plan.get("error") != null) {
				throw new IllegalStateException("Cannot sort " + grid + ": " + plan.get("error"));
			}
			if (Boolean.TRUE.equals(plan.get("sorted")) && (descending == Boolean.TRUE.equals(plan.get("descending")))) {
				trace(String.format("    Sorted by %s %s", column, descending ? "descending" : "ascending"));
				return;
			}
			if (! Boolean.TRUE.equals(plan.get("canSort"))) {
				throw new IllegalStateException("The list grid " + grid + " cannot be sorted by " + column);
			}
			if (! (plan.get("header") instanceof WebElement header)) {
				throw new IllegalStateException("The list grid " + grid + " shows no header for " + column);
			}
			trace("    Click the header of " + column);
			SelenideElement element = $(header);
			prepare(element);
			click(element);
			awaitList(grid, "sort", before, true, "the list grid to show its rows sorted by " + column);
		}
		throw new IllegalStateException(String.format("Clicking the header of %s did not sort %s by it alone, %s", column, grid,
														descending ? "descending" : "ascending"));
	}

	/**
	 * Returns the position of the combo or radio option whose code or label is the value, as {@code dataEnter} takes it.
	 *
	 * @param locator	the combo's or radio group's locator
	 * @param value		the code or label
	 * @param byCode	{@code true} to match the code, {@code false} the label
	 * @return the zero-based position
	 * @throws IllegalStateException if no option, or more than one, matches
	 */
	public int optionIndex(@Nonnull String locator, @Nonnull String value, boolean byCode) {
		require(root(locator));
		Map<String, Object> result = map(script("return _sail.optionIndex(arguments[0], arguments[1], arguments[2])",
													root(locator), value, Boolean.valueOf(byCode)));
		if (result.get("index") instanceof Number index) {
			return index.intValue();
		}
		throw new IllegalStateException(String.format("Cannot choose an option of %s: %s (the options, code=label, are %s)",
														locator, result.get("error"), result.get("options")));
	}

	/**
	 * Picks the first row of a lookup description's drop-down, in the order shown, that shows the text in a cell:
	 * open the drop-down with its picker (or type the search into the lookup, which opens it), then click the row.
	 *
	 * @param locator	the lookup description's locator
	 * @param search	the text to type, or {@code null} to open the drop-down with its picker
	 * @param text		the text the row contains (case-sensitive)
	 * @return {@code true} once picked, {@code false} if the lookup is disabled
	 * @throws IllegalStateException if no row shows the text
	 */
	public boolean lookupDescriptionContaining(@Nonnull String locator, @Nullable String search, @Nonnull String text) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The lookup description is disabled");
			return false;
		}
		prepare(found.element());

		String combo = String.format("%s/item[Class=ComboBoxItem||name=_combo]", locator);
		if (search == null) {
			SelenideElement picker = require(combo + "/[icon=\"picker\"]").element();
			prepare(picker);
			click(picker);
		}
		else if (! text(new SmartClientTextCommand(combo + "/element", search, true))) {
			return false;
		}

		waitForFullPageResponse();
		// Waits for SmartClient to show the drop-down with its rows (the fetch the picker or the search started has returned)
		Number row = until("the lookup's drop-down to show its rows", d -> {
			Map<String, Object> result = map(script("return _sail.pickListRow(arguments[0], arguments[1])", root(locator), text));
			if (result.get("row") instanceof Number number) {
				if (number.intValue() < 0) {
					throw new IllegalStateException(String.format("No row of the drop-down of %s contains '%s' (%s rows)",
																	locator, text, result.get("rows")));
				}
				return number;
			}
			lastProblem = String.valueOf(result.get("error"));
			return null;
		});
		trace(String.format("    Row %d contains '%s'", Integer.valueOf(row.intValue()), text));
		SelenideElement element = require(String.format("%s/pickList/body/row[%d]/col[0]", combo, Integer.valueOf(row.intValue()))).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	/**
	 * Reports whether a widget is on the view, shown and enabled, without failing if it is not there.
	 *
	 * @param locator	the widget's locator
	 * @return found, shown and enabled
	 */
	public @Nonnull boolean[] widgetState(@Nonnull String locator) {
		waitForFullPageResponse();
		// Waits for the view the locator expects, as when locating: on another view the widget's state is unknown,
		// which must fail the step rather than read as "not shown"
		Map<String, Object> result = until("the view of " + locator + " to show", d -> {
			Map<String, Object> state = map(script("return _sail.widgetState(arguments[0])", root(locator)));
			return Boolean.TRUE.equals(state.get("wrongView")) ? null : state;
		});
		return new boolean[] {Boolean.TRUE.equals(result.get("found")),
								Boolean.TRUE.equals(result.get("shown")),
								Boolean.TRUE.equals(result.get("enabled"))};
	}

	/**
	 * Lists the messages showing: dialogs (information, warning, or the Problems dialog as an error),
	 * field errors and growls.
	 *
	 * @param type	{@code info}, {@code warning} or {@code error}, or {@code null} for any
	 * @return the messages' text
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<String> messages(@Nullable String type) {
		waitForFullPageResponse();
		List<String> result = new java.util.ArrayList<>();
		Object messages = script("return _sail.messages()");
		if (messages instanceof List<?> list) {
			for (Object message : (List<Object>) list) {
				if (message instanceof Map<?, ?> m) {
					if ((type == null) || type.equals(m.get("type"))) {
						result.add(String.valueOf(m.get("text")));
					}
				}
			}
		}
		return result;
	}

	/**
	 * Finds a menu entry in the SmartClient menu, and optionally clicks it.
	 *
	 * @param moduleTitle	the module's title (its menu section)
	 * @param labels		the shown text of the groups and the item, outermost first; empty for the module itself
	 * @param click			whether to click the entry
	 * @return {@code true} if the entry is in the menu
	 */
	public boolean menu(@Nonnull String moduleTitle, @Nonnull List<String> labels, boolean click) {
		waitForFullPageResponse();
		Map<String, Object> result = map(script("return _sail.menu(arguments[0], arguments[1])", moduleTitle, labels));
		if (! Boolean.TRUE.equals(result.get("present"))) {
			trace("    " + result.get("why"));
			return false;
		}
		if (! click) {
			return true;
		}
		if ((! Boolean.TRUE.equals(result.get("expanded"))) && (result.get("header") instanceof WebElement header)) {
			trace("    Open the " + moduleTitle + " menu");
			SelenideElement element = $(header);
			prepare(element);
			click(element);
			waitForFullPageResponse();
			result = map(script("return _sail.menu(arguments[0], arguments[1])", moduleTitle, labels));
		}
		// Open each closed group on the way, as a user does: double-click its row (a menu tree has no open icons)
		for (int i = 0; (i < labels.size()) && (result.get("folder") != null); i++) {
			if (! (result.get("folderRow") instanceof WebElement folder)) {
				throw new IllegalStateException("The menu group " + result.get("folder") + " is not shown");
			}
			trace("    Open the " + result.get("folder") + " menu group");
			SelenideElement element = $(folder);
			prepare(element);
			doubleClick(element);
			waitForFullPageResponse();
			result = map(script("return _sail.menu(arguments[0], arguments[1])", moduleTitle, labels));
		}
		if (! (result.get("row") instanceof WebElement row)) {
			throw new IllegalStateException("The menu entry " + moduleTitle + " / " + String.join(" / ", labels) + " is not shown");
		}
		SelenideElement element = $(row);
		prepare(element);
		click(element);
		waitForFullPageResponse();
		return true;
	}

	/**
	 * Sets a list membership's members to exactly the given descriptions, moving rows between the lists as a user would:
	 * select a row, then press the assign or unassign button.
	 *
	 * @param locator	the list membership's locator
	 * @param value		the members' descriptions, separated by {@code |}
	 * @return {@code true} once the members are as given
	 */
	@SuppressWarnings("unchecked")
	public boolean membership(@Nonnull String locator, @Nullable String value) {
		require(locator);
		List<String> wanted = new java.util.ArrayList<>();
		if (value != null) {
			for (String part : value.split("\\|")) {
				if (! part.isBlank()) {
					wanted.add(part.trim());
				}
			}
		}
		Map<String, Object> state = map(script("return _sail.membership(arguments[0])", locator));
		List<String> members = (List<String>) state.get("members");
		List<String> candidates = (List<String>) state.get("candidates");
		if ((members == null) || (candidates == null)) {
			throw new IllegalStateException("Could not read list membership " + locator + ": " + state.get("error"));
		}
		for (String member : new java.util.ArrayList<>(members)) {
			if (! wanted.contains(member)) {
				move(locator, "member", member, "unassign");
			}
		}
		for (String want : wanted) {
			if (! members.contains(want)) {
				if (! candidates.contains(want)) {
					throw new IllegalStateException(String.format("'%s' is not a candidate of %s; the candidates are %s", want, locator, candidates));
				}
				move(locator, "candidate", want, "assign");
			}
		}
		state = map(script("return _sail.membership(arguments[0])", locator));
		members = (List<String>) state.get("members");
		return (members != null) && new java.util.HashSet<>(members).equals(new java.util.HashSet<>(wanted));
	}

	/**
	 * Selects a row of one of a list membership's lists and presses a button.
	 *
	 * @param locator	the list membership's locator
	 * @param list		{@code candidate} or {@code member}
	 * @param text		the row's description
	 * @param button	{@code assign} or {@code unassign}
	 */
	private void move(@Nonnull String locator, @Nonnull String list, @Nonnull String text, @Nonnull String button) {
		trace(String.format("    %s '%s'", "assign".equals(button) ? "Assign" : "Unassign", text));
		Object row = script("return _sail.membershipElement(arguments[0], arguments[1], arguments[2])", locator, list, text);
		if (! (row instanceof WebElement rowElement)) {
			throw new IllegalStateException(String.format("No %s row '%s' in %s", list, text, locator));
		}
		SelenideElement element = $(rowElement);
		prepare(element);
		click(element);
		waitForFullPageResponse();
		Object press = script("return _sail.membershipElement(arguments[0], arguments[1])", locator, button);
		if (! (press instanceof WebElement pressElement)) {
			throw new IllegalStateException("No " + button + " button in " + locator);
		}
		element = $(pressElement);
		prepare(element);
		click(element);
		waitForFullPageResponse();
	}

	/**
	 * Replaces an HTML item's content: press its Edit button, type into the editor window's editor, press Apply.
	 *
	 * @param locator	the HTML item's locator
	 * @param value		the text to type
	 * @return {@code true} once applied
	 */
	public boolean html(@Nonnull String locator, @Nullable String value) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The HTML editor is disabled");
			return false;
		}
		Map<String, Object> editor = map(script("return _sail.htmlEditor(arguments[0])", locator));
		if (! (editor.get("edit") instanceof WebElement edit)) {
			throw new IllegalStateException("No Edit button for " + locator + ": " + editor.get("error"));
		}
		SelenideElement element = $(edit);
		prepare(element);
		click(element);
		// Waits for the editor window: CKEditor's script loads on first use, then its instance and iframe become ready
		Map<String, Object> open = until("the HTML editor of " + locator + " to be ready", d -> {
			Map<String, Object> state = map(script("return _sail.htmlEditor(arguments[0])", locator));
			return ((state.get("frame") instanceof WebElement) && (state.get("apply") instanceof WebElement)) ? state : null;
		});
		WebElement frame = (WebElement) open.get("frame");
		WebElement apply = (WebElement) open.get("apply");
		driver.switchTo().frame(frame);
		try {
			WebElement body = driver.findElement(org.openqa.selenium.By.tagName("body"));
			body.click();
			script("document.execCommand('selectAll', false, null);");
			body.sendKeys(Keys.DELETE);
			if ((value != null) && (! value.isEmpty())) {
				body.sendKeys(value);
			}
		}
		finally {
			driver.switchTo().defaultContent();
		}
		element = $(apply);
		prepare(element);
		click(element);
		waitForFullPageResponse();
		return true;
	}

	/**
	 * Uploads a file through Skyve's upload page: click the trigger (a content item's Upload button, or an upload action),
	 * choose the file in the page's file input, press its Upload button (the image editor's, for an image), and wait for the
	 * page's window to close.
	 *
	 * @param trigger	the element that opens the upload page
	 * @param file		the file
	 * @throws IllegalStateException if the upload page does not open, reports an error or does not close
	 */
	public void upload(@Nonnull WebElement trigger, @Nonnull java.io.File file) {
		SelenideElement element = $(trigger);
		prepare(element);
		click(element);
		// Waits for the upload window's page (upload.xhtml, in an iframe) to open
		WebElement frame = until("the upload page to open", d -> (WebElement) script("return _sail.uploadFrame()"));
		trace("    upload page opened");
		// Waits for the upload page's file input, then for its Upload button to enable once it has the file
		driver.switchTo().frame(frame);
		try {
			WebElement input = waitFor(d -> {
				List<WebElement> inputs = d.findElements(org.openqa.selenium.By.cssSelector("input[type=file]"));
				return inputs.isEmpty() ? null : inputs.get(0);
			});
			input.sendKeys(file.getAbsolutePath());
			trace("    file chosen");
			// Waits for the page to offer its Upload button for the file: the file list's (enabled once it has the file),
			// or, for an image, the image editor's (#crop, shown once the editor has the image); or for the page to have
			// taken the file by itself (no file input left)
			WebElement upload = waitFor(d -> {
				for (WebElement button : d.findElements(org.openqa.selenium.By.cssSelector(".ui-fileupload-upload, #crop"))) {
					String styleClass = button.getDomAttribute("class");
					if (button.isDisplayed() && button.isEnabled() &&
							((styleClass == null) || (! styleClass.contains("ui-state-disabled")))) {
						return button;
					}
				}
				return d.findElements(org.openqa.selenium.By.cssSelector("input[type=file]")).isEmpty() ? input : null;
			});
			if (upload != input) {
				trace("    press " + (("crop".equals(upload.getDomAttribute("id"))) ? "the image editor's Upload" : "Upload"));
				upload.click();
			}
		}
		finally {
			driver.switchTo().defaultContent();
		}
		trace("    upload pressed");
		// Waits for the upload to be processed: the upload page closes its window (content uploads), or reports the
		// outcome in its own messages (upload actions), or shows why it could not
		WebElement uploadFrame = frame;
		String outcome = until("the upload to finish", d -> {
			if (! (script("return _sail.uploadFrame()") instanceof WebElement)) {
				return "closed";
			}
			List<String> result = uploadPageOutcome(uploadFrame);
			if (result.isEmpty()) {
				return null;
			}
			if ("error".equals(result.get(0))) {
				throw new IllegalStateException("The upload failed: " + result.get(1));
			}
			return result.get(1);
		});
		if (! "closed".equals(outcome)) {
			trace("    the upload page says: " + outcome + " - close it");
			if (! (script("return _sail.windowClose()") instanceof WebElement close)) {
				throw new IllegalStateException("The upload window has no close button");
			}
			click($(close));
		}
		waitForFullPageResponse();
	}

	/**
	 * Reads the outcome the upload page shows: an error, or an information message.
	 *
	 * @param frame	the upload page's iframe
	 * @return {severity, text}, or empty while it shows neither
	 */
	@SuppressWarnings("unchecked")
	private @Nonnull List<String> uploadPageOutcome(@Nonnull WebElement frame) {
		try {
			driver.switchTo().frame(frame);
			Object result = script("var e = Array.from(document.querySelectorAll('.ui-growl-error, .ui-growl-fatal, .ui-messages-error')).find(function(x) {" +
									"  return x.getClientRects().length > 0 && (x.innerText || '').trim();});" +
									"if (e) return ['error', e.innerText.replace(/\\s+/g, ' ').trim()];" +
									"var i = Array.from(document.querySelectorAll('.ui-growl-info, .ui-messages-info')).find(function(x) {" +
									"  return x.getClientRects().length > 0 && (x.innerText || '').trim();});" +
									"if (i) return ['info', i.innerText.replace(/\\s+/g, ' ').trim()];" +
									"return [];");
			return (result instanceof List<?> list) ? (List<String>) list : List.of();
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			return List.of();
		}
		finally {
			driver.switchTo().defaultContent();
		}
	}

	/**
	 * Finds a content item's Upload button.
	 *
	 * @param locator	the content item's locator
	 * @return the button
	 * @throws IllegalStateException if the item has no enabled Upload button
	 */
	public @Nonnull WebElement uploadButton(@Nonnull String locator) {
		require(locator);
		waitForFullPageResponse();
		Map<String, Object> result = map(script("return _sail.uploadButton(arguments[0])", locator));
		if ((result.get("element") instanceof WebElement button) && (! Boolean.TRUE.equals(result.get("disabled")))) {
			return button;
		}
		throw new IllegalStateException("No enabled Upload button for " + locator + ": " + result.get("error"));
	}

	/**
	 * Returns the row a data grid is editing inline.
	 *
	 * @param locator	a locator of the grid or one of its parts
	 * @return the zero-based row, or -1 if no row is being edited
	 */
	public int editRow(@Nonnull String locator) {
		waitForFullPageResponse();
		Object row = script("return _sail.editRow(arguments[0])", locator.replaceFirst("\\}/.*$", "}"));
		return (row instanceof Number number) ? number.intValue() : -1;
	}

	/**
	 * Clicks the first cell of a grid row.
	 *
	 * @param locator	the row locator, with {@code %d} placeholders for the row and column
	 * @param row		the zero-based row
	 * @return {@code true} once clicked
	 */
	private boolean row(@Nonnull String locator, int row) {
		int column = 0;
		if (locator.startsWith(Locator.SAIL_PREFIX)) {
			String grid = locator.replaceFirst("\\}/.*$", "}");
			require(grid);
			Object index = script("return _sail.dataColumn(arguments[0])", grid);
			column = (index instanceof Number number) ? number.intValue() : 0;
			if (Boolean.TRUE.equals(script("return _sail.rowSelected(arguments[0], arguments[1])", grid, Integer.valueOf(row)))) {
				trace("    Row " + row + " is already selected");
				return true;
			}
		}
		if (locator.startsWith(Locator.SAIL_PREFIX)) {
			// the row may be scrolled out of the grid's body: bring it into view the grid's way first
			script("return _sail.scrollToRow(arguments[0], arguments[1])", root(locator), Integer.valueOf(row));
			waitForFullPageResponse();
		}
		String cell = String.format(locator, Integer.valueOf(row), Integer.valueOf(column));
		SelenideElement element = require(cell).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	@Override
	public boolean lookupDescriptionByRow(SmartClientLookupDescriptionByRowCommand command) {
		String locator = command.locator();
		int row = command.row();

		Found found = require(locator);
		if (found.disabled()) {
			trace("    The lookup description is disabled");
			return false;
		}
		prepare(found.element());

		// Find the drop down button
		SelenideElement element = require(String.format("%s/item[Class=ComboBoxItem||name=_combo]/[icon=\"picker\"]", locator)).element();
		prepare(element);
		click(element);

		// Wait for the pick list drop down
		require(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator)).element().shouldBe(visible);

		// Value here should be an index in the drop down starting from 0
		element = require(String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList/body/row[%d]/col[0]",
											locator,
											Integer.valueOf(row))).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	@Override
	public boolean lookupDescriptionBySearch(SmartClientLookupDescriptionBySearchCommand command) {
		String locator = command.locator();
		String search = command.search();

		Found found = require(locator);
		if (found.disabled()) {
			trace("    The lookup description is disabled");
			return false;
		}
		prepare(found.element());

		// Enter search
		String searchBarLocator = String.format("%s/item[Class=ComboBoxItem||name=_combo]/element", locator);
		if (! text(new SmartClientTextCommand(searchBarLocator, search, true))) {
			return false;
		}

		// Wait for the pick list drop down
		String pickList = String.format("%s/item[Class=ComboBoxItem||name=_combo]/pickList", locator);
		require(pickList);
		// Waits for SmartClient to show the pick list it has drawn (the search's fetch has returned by now)
		until("the lookup's pick list to show", d -> {
			Found list = find(pickList);
			return ((list != null) && list.shown()) ? Boolean.TRUE : null;
		});
		SelenideElement element;

		// Select the first row
		element = require(pickList + "/body/row[0]/col[0]").element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	@Override
	public boolean radio(SmartClientRadioCommand command) {
		String locator = command.locator();
		int index = command.index();

		Found group = require(locator);
		if (group.disabled()) {
			trace("    The radio group is disabled");
			return false;
		}
		SelenideElement element = require(String.format("%s/item[index=%d]/element", locator, Integer.valueOf(index))).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	@Override
	public boolean tab(SmartClientTabCommand command) {
		Found found = require(command.locator());
		if (found.disabled()) {
			trace("    The tab is disabled");
			return false;
		}
		requireShown(found, "tab");
		SelenideElement element = found.element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	@Override
	public boolean text(SmartClientTextCommand command) {
		String locator = command.locator();
		String value = command.value();

		Found found = require(locator);
		SelenideElement element = found.element();
		prepare(element);

		if (found.disabled() ||
				(! element.isEnabled()) ||
				(element.attr("disabled") != null) ||
				(element.attr("readonly") != null)) {
			trace("    The field is disabled or read only");
			return false;
		}

		// Type as a user would: click into the field, select what is there (the selection itself is set through the DOM),
		// delete it, then type the value (key by key when a change handler runs, see below), so that SmartClient sees
		// every change (setting the DOM value directly can leave the item's value stale)
		click(element);
		script("if (arguments[0].select) arguments[0].select();", element.toWebElement());
		// Keys go to whatever has the focus, as from a keyboard: a change handler can rerender the field's form while
		// typing, and SmartClient gives the new field the focus, so the rest of the text must follow it
		new org.openqa.selenium.interactions.Actions(driver).sendKeys(Keys.BACK_SPACE).perform();
		if ((value != null) && (! value.isEmpty())) {
			if (locator.startsWith(Locator.SAIL_PREFIX) && Boolean.TRUE.equals(script("return _sail.hasChangeHandler(arguments[0])", root(locator)))) {
				// The field's change handler can call the server on a keystroke, and the response sets the field to the value
				// it was sent: a burst of keys would be overwritten. So type a key, wait for what it started, type the next.
				for (int i = 0, l = value.length(); i < l; i++) {
					new org.openqa.selenium.interactions.Actions(driver).sendKeys(String.valueOf(value.charAt(i))).perform();
					waitForFullPageResponse();
				}
			}
			else {
				new org.openqa.selenium.interactions.Actions(driver).sendKeys(value).perform();
			}
		}
		boolean cell = locator.contains("\"kind\":\"cell\"");
		if ((! command.keyPresses()) && (! cell)) {
			// leave the field, so that SmartClient commits the value and runs its change handlers
			// (not in an inline grid editor, where leaving the last cell ends the row's edit).
			// A change handler can rerender the field's form while typing, so find the field again first.
			waitForFullPageResponse();
			element = require(locator).element();
			element.sendKeys(Keys.TAB);
			// SmartClient can keep focus in the field (a modal window's only field takes the Tab back). Then the next
			// click would blur it first: the field re-validates, the form re-lays out under the pointer and the click is
			// lost. So make sure the field has been left before going on.
			script("if (document.activeElement === arguments[0]) arguments[0].blur();", element.toWebElement());
			waitForFullPageResponse();
		}
		if ((! command.keyPresses()) && locator.startsWith(Locator.SAIL_PREFIX)) {
			commit(locator, value);
		}

		return true;
	}

	/**
	 * Replaces the content of a rich text editor by typing into its editable area.
	 *
	 * @param locator	the locator of the editable area
	 * @param value		the text to type
	 * @return {@code true} if typed, {@code false} if the editor is disabled
	 */
	public boolean richText(@Nonnull String locator, @Nullable String value) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The rich text editor is disabled");
			return false;
		}
		SelenideElement element = found.element();
		prepare(element);
		click(element);
		// select everything in the editable area so that typing replaces it
		script("""
				const e = arguments[0];
				e.focus();
				const range = document.createRange();
				range.selectNodeContents(e);
				const selection = window.getSelection();
				selection.removeAllRanges();
				selection.addRange(range);""", element);
		element.sendKeys(Keys.DELETE);
		if (value != null) {
			element.sendKeys(value);
		}
		// leave the editor so SmartClient saves its value
		element.sendKeys(Keys.TAB);
		waitForFullPageResponse();
		commit(locator, value);
		return true;
	}

	/**
	 * Makes sure what was typed reached the item and the form's ValuesManager, as leaving the field does,
	 * before the script goes on. Leaving the field (Tab) normally does this, but without OS focus a headed
	 * browser may not deliver the blur, and a following Save would then validate the old value.
	 *
	 * @param locator	the locator of the element typed into
	 * @param value		what was typed
	 * @throws IllegalStateException if the field did not take the text, or the value did not reach the form
	 */
	private void commit(@Nonnull String locator, @Nullable String value) {
		Map<String, Object> result = map(script("return _sail.commit(arguments[0])", locator));
		Object error = result.get("error");
		if (error != null) {
			throw new IllegalStateException("Could not commit " + locator + ": " + error);
		}
		if (! Boolean.TRUE.equals(result.get("focused"))) {
			trace("    (the browser window does not have focus; committed the value through SmartClient)");
		}
		String element = String.valueOf(result.get("element"));
		if ((value != null) && (! value.isEmpty()) && element.isEmpty()) {
			throw new IllegalStateException(String.format("Typed '%s' into %s but the field shows nothing", value, locator));
		}
		if ((value != null) && Boolean.TRUE.equals(result.get("plain")) && (! value.equals(element))) {
			throw new IllegalStateException(String.format("Typed '%s' into %s but the field shows '%s'", value, locator, element));
		}
		if (! Boolean.TRUE.equals(result.get("committed"))) {
			throw new IllegalStateException(String.format("Typed '%s' into %s but the value (%s) did not reach the form's values",
															value, locator, result.get("stored")));
		}
		waitForFullPageResponse();
	}

	/**
	 * Sets a slider. The thumb is scrolled into view and checked like any other widget, then the value
	 * is set through SmartClient's {@code Slider.setValue()}, which fires the same change events as a drag.
	 *
	 * @param locator	the slider item locator
	 * @param value		the numeric value
	 * @return {@code true} if the slider took the value
	 */
	public boolean slider(@Nonnull String locator, @Nonnull String value) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The slider is disabled");
			return false;
		}
		prepare(found.element());
		Map<String, Object> result = map(script("return _sail.slide(arguments[0], arguments[1])", locator, value));
		Object error = result.get("error");
		if (error != null) {
			throw new IllegalStateException("Could not set slider " + locator + ": " + error);
		}
		waitForFullPageResponse();
		Object set = result.get("value");
		try {
			return (set != null) && (Double.parseDouble(String.valueOf(set)) == Double.parseDouble(value));
		}
		catch (NumberFormatException e) {
			throw new IllegalArgumentException("A slider value must be a number, not " + value, e);
		}
	}

	@Override
	public boolean selectOne(SmartClientSelectOneCommand command) {
		String locator = command.locator();
		int index = command.index();

		Found found = require(locator);
		if (found.disabled()) {
			trace("    The combo is disabled");
			return false;
		}
		prepare(found.element());

		SelenideElement element = require(String.format("%s/[icon=\"picker\"]", locator)).element();
		prepare(element);
		click(element);

		// Wait for pick list drop down
		element = require(String.format("%s/pickList", locator)).element();
		element.shouldBe(visible);

		// Value here should be an index in the drop down starting from 0
		element = require(String.format("%s/pickList/body/row[%d]", locator, Integer.valueOf(index))).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	/**
	 * Answers yes to any confirmation question that is showing.
	 *
	 * @return {@code true} if a question showed and was answered
	 */
	@Override
	public boolean confirm() {
		return answer(Boolean.TRUE, "the last click");
	}

	@Override
	public boolean verifySuccess() {
		String problem = problem();
		if (problem == null) {
			lastProblem = null;
			return true;
		}

		lastProblem = problem;
		return false;
	}

	@Override
	public void assertSuccess() {
		if (! verifySuccess()) {
			throw new AssertionError("Not successful - " + lastProblem);
		}
	}

	@Override
	public boolean verifyFailure(String messageToCheck) {
		// Error messages only (the Problems dialog, field errors, error growls), once the page has settled, as PrimeFaces
		List<String> found = messages("error");
		if (found.isEmpty()) {
			lastProblem = "no error message or field error is showing (" + describeMessages() + ')';
		}
		else if (messageToCheck == null) {
			return true;
		}
		else {
			for (String message : found) {
				if (message.contains(messageToCheck)) {
					return true;
				}
			}
			lastProblem = String.format("expected a problem containing '%s' but found %s", messageToCheck, found);
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
			throw new AssertionError("Successful - " + lastProblem);
		}
	}

	@Override
	public void assertFailure() {
		assertFailure(null);
	}

	/**
	 * Describes the SmartClient dialog and the field errors of the showing view.
	 *
	 * @return the messages, or a statement that there are none
	 */
	@Override
	public @Nonnull String describeMessages() {
		try {
			if (! isSmartClientPage()) {
				return "(not a SmartClient page: " + driver.getCurrentUrl() + ')';
			}
			waitForFullPageResponse();
			List<String> messages = problemMessages(map(script("return _sail.problems()")), true);
			return messages.isEmpty() ? "no message, question or field error" : String.join("; ", messages);
		}
		catch (RuntimeException e) {
			return "(could not read the messages: " + firstLine(e.getMessage()) + ')';
		}
	}

	/**
	 * Describes what makes the current view unsuccessful: a warning or question dialog, or field errors.
	 * An informational message ({@code isc.say}) is not a problem.
	 *
	 * @return the problem, or {@code null} if there is none
	 */
	private @Nullable String problem() {
		waitForFullPageResponse();
		Map<String, Object> problems = map(script("return _sail.problems()"));
		List<String> messages = problemMessages(problems, false);
		return messages.isEmpty() ? null : String.join("; ", messages);
	}

	/**
	 * Lists the problems reported by the browser helper.
	 *
	 * @param problems		the result of {@code _sail.problems()}
	 * @param includeInfo	whether an informational message counts
	 * @return the messages
	 */
	@SuppressWarnings("unchecked")
	private static @Nonnull List<String> problemMessages(@Nonnull Map<String, Object> problems, boolean includeInfo) {
		List<String> result = new java.util.ArrayList<>();
		Object dialog = problems.get("dialog");
		if (dialog instanceof Map<?, ?> d) {
			String kind = String.valueOf(d.get("kind"));
			if (includeInfo || (! "say".equals(kind))) {
				String prefix = "ask".equals(kind) ? "unanswered question: " : (kind + " message: ");
				result.add(prefix + d.get("message"));
			}
		}
		Object errors = problems.get("errors");
		if (errors instanceof List<?> list) {
			for (Object error : (List<Object>) list) {
				result.add("field error " + error);
			}
		}
		return result;
	}

	/**
	 * Describes the dialog that is showing.
	 *
	 * @return its kind and message, or {@code null} if no dialog is showing
	 */
	public @Nullable String dialogMessage() {
		Object dialog = script("return window._sail ? _sail.dialog() : null");
		if (dialog instanceof Map<?, ?> d) {
			return d.get("kind") + " '" + d.get("message") + "'";
		}
		return null;
	}

	/**
	 * Returns the kind of dialog that is showing.
	 *
	 * @return {@code say}, {@code warn} or {@code ask}; or {@code null} if no dialog is showing
	 */
	public @Nullable String dialogKind() {
		Object dialog = script("return window._sail ? _sail.dialog() : null");
		if (dialog instanceof Map<?, ?> d) {
			return String.valueOf(d.get("kind"));
		}
		return null;
	}

	/**
	 * Reads the value a form item displays, for {@code testValue}.
	 *
	 * @param locator	the SAIL locator of the item
	 * @return the displayed value
	 * @throws IllegalStateException if the item cannot be found
	 */
	public @Nonnull String value(@Nonnull String locator) {
		return String.valueOf(values(locator).get(0));
	}

	/**
	 * Reads the value a form item displays, followed by other acceptable representations
	 * (the underlying value, or the HTML of rich text).
	 *
	 * @param locator	the SAIL locator of the item
	 * @return the displayed value first, then alternatives
	 * @throws IllegalStateException if the item cannot be found
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull List<String> values(@Nonnull String locator) {
		require(locator.replaceFirst("\\}/.*$", "}")); // wait for the item to exist
		Map<String, Object> result = map(script("return _sail.value(arguments[0])", locator));
		Object error = result.get("error");
		if (error != null) {
			throw new IllegalStateException("Could not read " + locator + ": " + error);
		}
		List<String> values = new java.util.ArrayList<>();
		values.add(String.valueOf(result.get("value")));
		Object alternatives = result.get("alternatives");
		if (alternatives instanceof List<?> list) {
			for (Object alternative : (List<Object>) list) {
				values.add(String.valueOf(alternative));
			}
		}
		return values;
	}

	/**
	 * Locates an element, waiting for it to appear, and failing with the browser's explanation if it does not.
	 *
	 * @param locator	the AutoTest or SAIL locator
	 * @return what was found
	 * @throws IllegalStateException if nothing is found in time, or the locator can never match
	 */
	public @Nonnull Found require(@Nonnull String locator) {
		waitForFullPageResponse();
		// Waits for SmartClient to draw the widget: after a response it lays out and draws from zero-delay timers
		return until("SmartClient to show " + locator, d -> {
			Found found = find(locator);
			if ((found == null) && Boolean.FALSE.equals(lastRetry)) {
				throw new IllegalStateException(String.format("Could not find %s: %s", locator, lastProblem));
			}
			return found;
		});
	}

	// whether the last find() may succeed if tried again
	private Boolean lastRetry;

	/**
	 * Looks for an element once, without waiting.
	 *
	 * @param locator	the AutoTest or SAIL locator
	 * @return what was found, or {@code null}
	 */
	public @Nullable Found findNow(@Nonnull String locator) {
		waitForFullPageResponse();
		return find(locator);
	}

	/**
	 * Asks the browser helper for an element once.
	 *
	 * @param locator	the AutoTest or SAIL locator
	 * @return what was found, or {@code null}
	 */
	private @Nullable Found find(@Nonnull String locator) {
		lastRetry = Boolean.TRUE;
		Map<String, Object> result = map(script("return _sail.find(arguments[0])", locator));
		Object element = result.get("element");
		if (element instanceof WebElement webElement) {
			lastProblem = null;
			return new Found($(webElement),
								Boolean.TRUE.equals(result.get("shown")),
								Boolean.TRUE.equals(result.get("disabled")),
								String.valueOf(result.get("locator")));
		}
		lastProblem = String.valueOf(result.get("error"));
		lastRetry = Boolean.valueOf(Boolean.TRUE.equals(result.get("retry")));
		return null;
	}

	/**
	 * Fails if SmartClient says the widget is not shown (hidden by the view, or under a tab that is not selected).
	 *
	 * @param found	what was found
	 * @param what	what the widget is, for the message
	 * @throws IllegalStateException if the widget is not shown
	 */
	private static void requireShown(@Nonnull Found found, @Nonnull String what) {
		if (! found.shown()) {
			throw new IllegalStateException(String.format("The %s %s is on the view but not shown (hidden here, or on a tab that is not selected)",
															what, found.locator()));
		}
	}

	/**
	 * Counts the windows open in the window stack (zoomed-in views, pick lists, upload pages).
	 *
	 * @return the number of windows
	 */
	public int openWindows() {
		waitForFullPageResponse();
		Object count = script("return (isc.WindowStack._stack || []).length");
		return (count instanceof Number number) ? number.intValue() : 0;
	}

	/**
	 * Describes what the browser shows, for error messages.
	 *
	 * @return the helper's state
	 */
	public @Nonnull String state() {
		try {
			return String.valueOf(script("return window._sail ? JSON.stringify(_sail.state()) : 'no SAIL helper on ' + location.href"));
		}
		catch (WebDriverException e) {
			return "unknown (" + e.getMessage() + ')';
		}
	}

	/**
	 * Waits until the view showing in the browser is the expected one.
	 *
	 * @param moduleName	the module
	 * @param documentName	the document
	 * @throws IllegalStateException if the view does not show in time
	 */
	public void waitForView(@Nonnull String moduleName, @Nonnull String documentName) {
		String expected = moduleName + '.' + documentName;
		waitForFullPageResponse();
		// Waits for the view the step opens: the harness creates it after the page loads (its smartgen request,
		// then the bean's fetch), or the window stack shows it after a zoom
		until("the view " + expected + " to show", d -> {
			waitForFullPageResponse();
			return expected.equals(script("return _sail.describe(_sail.topView())")) ? Boolean.TRUE : null;
		});
	}

	/**
	 * Reports whether the browser shows the view (its top-most edit view), once the page has settled.
	 *
	 * @param moduleName	the module
	 * @param documentName	the document
	 * @return {@code true} if it shows
	 */
	public boolean showsView(@Nonnull String moduleName, @Nonnull String documentName) {
		waitForFullPageResponse();
		return (moduleName + '.' + documentName).equals(script("return _sail.describe(_sail.topView())"));
	}

	/**
	 * Waits until the browser shows a list view.
	 *
	 * @throws IllegalStateException if the list does not show in time
	 */
	public void waitForList() {
		waitForFullPageResponse();
		// Waits for the list view the step opens, which the harness sets up after the page loads
		until("a list view to show", d -> {
			waitForFullPageResponse();
			return "list".equals(script("return _sail.describe(_sail.topView())")) ? Boolean.TRUE : null;
		});
	}

	/**
	 * Performs a lookup description new interaction.
	 *
	 * @param locator the locator string for the lookup description widget
	 * @return true if successful, false otherwise
	 */
	public boolean lookupDescriptionNew(@Nonnull String locator) {
		return lookupDescriptionMenu(locator, "New");
	}

	/**
	 * Performs a lookup description edit interaction.
	 *
	 * @param locator the locator string for the lookup description widget
	 * @return true if successful, false otherwise
	 */
	public boolean lookupDescriptionEdit(@Nonnull String locator) {
		return lookupDescriptionMenu(locator, "Edit");
	}

	/**
	 * Chooses an entry from a lookup description's split button menu.
	 *
	 * @param locator	the lookup description locator
	 * @param title		the menu entry title
	 * @return {@code true} once chosen, {@code false} if the lookup is disabled
	 */
	private boolean lookupDescriptionMenu(@Nonnull String locator, @Nonnull String title) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The lookup description is disabled");
			return false;
		}
		prepare(found.element());

		// Select the drop-down
		SelenideElement element = require(String.format(
				"%s/item[Class=CanvasItem||name=_splitButton]/canvas/member[Class=MenuButton||index=1]",
				locator)).element();
		prepare(element);
		click(element);

		// Select the menu entry
		element = require(String.format("//Menu[level=0]/body/row[title=%s]/col[fieldName=title]", title)).element();
		prepare(element);
		click(element);
		waitForFullPageResponse();

		return true;
	}

	/**
	 * Clicks the OK button of the SmartClient dialog if one is showing.
	 *
	 * @return {@code true} if the button was found and clicked, {@code false} otherwise
	 */
	public boolean ok() {
		if (! isSmartClientPage()) {
			return false;
		}
		waitForFullPageResponse();
		if (dialogKind() == null) {
			return false;
		}
		Found found = findNow(DIALOG_OK);
		if (found != null) {
			trace("    Dismiss " + dialogMessage());
			click(found.element());
			waitForFullPageResponse();
			return true;
		}

		return false;
	}

	/**
	 * Waits for the page to load, for SmartClient to start and for it to settle: no request pending, no deferred
	 * call of a view, grid, button or dialog waiting to fire (a dialog's callback, a blur's rerender, a row
	 * selection's server call), and every shown widget done processing (see {@code _sail.settled()}).
	 *
	 * @throws IllegalStateException if SmartClient does not load, or does not settle
	 */
	public void waitForFullPageResponse() {
		// Waits for the asynchronous work the last step started: a page load, a request, the deferred calls above
		until("SmartClient to load and settle (is the session using the SmartClient (desktop) UX/UI?)", d -> {
			Object state;
			try {
				state = script("if (document.readyState !== 'complete') return 'loading';" +
								"if (!window._sail) return 'inject';" +
								"return _sail.settled() ? 'settled' : 'busy';");
			}
			catch (org.openqa.selenium.JavascriptException e) {
				// the page is being replaced
				lastProblem = firstLine(e.getMessage());
				return null;
			}
			if ("inject".equals(state)) {
				script(HELPER);
				return null;
			}
			lastProblem = "settled".equals(state) ? null : String.valueOf(state);
			return "settled".equals(state) ? Boolean.TRUE : null;
		});
	}

	/**
	 * Waits for SmartClient to settle after something changed the page outside this class, such as a script.
	 */
	public void settle() {
		waitForFullPageResponse();
	}

	/**
	 * Ends inline editing in any data grid of the showing view.
	 *
	 * @return {@code true} if a grid was editing
	 */
	public boolean endInlineEditing() {
		waitForFullPageResponse();
		if (! Boolean.TRUE.equals(script("return _sail.inlineEditing()"))) {
			return false;
		}
		// Enter commits the row, as it does for a user (ListGrid.endEditing() can drop a new row)
		driver.switchTo().activeElement().sendKeys(Keys.ENTER);
		waitForFullPageResponse();
		if (Boolean.TRUE.equals(script("return _sail.inlineEditing()"))) {
			throw new IllegalStateException("Enter did not end the inline edit of the data grid row");
		}
		return true;
	}

	/**
	 * Clicks the specified element once.
	 *
	 * @param element the element to click
	 */
	public void click(@Nonnull SelenideElement element) {
		click(element, false);
	}

	/**
	 * Performs a double-click on the specified element.
	 *
	 * @param element the element to double-click
	 */
	public void doubleClick(@Nonnull SelenideElement element) {
		click(element, true);
	}

	/**
	 * Clicks or double-clicks the given element after scrolling it into view the SmartClient way,
	 * retrying once after scrolling it again if something else received the click.
	 *
	 * @param element the element to click
	 * @param doubleClick {@code true} to perform a double-click, {@code false} for a single click
	 */
	private void click(@Nonnull SelenideElement element, boolean doubleClick) {
		WebElement target = element.toWebElement();
		dismissDialogInTheWay(target);
		if ((! doubleClick) && Boolean.TRUE.equals(script("return !!window._sail && _sail.clickWouldPair(arguments[0])", target))) {
			// Waits for SmartClient's double-click interval to pass: it would read this click as the second click of a
			// double click on the same widget (a button then ignores it), and SAIL means a separate click
			until("SmartClient's double-click interval to pass", d -> Boolean.valueOf(! Boolean.TRUE.equals(script("return _sail.clickWouldPair(arguments[0])", target))));
		}
		try {
			reveal(element);
			clickIt(element, doubleClick);
		}
		catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
			throw new IllegalStateException("The element went stale before it could be clicked: is the view re-rendering?");
		}
		catch (WebDriverException | com.codeborne.selenide.ex.UIAssertionError e) {
			// Another element took the click: SmartClient's hover (move the pointer off the widget that shows it, as a
			// user would, then click), one of the same SmartClient widget's nested elements (click that), or a
			// floating element (scroll the target to the centre and try again)
			Object same = script("return _sail.sameCanvasAt(arguments[0])", target);
			if (Boolean.TRUE.equals(script("return _sail.hoverCovers(arguments[0])", target))) {
				trace("    SmartClient's hover covers the element - move the pointer onto it, which hides the hover");
				new org.openqa.selenium.interactions.Actions(driver).moveToElement(target).perform();
				// Waits for SmartClient to hide its hover: the pointer left the widget that showed it
				until("SmartClient's hover to hide", d -> Boolean.valueOf(! Boolean.TRUE.equals(script("return _sail.hoverCovers(arguments[0])", target))));
				clickIt(element, doubleClick);
			}
			else if (same instanceof WebElement top) {
				clickIt($(top), doubleClick);
			}
			else {
				trace("    Could not click on the element (" + firstLine(e.getMessage()) + ") - scroll it into view and try again");
				((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView({block: 'center'});", element);
				clickIt(element, doubleClick);
			}
		}
	}

	/**
	 * Dismisses a message dialog that an earlier step left showing, when it is in the way of a click elsewhere, as a user
	 * would before going on. Messages stay up until then, for testMessage, testSuccess and testFailure to read.
	 *
	 * @param target	the element to be clicked
	 * @throws IllegalStateException if the dialog is an unanswered question: answering it is the script's job
	 */
	private void dismissDialogInTheWay(@Nonnull WebElement target) {
		if (! Boolean.TRUE.equals(script("return !!window._sail"))) {
			return;
		}
		if (script("return _sail.dialogInTheWay(arguments[0])", target) instanceof Map<?, ?> dialog) {
			if ("ask".equals(dialog.get("kind"))) {
				throw new IllegalStateException("An unanswered question is showing: " + dialog.get("message"));
			}
			trace("    Dismiss the " + dialog.get("kind") + " message '" + dialog.get("message") + "' left by an earlier step");
			ok();
		}
	}

	/**
	 * Returns the first line of a message.
	 *
	 * @param message	the message
	 * @return the first line
	 */
	private static @Nonnull String firstLine(@Nullable String message) {
		if (message == null) {
			return "";
		}
		int newLine = message.indexOf('\n');
		return (newLine < 0) ? message : message.substring(0, newLine);
	}

	/**
	 * Performs the actual single or double click on the given element.
	 *
	 * @param element the element to click
	 * @param doubleClick {@code true} to perform a double-click, {@code false} for a single click
	 */
	private static void clickIt(@Nonnull SelenideElement element, boolean doubleClick) {
		if (doubleClick) {
			element.doubleClick();
		} else {
			element.click();
		}
	}

	/**
	 * Scrolls an element into view through SmartClient ({@code Canvas.scrollIntoView()} on the
	 * nearest canvas and its ancestors), so that widgets below the fold of a scrolling SmartClient
	 * container can be reached.
	 *
	 * @param element	the element
	 */
	private void reveal(@Nonnull SelenideElement element) {
		script("if (window._sail) _sail.reveal(arguments[0]);", element.toWebElement());
	}

	/**
	 * Ensures the given element is scrolled into view and visible.
	 *
	 * @param element the {@link SelenideElement} to prepare
	 */
	private void prepare(@Nonnull SelenideElement element) {
		element.should(exist);
		reveal(element);
		element.shouldBe(visible);
	}

	/**
	 * Runs JavaScript in the page.
	 *
	 * @param script	the script
	 * @param arguments	its arguments
	 * @return the result
	 */
	private @Nullable Object script(@Nonnull String script, Object... arguments) {
		return ((JavascriptExecutor) driver).executeScript(script, arguments);
	}

	/**
	 * Casts a JavaScript object result to a map.
	 *
	 * @param result	the script result
	 * @return the map, empty if the result was not an object
	 */
	@SuppressWarnings("unchecked")
	private static @Nonnull Map<String, Object> map(@Nullable Object result) {
		return (result instanceof Map<?, ?> map) ? (Map<String, Object>) map : Map.of();
	}
}

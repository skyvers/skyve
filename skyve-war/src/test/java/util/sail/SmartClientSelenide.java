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

	private static final long POLL_MILLIS = 100;
	// Time for a page to finish loading and for SmartClient to start
	private static final long LOAD_TIMEOUT_MILLIS = 30_000;
	// Time for SmartClient to finish its requests and become idle
	private static final long IDLE_TIMEOUT_MILLIS = 30_000;
	// Time for an element to appear
	private static final long LOCATE_TIMEOUT_MILLIS = 10_000;
	// Time for a dialog to appear after a click
	private static final long DIALOG_TIMEOUT_MILLIS = 5_000;

	private static final String DIALOG_OK = Locator.sail(null, Kind.dialog, "ok", null, null, null, null);
	private static final String DIALOG_YES = Locator.sail(null, Kind.dialog, "yes", null, null, null, null);

	// How long SmartClient must stay idle after a user action before the page counts as settled.
	// SmartClient starts some requests from timers (a dialog's callback, a change handler), so an idle
	// check straight after a click can pass before the request has started.
	private static final long QUIET_MILLIS = 150;

	// The reason the last locate() or verify call failed, for messages
	private String lastProblem;

	// Set by clicks; the next wait then requires SmartClient to stay idle for QUIET_MILLIS
	private boolean acted = false;

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
	 * Signs in, waits for the outcome, and on success selects the SmartClient UX/UI.
	 * <p>
	 * Desktop browsers are routed to PrimeFaces by default, so this clicks the PrimeFaces
	 * "Switch mode" menu item, as a user would, which sets the session's UX/UI to {@code desktop}.
	 * A failed sign-in leaves the browser on the sign-in page and is traced, not thrown,
	 * because a script may sign in with bad credentials on purpose.
	 */
	@Override
	public void login(@Nullable String customer, @Nonnull String username, @Nonnull String password) {
		super.login(customer, username, password);
		if (getLoginError() == null) {
			selectSmartClient();
		}
		else {
			trace(String.format("    Sign in as %s failed: %s", username, getLoginError()));
		}
	}

	/**
	 * Makes the signed-in session use the SmartClient UX/UI.
	 *
	 * @throws IllegalStateException if the page has no "Switch mode" link or SmartClient does not load
	 */
	private void selectSmartClient() {
		if (isSmartClientPage()) {
			return;
		}
		Object switched = script("""
				const a = Array.from(document.querySelectorAll('a')).find((x) => /switch mode/i.test(x.textContent || '') || x.querySelector('.fa-retweet'));
				if (a) { a.click(); return true; }
				return false;""");
		if (! Boolean.TRUE.equals(switched)) {
			throw new IllegalStateException(String.format(
					"Signed in, but %s (%s) is not SmartClient and has no \"Switch mode\" link to select the SmartClient UX/UI. " +
					"Can this user switch modes?", driver.getCurrentUrl(), driver.getTitle()));
		}
		trace("    Switched the session to the SmartClient UX/UI");

		long deadline = System.currentTimeMillis() + LOAD_TIMEOUT_MILLIS;
		while (System.currentTimeMillis() < deadline) {
			sleep(POLL_MILLIS, "Interrupted waiting for SmartClient");
			if (isSmartClientPage()) {
				waitForFullPageResponse();
				return;
			}
		}
		throw new IllegalStateException(String.format("Switched mode, but SmartClient did not load on %s (%s)",
														driver.getCurrentUrl(), driver.getTitle()));
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

		if (command.confirm()) {
			if (! confirm()) {
				throw new IllegalStateException("Expected a confirmation question after clicking " + command.locator() + " but none showed");
			}
		}

		waitForFullPageResponse();

		return true;
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
	 * @param confirm	whether to answer yes to the confirmation that follows
	 * @return {@code true} if clicked, {@code false} if the button is disabled
	 */
	private boolean toolButton(@Nonnull String locator, boolean confirm) {
		Found found = require(locator);
		if (found.disabled()) {
			trace("    The grid button is disabled");
			return false;
		}
		requireShown(found, "grid button");
		SelenideElement element = found.element();
		prepare(element);
		click(element);

		if (confirm && (! confirm())) {
			throw new IllegalStateException("Expected a confirmation question after clicking " + locator + " but none showed");
		}

		waitForFullPageResponse();

		return true;
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
		SelenideElement element = require(pickList).element();
		element.should(exist, Duration.ofSeconds(30)).shouldBe(visible);

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

		// Type as a user would: select what is there, delete it, then type the value key by key,
		// so that SmartClient sees every change (setting the DOM value directly can leave the item's value stale)
		script("arguments[0].focus(); if (arguments[0].select) arguments[0].select();", element.toWebElement());
		element.sendKeys(Keys.BACK_SPACE);
		if ((value != null) && (! value.isEmpty())) {
			element.sendKeys(value);
		}
		if (! command.keyPresses()) {
			// leave the field, so that SmartClient commits the value and runs its change handlers
			acted();
			element.sendKeys(Keys.TAB);
			waitForFullPageResponse();
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
		acted();
		element.sendKeys(Keys.DELETE);
		if (value != null) {
			element.sendKeys(value);
		}
		// leave the editor so SmartClient saves its value
		element.sendKeys(Keys.TAB);
		waitForFullPageResponse();
		return true;
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
		acted();
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
	 * Answers yes to the confirmation question, waiting briefly for it to show.
	 *
	 * @return {@code true} if a question showed and was answered
	 */
	@Override
	public boolean confirm() {
		Found found = locate(DIALOG_YES, DIALOG_TIMEOUT_MILLIS);
		if (found != null) {
			trace("    Answer yes to: " + dialogMessage());
			click(found.element());
			waitForFullPageResponse();
			return true;
		}

		return false;
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
		Map<String, Object> problems = map(script("return _sail.problems()"));
		List<String> found = problemMessages(problems, true);
		if (found.isEmpty()) {
			lastProblem = "no error message, question or field error is showing";
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
	 * Returns the reason the last verification failed.
	 *
	 * @return the reason, or {@code null}
	 */
	public @Nullable String getLastProblem() {
		return lastProblem;
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
	 * Locates the element specified by the given locator, retrying until it appears.
	 *
	 * @param locator the SmartClient AutoTest or SAIL locator string
	 * @return the located SelenideElement; or null if the element cannot be found
	 */
	public @Nullable SelenideElement locate(@Nonnull String locator) {
		Found found = locate(locator, LOCATE_TIMEOUT_MILLIS);
		return (found == null) ? null : found.element();
	}

	/**
	 * Locates an element, failing with the browser's explanation if it does not appear.
	 *
	 * @param locator	the AutoTest or SAIL locator
	 * @return what was found
	 * @throws IllegalStateException if nothing is found in time
	 */
	public @Nonnull Found require(@Nonnull String locator) {
		Found result = locate(locator, LOCATE_TIMEOUT_MILLIS);
		if (result == null) {
			throw new IllegalStateException(String.format("Could not find %s: %s (browser state %s)",
															locator, lastProblem, state()));
		}
		return result;
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
	 * Locates an element, retrying until it appears or the timeout passes.
	 *
	 * @param locator		the AutoTest or SAIL locator
	 * @param timeoutMillis	how long to keep trying
	 * @return what was found, or {@code null}; {@link #getLastProblem()} then says why
	 */
	public @Nullable Found locate(@Nonnull String locator, long timeoutMillis) {
		trace("    Locating " + locator);
		long deadline = System.currentTimeMillis() + timeoutMillis;
		do {
			waitForFullPageResponse();
			try {
				Map<String, Object> result = map(script("return _sail.find(arguments[0])", locator));
				Object element = result.get("element");
				if (element instanceof WebElement webElement) {
					SelenideElement selenideElement = $(webElement);
					selenideElement.should(exist);
					return new Found(selenideElement,
										Boolean.TRUE.equals(result.get("shown")),
										Boolean.TRUE.equals(result.get("disabled")),
										String.valueOf(result.get("locator")));
				}
				lastProblem = String.valueOf(result.get("error"));
				if (! Boolean.TRUE.equals(result.get("retry"))) {
					return null;
				}
			}
			catch (@SuppressWarnings("unused") StaleElementReferenceException | AssertionError e) {
				lastProblem = "the element went stale";
			}
			catch (WebDriverException e) {
				lastProblem = e.getMessage();
			}
			sleep(POLL_MILLIS, "Interrupted while waiting for element: " + locator);
		}
		while (System.currentTimeMillis() < deadline);

		return null;
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
		long deadline = System.currentTimeMillis() + LOAD_TIMEOUT_MILLIS;
		do {
			waitForFullPageResponse();
			Object showing = script("return _sail.describe(_sail.topView())");
			if (expected.equals(showing)) {
				return;
			}
			sleep(POLL_MILLIS, "Interrupted waiting for view " + expected);
		}
		while (System.currentTimeMillis() < deadline);
		throw new IllegalStateException(String.format("Expected the view %s to show, but the browser shows %s",
														expected, state()));
	}

	/**
	 * Waits until the browser shows a list view.
	 *
	 * @throws IllegalStateException if the list does not show in time
	 */
	public void waitForList() {
		long deadline = System.currentTimeMillis() + LOAD_TIMEOUT_MILLIS;
		do {
			waitForFullPageResponse();
			if ("list".equals(script("return _sail.describe(_sail.topView())"))) {
				return;
			}
			sleep(POLL_MILLIS, "Interrupted waiting for a list");
		}
		while (System.currentTimeMillis() < deadline);
		throw new IllegalStateException("Expected a list view to show, but the browser shows " + state());
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
		Found found = locate(DIALOG_OK, POLL_MILLIS);
		if (found != null) {
			trace("    Dismiss " + dialogMessage());
			click(found.element());
			waitForFullPageResponse();
			return true;
		}

		return false;
	}

	/**
	 * Waits for the full page to load, for SmartClient to start and for it to become idle.
	 *
	 * @throws IllegalStateException if SmartClient does not load, or stays busy
	 */
	public void waitForFullPageResponse() {
		long deadline = System.currentTimeMillis() + LOAD_TIMEOUT_MILLIS;
		String problem = null;
		while (true) {
			try {
				Object loaded = script("if (document.readyState !== 'complete') return 'loading';" +
										"if (!window._sail) return 'inject';" +
										"return _sail.loaded() ? 'loaded' : 'starting';");
				if ("inject".equals(loaded)) {
					script(HELPER);
					continue;
				}
				if ("loaded".equals(loaded)) {
					break;
				}
				problem = String.valueOf(loaded);
			}
			catch (WebDriverException e) {
				// navigation in progress
				problem = e.getMessage();
			}
			if (System.currentTimeMillis() > deadline) {
				throw new IllegalStateException(String.format("SmartClient did not load on %s (%s) after %d seconds: %s. " +
																"Is the session using the SmartClient (desktop) UX/UI?",
																driver.getCurrentUrl(),
																driver.getTitle(),
																Long.valueOf(LOAD_TIMEOUT_MILLIS / 1000),
																problem));
			}
			sleep(POLL_MILLIS, "Interrupted while waiting for the page");
		}

		deadline = System.currentTimeMillis() + IDLE_TIMEOUT_MILLIS;
		long quiet = acted ? QUIET_MILLIS : 0;
		long idleSince = -1;
		while (true) {
			long now = System.currentTimeMillis();
			if (Boolean.TRUE.equals(script("return _sail.idle()"))) {
				if (idleSince < 0) {
					idleSince = now;
				}
				if (now - idleSince >= quiet) {
					break;
				}
			}
			else {
				idleSince = -1;
			}
			if (now > deadline) {
				throw new IllegalStateException(String.format("SmartClient was still busy after %d seconds: %s",
																Long.valueOf(IDLE_TIMEOUT_MILLIS / 1000),
																state()));
			}
			sleep(POLL_MILLIS / 2, "Interrupted while waiting for SmartClient");
		}
		acted = false;
	}

	/**
	 * Waits for SmartClient to settle after something changed the page outside this class, such as a script.
	 */
	public void settle() {
		acted();
		waitForFullPageResponse();
	}

	/**
	 * Records that the user acted (clicked or typed), so that the next wait lets SmartClient settle.
	 */
	private void acted() {
		acted = true;
	}

	/**
	 * Ends inline editing in any data grid of the showing view.
	 *
	 * @return {@code true} if a grid was editing
	 */
	public boolean endInlineEditing() {
		waitForFullPageResponse();
		boolean result = Boolean.TRUE.equals(script("return _sail.endInlineEditing()"));
		waitForFullPageResponse();
		return result;
	}

	/**
	 * Helper method to sleep and handle InterruptedException.
	 *
	 * @param millis the number of milliseconds to sleep
	 * @param errorMessage the error message to include if the sleep is interrupted
	 */
	@SuppressWarnings({"static-method", "java:S2925"})
	public void sleep(long millis, String errorMessage) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(errorMessage, e);
		}
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
		acted();
		try {
			reveal(element);
			clickIt(element, doubleClick);
		}
		catch (@SuppressWarnings("unused") StaleElementReferenceException e) {
			throw new IllegalStateException("The element went stale before it could be clicked: is the view re-rendering?");
		}
		catch (WebDriverException e) {
			// This could occur when the control is behind a floating element
			trace("    Could not click on the element (" + firstLine(e.getMessage()) + ") - scroll it into view and try again");
			((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView({block: 'center'});", element);
			clickIt(element, doubleClick);
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

package util.sail;

import static com.codeborne.selenide.Selectors.byName;
import static com.codeborne.selenide.Selenide.$;
import static com.codeborne.selenide.Selenide.open;

import java.time.Duration;
import java.util.List;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.skyve.domain.messages.DomainException;

import com.codeborne.selenide.SelenideElement;
import com.codeborne.selenide.WebDriverRunner;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import util.sail.commands.ButtonCommand;
import util.sail.commands.CheckboxCommand;
import util.sail.commands.DataGridButtonCommand;
import util.sail.commands.DataGridSelectCommand;
import util.sail.commands.ListGridButtonCommand;
import util.sail.commands.ListGridSelectCommand;
import util.sail.commands.LookupDescriptionByRowCommand;
import util.sail.commands.LookupDescriptionBySearchCommand;
import util.sail.commands.RadioCommand;
import util.sail.commands.SelectOneCommand;
import util.sail.commands.TabCommand;
import util.sail.commands.TextCommand;

/**
 * Abstract base class for UI testing using Selenide, parameterised by
 * a family of command types representing different UI interaction categories.
 * <p>
 * Subclasses bind concrete command implementations corresponding to
 * specific UI frameworks, enabling type-safe and reusable automation logic.
 *
 * @param <BTNC> ButtonCommand type
 * @param <CHKC> CheckboxCommand type
 * @param <DGBC> DataGridButtonCommand type
 * @param <DGSC> DataGridSelectCommand type
 * @param <LGBC> ListGridButtonCommand type
 * @param <LGSC> ListGridSelectCommand type
 * @param <LDRC> LookupDescriptionByRowCommand type
 * @param <LDSC> LookupDescriptionBySearchCommand type
 * @param <RADC> RadioCommand type
 * @param <TABC> TabCommand type
 * @param <TXTC> TextCommand type
 * @param <SELC> SelectOneCommand type
 * @author simeonsolomou
 */
public abstract class Selenide<
		BTNC extends ButtonCommand,
		CHKC extends CheckboxCommand,
		DGBC extends DataGridButtonCommand,
		DGSC extends DataGridSelectCommand,
		LGBC extends ListGridButtonCommand,
		LGSC extends ListGridSelectCommand,
		LDRC extends LookupDescriptionByRowCommand,
		LDSC extends LookupDescriptionBySearchCommand,
		RADC extends RadioCommand,
		TABC extends TabCommand,
		TXTC extends TextCommand,
		SELC extends SelectOneCommand> extends CrossBrowserSelenium {

	/** System property naming the timeout in seconds for every wait on the browser (default 30). */
	public static final String TIMEOUT_PROPERTY = "sail.timeoutSeconds";
	/**
	 * The timeout for every wait on the browser, for an asynchronous event (a page or ajax response, an element
	 * appearing). It is a limit, not a delay: waits end as soon as their event happens.
	 */
	public static final Duration TIMEOUT = Duration.ofSeconds(Long.getLong(TIMEOUT_PROPERTY, 30L).longValue());

	private String baseUrl;
	private String loginError;

	@Override
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		super.startBrowser(configuration);

		WebDriverRunner.setWebDriver(driver);
		// Selenide's should...() conditions share the one timeout
		com.codeborne.selenide.Configuration.timeout = TIMEOUT.toMillis();
		this.baseUrl = configuration.getBaseUrl();
	}

	/**
	 * Drives the browser that another instance started (see {@link CrossBrowserSelenium#useBrowserOf}), and makes it
	 * Selenide's browser.
	 *
	 * @param other	the instance whose browser to drive
	 */
	@Override
	public void useBrowserOf(@Nonnull CrossBrowserSelenium other) {
		super.useBrowserOf(other);
		WebDriverRunner.setWebDriver(driver);
		com.codeborne.selenide.Configuration.timeout = TIMEOUT.toMillis();
		BrowserConfiguration shared = getConfiguration();
		if (shared != null) {
			this.baseUrl = shared.getBaseUrl();
		}
	}

	/**
	 * Waits for an asynchronous event in the browser: the one wait mechanism of the SAIL harness.
	 *
	 * @param <T> the condition's result type
	 * @param condition	true (or non-null) when the event has happened
	 * @return the condition's result
	 * @throws org.openqa.selenium.TimeoutException	if the event does not happen within {@link #TIMEOUT}
	 */
	public <T> T waitFor(@Nonnull java.util.function.Function<org.openqa.selenium.WebDriver, T> condition) {
		return waitFor(TIMEOUT, Duration.ofMillis(100), condition);
	}

	/**
	 * Waits for an asynchronous event in the browser with a timeout and polling interval of the script author's choosing
	 * (e.g. a {@code timeoutMillis} that polls a server job).
	 *
	 * @param <T> the condition's result type
	 * @param timeout	the timeout
	 * @param polling	how often to evaluate the condition
	 * @param condition	true (or non-null) when the event has happened
	 * @return the condition's result
	 * @throws org.openqa.selenium.TimeoutException	if the event does not happen in time
	 */
	public <T> T waitFor(@Nonnull Duration timeout,
							@Nonnull Duration polling,
							@Nonnull java.util.function.Function<org.openqa.selenium.WebDriver, T> condition) {
		return new WebDriverWait(driver, timeout, polling)
					.ignoring(org.openqa.selenium.StaleElementReferenceException.class)
					.until(condition);
	}

	/**
	 * Opens the specified relative URL on the base site.
	 * 
	 * @param url relative URL path
	 */
	public void get(@Nonnull String url) {
		open(baseUrl + url);
	}

	/**
	 * Opens the specified absolute URL.
	 * 
	 * @param url absolute URL
	 */
	@SuppressWarnings("static-method")
	public void openUrl(@Nonnull String url) {
		open(url);
	}

	/**
	 * Performs login using username and password.
	 * 
	 * @param username login username
	 * @param password login password
	 */
	public void login(String username, String password) {
		login(null, username, password);
	}

	/**
	 * Performs login using optional customer, username, and password, and waits for the outcome:
	 * either the page after sign in, or the sign in page showing an error.
	 * {@link #getLoginError()} then says which.
	 * 
	 * @param customer optional customer identifier
	 * @param username login username
	 * @param password login password
	 */
	public void login(@Nullable String customer, @Nonnull String username, @Nonnull String password) {
		loginError = null;
		open(baseUrl);
		if (! $(byName("user")).exists()) {
			// Still signed in (e.g. an earlier script stopped before its logout): sign out first
			logout();
			open(baseUrl);
		}
		// The page's own scripts can move the focus while typing (seen with headed browsers): check what landed and retry
		boolean typed = false;
		for (int attempt = 1; (attempt <= 3) && (! typed); attempt++) {
			if (customer != null) {
				$(byName("customer")).val(customer);
			}
			$(byName("user")).val(username);
			$(byName("password")).val(password);
			typed = username.equals($(byName("user")).getValue()) &&
						password.equals($(byName("password")).getValue()) &&
						((customer == null) || customer.equals($(byName("customer")).getValue()));
			if (! typed) {
				trace("    the sign in fields did not take the typed values - type them again");
			}
		}
		if (! typed) {
			// Submitting garbled credentials could make a sign in that should succeed fail (or a testFailure pass)
			throw new IllegalStateException("The sign in fields did not take the typed values after 3 attempts");
		}

		SelenideElement submit = $("input[type=\"submit\"]");
		WebElement submitElement = submit.getWrappedElement();
		submit.click();

		// Wait for the sign in page to be replaced by the next page (home, or the sign in page with its error), and for that page to load
		waitFor(d -> {
			try {
				submitElement.isEnabled();
				return null;
			}
			catch (@SuppressWarnings("unused") org.openqa.selenium.WebDriverException e) {
				return Boolean.TRUE; // stale, or no longer in the document: the page has been replaced
			}
		});
		waitFor(d -> Boolean.valueOf("complete".equals(((JavascriptExecutor) d).executeScript("return document.readyState"))));

		// A failed sign in comes back to the sign in page with an error message
		if ($("input[type=\"submit\"]").exists() && $(byName("password")).exists()) {
			SelenideElement error = $(".ui.error.message");
			loginError = error.exists() ? error.text().replaceAll("\\s+", " ").trim() : "still on the sign in page";
		}
	}

	/**
	 * Returns the error that the last {@link #login(String, String, String)} showed.
	 * 
	 * @return the error text, or null if the last sign in succeeded
	 */
	public @Nullable String getLoginError() {
		return loginError;
	}

	/**
	 * What a page says about the browser window's OS focus, for a failure's diagnosis.
	 *
	 * @param page		a token for the page (each page load gets a new one)
	 * @param blurs		how often the window lost the OS focus since the counter was installed in this page
	 * @param installed	whether the counter was installed by this probe, so that losses before it are unknown
	 * @param hasFocus	whether the page had the OS focus ({@code document.hasFocus()}) when probed
	 */
	public record FocusProbe(@Nonnull String page, long blurs, boolean installed, boolean hasFocus) {
	}

	/**
	 * Probes the page's count of window blur events (the browser window losing the OS focus), installing the counter
	 * ({@code window.__sailBlurs}) if the page does not have it yet. A diagnostic only: it never throws.
	 *
	 * @return the probe, or {@code null} if there is no page to ask or the script fails
	 */
	public @Nullable FocusProbe focusProbe() {
		try {
			Object result = ((JavascriptExecutor) driver).executeScript(
					"var c = window.__sailBlurs, installed = false;" +
					"if (! c) {" +
					"  c = window.__sailBlurs = {page: String(Date.now()) + '-' + Math.random(), n: 0};" +
					"  window.addEventListener('blur', function() {window.__sailBlurs.n++;});" +
					"  installed = true;" +
					"}" +
					"return [c.page, c.n, installed, document.hasFocus()];");
			if ((result instanceof List<?> list) && (list.size() == 4) && (list.get(1) instanceof Number blurs)) {
				return new FocusProbe(String.valueOf(list.get(0)), blurs.longValue(),
										Boolean.TRUE.equals(list.get(2)), Boolean.TRUE.equals(list.get(3)));
			}
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			// no page, a page being replaced, or no driver: say nothing
		}
		return null;
	}

	/**
	 * Asks the server for the signed-in user's effective roles, in the browser's own session: {@code module.Role}
	 * names, with the roles the user's groups give. The server answers only when {@code sail.recording} is on
	 * (see docs/sail.md, The test user).
	 *
	 * @return the role names, or {@code null} if the server does not answer (recording off, an older server, or no
	 * 			page to ask from)
	 */
	@SuppressWarnings("unchecked")
	public @Nullable List<String> signedInRoleNames() {
		Object result;
		try {
			result = ((org.openqa.selenium.JavascriptExecutor) driver).executeAsyncScript(
					"var done = arguments[arguments.length - 1];" +
					"fetch(arguments[0], {credentials: 'same-origin'})" +
					".then(function (r) { return r.ok ? r.json() : null; })" +
					".then(function (j) { done(j && Array.isArray(j.roles) ? j.roles : null); }, function () { done(null); });",
					baseUrl + "sailrecording?roles");
		}
		catch (org.openqa.selenium.WebDriverException e) {
			trace("    could not ask the server for the user's roles: " + e.getClass().getSimpleName());
			return null;
		}
		return (result instanceof List<?> list) ? (List<String>) list : null;
	}

	/**
	 * Performs logout by navigating to the logged out URL.
	 */
	public void logout() {
		open(baseUrl + "loggedOut");
	}

	/**
	 * Performs a button interaction based on the given command.
	 * 
	 * @param command parameters defining the button interaction
	 * @return true if the button interaction was successful, false otherwise
	 */
	public abstract boolean button(BTNC command);

	/**
	 * Performs a checkbox interaction based on the given command.
	 * 
	 * @param command parameters defining the checkbox interaction
	 * @return true if successful, false otherwise
	 */
	public abstract boolean checkbox(CHKC command);

	/**
	 * Performs a data grid button interaction using the specified command.
	 * 
	 * @param command parameters for the data grid button action
	 * @return true if successful, false otherwise
	 */
	public abstract boolean dataGridButton(DGBC command);

	/**
	 * Performs a data grid selection action using the specified command.
	 * 
	 * @param command parameters for data grid selection
	 * @return true if successful, false otherwise
	 */
	public abstract boolean dataGridSelect(DGSC command);

	/**
	 * Performs a list grid button interaction based on the given command.
	 * 
	 * @param command parameters defining the list grid button action
	 * @return true if successful, false otherwise
	 */
	public abstract boolean listGridButton(LGBC command);

	/**
	 * Performs a list grid selection action based on the given command.
	 * 
	 * @param command parameters defining the list grid selection
	 * @return true if successful, false otherwise
	 */
	public abstract boolean listGridSelect(LGSC command);

	/**
	 * Looks up description by row using the specified command.
	 * 
	 * @param command parameters for lookup by row
	 * @return true if successful, false otherwise
	 */
	public abstract boolean lookupDescriptionByRow(LDRC command);

	/**
	 * Looks up description by search using the specified command.
	 * 
	 * @param command parameters for lookup by search
	 * @return true if successful, false otherwise
	 */
	public abstract boolean lookupDescriptionBySearch(LDSC command);

	/**
	 * Performs a radio button interaction using the given command.
	 * 
	 * @param command parameters defining the radio button action
	 * @return true if successful, false otherwise
	 */
	public abstract boolean radio(RADC command);

	/**
	 * Performs a tab interaction using the given command.
	 * 
	 * @param command parameters defining the tab action
	 * @return true if successful, false otherwise
	 */
	public abstract boolean tab(TABC command);

	/**
	 * Performs a text input interaction using the given command.
	 * 
	 * @param command parameters defining the text input
	 * @return true if successful, false otherwise
	 */
	public abstract boolean text(TXTC command);

	/**
	 * Performs a select-one interaction using the given command.
	 * 
	 * @param command parameters defining the selection
	 * @return true if successful, false otherwise
	 */
	public abstract boolean selectOne(SELC command);

	/**
	 * Confirms a modal prompt.
	 * 
	 * @return true if successful, false otherwise
	 */
	public abstract boolean confirm();

	/**
	 * Verifies if the last operation succeeded.
	 * 
	 * @return true if successful, false otherwise
	 */
	public abstract boolean verifySuccess();

	/**
	 * Asserts that the last operation succeeded.
	 * <p>
	 * Throws an assertion error if it failed.
	 */
	public abstract void assertSuccess();

	/**
	 * Verifies if the last operation failed.
	 * 
	 * @return true if failure detected, false otherwise
	 */
	public abstract boolean verifyFailure();

	/**
	 * Verifies if the last operation failed with the specified message.
	 * 
	 * @param messageToCheck message to check for failure
	 * @return true if failure detected, false otherwise
	 */
	public abstract boolean verifyFailure(String messageToCheck);

	/**
	 * Asserts that the last operation failed.
	 * <p>
	 * Throws an assertion error if it succeeded.
	 */
	public abstract void assertFailure();

	/**
	 * Asserts that the last operation failed with the specified message.
	 * <p>
	 * Throws an assertion error if it succeeded.
	 * 
	 * @param messageToCheck message to check for failure
	 */
	public abstract void assertFailure(String messageToCheck);

	/**
	 * Describes the messages the page shows, for failure reports.
	 * 
	 * @return the message text, or a description of their absence; never null
	 */
	@SuppressWarnings("static-method")
	public @Nonnull String describeMessages() {
		return "(no message details available)";
	}

	/**
	 * Outputs a trace comment to the console.
	 * 
	 * @param comment comment to output
	 */
	@SuppressWarnings("static-method")
	public void trace(String comment) {
		System.out.println(comment);
	}

	/**
	 * Pauses the current thread for the specified milliseconds.
	 * 
	 * @param millis time to pause in milliseconds
	 * @throws DomainException if interrupted during sleep
	 */
	@SuppressWarnings("static-method")
	public void pause(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			throw new DomainException("Couldn't pause", e);
		}
	}
}

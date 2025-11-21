package util.sail;

import static com.codeborne.selenide.Selectors.byName;
import static com.codeborne.selenide.Selenide.$;
import static com.codeborne.selenide.Selenide.open;

import org.skyve.domain.messages.DomainException;

import com.codeborne.selenide.WebDriverRunner;

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

	private String baseUrl;

	@Override
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		super.startBrowser(configuration);

		WebDriverRunner.setWebDriver(driver);
		this.baseUrl = configuration.getBaseUrl();
	}

	/**
	 * Opens the specified relative URL on the base site.
	 * 
	 * @param url relative URL path
	 */
	public void get(String url) {
		open(baseUrl + url);
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
	 * Performs login using optional customer, username, and password.
	 * 
	 * @param customer optional customer identifier
	 * @param username login username
	 * @param password login password
	 */
	public void login(String customer, String username, String password) {
		open(baseUrl);
		if (customer != null) {
			$(byName("customer")).val(customer);
		}

		$(byName("user")).val(username);
		$(byName("password")).val(password);

		$("input[type=\"submit\"").click();
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

package sail.admin.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.Locator.Kind;

import com.codeborne.selenide.Selenide;
import com.codeborne.selenide.WebDriverRunner;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Typing fast into a SmartClient field whose change handler calls the server keeps every key, and a button clicked
 * straight after the typing still runs.
 * <p>
 * The User wizard's new password rerenders the form when it changes. SAIL's executor types such a field one key at a
 * time, so this test types the whole password in one burst of key events, as a fast typist or a paste of keys would,
 * through Selenium directly. It first leaves the field with Tab and checks the value. It then types the password again
 * and clicks Next at once, which commits the field, whose rerender would mask the click: the wizard must move on to its
 * groups step (with the Save button), which it does only when the new password matches the confirmation.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class SmartClientFastTypingSailIT extends AbstractSmartClientInterpretedSail {

	private static final String VIEW = "admin.User";
	private static final String TYPED_PASSWORD = "SailFast7TypingOfAPassword";

	/** The new password the wizard holds, and the text its field shows (the wizard is a window over the Users view). */
	private static final String NEW_PASSWORD = "var v = _sail.topView();" +
												"return v._vm.getValue('newPassword') + '|' + v._vm.getItem('newPassword').getElementValue();";

	public SmartClientFastTypingSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void aBurstOfKeysIsKeptAndAClickStraightAfterRuns() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"The user name and password step of the User wizard\"><method>" +
							"<navigateMenu module=\"admin\" item=\"Security Admin/Users\" />" +
							"<tabSelect path=\"Users\" />" +
							"<listGridNew module=\"admin\" query=\"qUsers\" createView=\"true\" />" +
							"<dataEnter binding=\"searchContactName\" value=\"SAIL Fast Typist {run}\" />" +
							"<dataEnter binding=\"searchEmail\" value=\"sailfast.{run}@example.com\" />" +
							"<action name=\"Check\" />" +
							"<action name=\"New\" />" +
							"<dataEnter binding=\"userName\" value=\"sailfast{run}\" />" +
							"<dataEnter binding=\"confirmPassword\" value=\"" + TYPED_PASSWORD + "\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		WebDriver driver = WebDriverRunner.getWebDriver();
		WebElement field = selenium.require(Locator.sail(VIEW, Kind.item, "newPassword", null, null, null, null)).element();

		// A burst of keys, then Tab
		field.click();
		new Actions(driver).sendKeys(TYPED_PASSWORD).perform();
		new Actions(driver).sendKeys(Keys.TAB).perform();
		selenium.settle();
		Assertions.assertEquals(TYPED_PASSWORD + '|' + TYPED_PASSWORD, Selenide.executeJavaScript(NEW_PASSWORD), "the new password after typing it in one burst");

		// A burst of keys over the whole value, then a click on Next at once
		WebElement next = selenium.require(Locator.sail(VIEW, Kind.button, "Next", null, null, null, null)).element();
		field = selenium.require(Locator.sail(VIEW, Kind.item, "newPassword", null, null, null, null)).element();
		field.click();
		Selenide.executeJavaScript("arguments[0].select();", field);
		new Actions(driver).sendKeys(TYPED_PASSWORD).moveToElement(next).click().perform();
		selenium.settle();
		Assertions.assertTrue(selenium.require(Locator.sail(VIEW, Kind.button, "Save", null, null, null, null)).shown(),
								"Next ran and showed the groups step, which has the Save button");
	}

	/** Counts the date item's change events in window._sailDateChanges. */
	private static final String COUNT_DATE_CHANGES = "var item = isc.BizUtil.getCurrentView()._vm.getItem('date');" +
														"window._sailDateChanges = 0;" +
														"var changed = item.changed;" +
														"item.changed = function() {" +
														"  window._sailDateChanges++;" +
														"  return changed ? changed.apply(this, arguments) : true;" +
														"};";

	/**
	 * The date a gather returns (committing the typed value or not), whether the view has changed, the text the date's
	 * field shows, the number of change events, and the focused element's name.
	 */
	private static final String GATHERED_DATE = "var v = isc.BizUtil.getCurrentView();" +
												"var d = v.gather(false, arguments[0]).date;" +
												"var item = v._vm.getItem('date');" +
												"return (d ? d.getFullYear() + '-' + (d.getMonth() + 1) + '-' + d.getDate() : null) +" +
												"  '|' + v._vm.valuesHaveChanged() + '|' + document.getElementsByName('date_dateTextField')[0].value +" +
												"  '|' + window._sailDateChanges + '|' + document.activeElement.name;";

	/**
	 * A value typed into an item that changes on commit, and not committed yet (the field still has the focus), is left
	 * alone by a gather for a request in the background, such as a list grid's fetch: the user is still typing. A gather
	 * the user asked for (an action, a save, a cancel, a zoom) commits it first, as if the user had left the field, so it
	 * is gathered and makes the view changed. A date item's text field is such an item: SmartClient does not commit it
	 * when its values are read.
	 */
	@Test
	void aValueTypedButNotCommittedIsGatheredOnlyForTheUser() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"A view with a date\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ListAttributes\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));
		Selenide.executeJavaScript(COUNT_DATE_CHANGES);
		String today = Selenide.executeJavaScript("var d = isc.BizUtil.getCurrentView()._vm.getValue('date');" +
													"return d.getFullYear() + '-' + (d.getMonth() + 1) + '-' + d.getDate();");

		WebDriver driver = WebDriverRunner.getWebDriver();
		WebElement date = driver.findElement(org.openqa.selenium.By.name("date_dateTextField"));
		date.click();
		Selenide.executeJavaScript("arguments[0].select();", date);

		// Half way through typing, a background gather (as a list grid's fetch makes) leaves the typed text alone
		new Actions(driver).sendKeys("01 02").perform();
		Assertions.assertEquals(today + "|false|01 02|0|date_dateTextField", Selenide.executeJavaScript(GATHERED_DATE, Boolean.FALSE),
								"a background gather while typing");

		// The rest of the date, then a gather the user asked for commits it
		new Actions(driver).sendKeys(" 2021").perform();
		// (the date's text is left out: committing can reformat it)
		String gathered = Selenide.executeJavaScript(GATHERED_DATE, Boolean.TRUE);
		Assertions.assertTrue(gathered.startsWith("2021-2-1|true|") && gathered.endsWith("|1|date_dateTextField"),
								"the user's gather: the typed date gathered, the view changed, one change event, and the field still focused: " +
									gathered);
	}
}

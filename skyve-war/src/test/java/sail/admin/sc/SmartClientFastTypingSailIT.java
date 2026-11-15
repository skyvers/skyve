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
 * straight after the typing still runs (runtime issue SC-8).
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
}

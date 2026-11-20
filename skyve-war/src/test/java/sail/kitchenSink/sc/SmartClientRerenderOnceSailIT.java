package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
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
 * Leaving a SmartClient field whose change handler and blur handler both rerender the view sends one rerender.
 * <p>
 * Both handlers run when the field is left, and each defers its rerender a moment, so that a button clicked straight
 * after runs after the rerender. Before EditView dropped a deferred call already waiting, both rerenders were sent at
 * once on one conversation: the server kept whichever finished last, the view showed whichever response arrived last,
 * and a rerender that rebuilds a collection could leave the view showing rows the conversation did not hold. The Rerender
 * Fixture counts the rerenders the server is sent for Typed Text outside the conversation, so two requests sent at once
 * count as two; Show Rerender Count shows the count.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class SmartClientRerenderOnceSailIT extends AbstractSmartClientInterpretedSail {

	private static final String VIEW = "kitchensink.RerenderFixture";

	private static final String OPEN_FIXTURE = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
												"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
												"<interaction name=\"The Rerender Fixture\"><method>" +
												"<navigateEdit module=\"kitchensink\" document=\"RerenderFixture\" />";

	private static final String END = "</method></interaction></automation>";

	/** The count Show Rerender Count last showed, and the text Typed Text holds, in the view's values. */
	private static final String SHOWN = "var v = isc.BizUtil.getCurrentView();" +
										"return v._vm.getValue('rerenderCount') + '|' + v._vm.getValue('typed');";

	public SmartClientRerenderOnceSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	/** Typing and leaving the field with Tab, as SAIL's dataEnter does, sends one rerender. */
	@Test
	void leavingTheFieldSendsOneRerender() {
		String script = OPEN_FIXTURE +
							"<dataEnter binding=\"typed\" value=\"left with Tab\" />" +
							"<action name=\"ShowRerenderCount\" />" +
							"<testValue binding=\"rerenderCount\" value=\"1\" />" +
							END;
		Assertions.assertDoesNotThrow(() -> sailString(script));
	}

	/**
	 * Typing and then clicking Show Rerender Count at once, without leaving the field first, sends one rerender, and the
	 * click runs after it: the count it shows is 1.
	 */
	@Test
	void aClickStraightAfterTypingRunsAfterOneRerender() {
		Assertions.assertDoesNotThrow(() -> sailString(OPEN_FIXTURE + END));

		WebDriver driver = WebDriverRunner.getWebDriver();
		WebElement field = selenium.require(Locator.sail(VIEW, Kind.item, "typed", null, null, null, null)).element();
		WebElement show = selenium.require(Locator.sail(VIEW, Kind.button, "ShowRerenderCount", null, null, null, null)).element();
		field.click();
		new Actions(driver).sendKeys("clicked at once").moveToElement(show).click().perform();
		// the rerender, then the held click's action
		selenium.settle();

		Assertions.assertEquals("1|clicked at once", Selenide.executeJavaScript(SHOWN),
								"the count Show Rerender Count showed, and the text typed");
	}
}

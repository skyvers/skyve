package sail.kitchenSink.sc;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs automated SAIL tests for the kitchensink module (SmartClient).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 */
class SmartClientKitchenSinkSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void test() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/kitchenSink/sc/sc-kitchensink-sail.xml"));
	}

	/**
	 * Finds by content: list grid filters and sorting, captured generated values, options by code or label,
	 * and lookup description rows by their text.
	 */
	@Test
	void find() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/kitchenSink/sc/sc-kitchensink-sail-find.xml"));
	}

	/**
	 * The steps that find by content fail with a reason when the page cannot do what they say: no such link, no such
	 * option, and an operator an enum column does not take. Run under Verify, so that every interaction runs and each
	 * failure is reported.
	 */
	@Test
	void findFailures() {
		String script = """
				<automation uxui="desktop" userAgentType="desktop" testStrategy="Verify" xmlns="http://www.skyve.org/xml/sail">
					<before>
						<login user="admin" password="admin" />
						<switchMode />
					</before>
					<interaction name="No such link">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="No such link {run}" />
						</method>
					</interaction>
					<interaction name="No such option">
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<dataEnter binding="combo" value="Fourth" match="label" />
						</method>
					</interaction>
					<interaction name="No such operator">
						<method>
							<navigateMenu module="kitchensink" item="List" />
							<listGridFilter module="kitchensink" document="ListAttributes" column="constantEnum" value="One (1)!" operator="greaterThan" />
						</method>
					</interaction>
					<after>
						<logout />
					</after>
				</automation>
				""";
		AssertionError e = Assertions.assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		Assertions.assertAll(() -> Assertions.assertTrue(message.contains("with 3 failure(s)"), message),
								() -> Assertions.assertTrue(message.contains("found no displayed link containing it: the page's links are ["), message),
								() -> Assertions.assertTrue(message.contains("no option has the label Fourth"), message),
								() -> Assertions.assertTrue(message.contains("holds an enum, which is filtered with equals, not greaterThan"), message));
	}
}

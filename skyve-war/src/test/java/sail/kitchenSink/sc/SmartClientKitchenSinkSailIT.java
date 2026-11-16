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
		// each step that threw is attached with its cause, so its stack trace shows where it failed
		Throwable[] thrown = e.getSuppressed();
		Assertions.assertAll(() -> Assertions.assertEquals(3, thrown.length),
												() -> Assertions.assertTrue(java.util.Arrays.stream(thrown).allMatch(t -> t.getCause() != null)),
								() -> Assertions.assertTrue(message.contains("with 3 failure(s)"), message),
								() -> Assertions.assertTrue(message.contains("found no displayed link containing it: the page's links are ["), message),
								() -> Assertions.assertTrue(message.contains("no option has the label Fourth"), message),
								() -> Assertions.assertTrue(message.contains("holds an enum, which is filtered with equals, not greaterThan"), message));
	}

	/**
	 * The window steps fail with a reason when the page cannot do what they say: a link that opens its page in this
	 * window rather than a new one, a link that opens a new window without {@code window="new"}, {@code <closeWindow/>}
	 * with no window to close, and {@code <navigateBack/>} with no earlier page. A window left open by an interaction
	 * that failed is closed, so the next interaction has no window to close either. Run under Verify, so that every
	 * interaction runs and each failure is reported.
	 */
	@Test
	void windowFailures() {
		String script = """
				<automation uxui="desktop" userAgentType="desktop" testStrategy="Verify" xmlns="http://www.skyve.org/xml/sail">
					<before>
						<login user="admin" password="admin" />
						<switchMode />
					</before>
					<interaction name="A link that opens its page in this window">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open the selected lookup value" exact="true" window="new" />
						</method>
					</interaction>
					<interaction name="No window to close">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<closeWindow />
						</method>
					</interaction>
					<interaction name="A link that opens a new window, without window">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open a PDF in a new window" />
						</method>
					</interaction>
					<interaction name="No earlier page in a new window">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open a PDF in a new window" window="new" />
							<navigateBack />
						</method>
					</interaction>
					<interaction name="The failed interaction's window was closed">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<testWidget binding="selectedVueId" />
							<closeWindow />
						</method>
					</interaction>
					<after>
						<logout />
					</after>
				</automation>
				""";
		AssertionError e = Assertions.assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		Assertions.assertAll(() -> Assertions.assertTrue(message.contains("with 5 failure(s)"), message),
								() -> Assertions.assertTrue(message.contains("opened its page in this window, not a new one"), message),
								() -> Assertions.assertTrue(message.contains("opens another window (target _blank): follow it with <navigateLink window=\"new\" />"), message),
								() -> Assertions.assertTrue(message.contains("There is no earlier page in this window's history"), message),
								// the step follows the harness's own switch into the PDF's window: no focus baseline, so no note of a loss
								() -> Assertions.assertTrue(java.util.Arrays.stream(message.split("\n  - ")).filter(f -> f.contains("There is no earlier page"))
											.noneMatch(f -> f.contains("lost the OS focus")), message),
								() -> Assertions.assertTrue(message.contains("\"The failed interaction's window was closed\" step 3 <closeWindow />: MetaDataException: " +
																				"<closeWindow /> has no window to close"), message),
								() -> Assertions.assertEquals(2, message.split("has no window to close", -1).length - 1, message));
	}
}

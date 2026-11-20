package sail.kitchenSink.sc;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import sail.kitchenSink.ScriptStepFailures;
import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * The SmartClient kitchen sink tests that need Java: the failure messages of the steps that cannot do what they say.
 * The conformance scripts themselves ({@code sc-kitchensink-sail.xml}, {@code sc-kitchensink-sail-find.xml}) run in
 * the kitchen sink suite ({@code KitchenSinkSuiteSailIT}).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
@Tag("smoke")
class SmartClientKitchenSinkSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
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
					<description>Signs in as admin in the SmartClient UI and tries steps that cannot do what they say: following a link that does not exist, entering an option that does not exist, and filtering an enum column with an operator it does not take. The test expects the script to fail, reporting all three problems with a clear reason for each.</description>
					<before>
						<login user="admin" password="admin" />
						<switchMode />
					</before>
					<interaction name="No such link">
						<description>Opens the List Grid Fixture and follows a link that is not on the page, which is expected to fail by saying which links the page has.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="No such link {run}" />
						</method>
					</interaction>
					<interaction name="No such option">
						<description>Opens the Kitchen Sink and enters an option label that the combo does not have, which is expected to fail by saying that no option has that label.</description>
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<dataEnter binding="combo" value="Fourth" match="label" />
						</method>
					</interaction>
					<interaction name="No such operator">
						<description>Opens the Kitchen Sink list and filters an enum column with greater than, which is expected to fail by saying that an enum is filtered with equals.</description>
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
					<description>Signs in as admin in the SmartClient UI and tries window steps that cannot do what they say: following links that open the wrong kind of window, closing a window when none is open, and going back with no earlier page. The test expects the script to fail, reporting each problem with a clear reason.</description>
					<before>
						<login user="admin" password="admin" />
						<switchMode />
					</before>
					<interaction name="A link that opens its page in this window">
						<description>Opens the List Grid Fixture and follows a link as if it opened a new window, which is expected to fail because the link opens its page in this window.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open the selected lookup value" exact="true" window="new" />
						</method>
					</interaction>
					<interaction name="No window to close">
						<description>Opens the List Grid Fixture and closes a window when none is open, which is expected to fail.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<closeWindow />
						</method>
					</interaction>
					<interaction name="A link that opens a new window, without window">
						<description>Follows a link that opens a new window without saying so, which is expected to fail by telling the author to follow it with window="new".</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open a PDF in a new window" />
						</method>
					</interaction>
					<interaction name="No earlier page in a new window">
						<description>Follows a link into a new window and goes back, which is expected to fail because the new window has no earlier page.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="Open a PDF in a new window" window="new" />
							<navigateBack />
						</method>
					</interaction>
					<interaction name="The failed interaction's window was closed">
						<description>Opens the List Grid Fixture, tests a widget that is not there, and closes a window, which is expected to fail because the window the failed interaction left was closed already.</description>
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

	/**
	 * The {@code execute} failures, in this renderer: see {@link ScriptStepFailures#executeFailures()}.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the assertions are in ScriptStepFailures
	void executeFailures() {
		scriptStepFailures().executeFailures();
	}

	/**
	 * The {@code executeAsync} failures, in this renderer: see {@link ScriptStepFailures#executeAsyncFailures()}.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the assertions are in ScriptStepFailures
	void executeAsyncFailures() {
		scriptStepFailures().executeAsyncFailures();
	}

	/**
	 * The {@code waitFor} failures, in this renderer: see {@link ScriptStepFailures#waitForFailures()}.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the assertions are in ScriptStepFailures
	void waitForFailures() {
		scriptStepFailures().waitForFailures();
	}

	/**
	 * The {@code testScript} failures, in this renderer: see {@link ScriptStepFailures#testScriptFailures()}.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the assertions are in ScriptStepFailures
	void testScriptFailures() {
		scriptStepFailures().testScriptFailures();
	}

	/**
	 * The {@code call} failures, in this renderer: see {@link ScriptStepFailures#callFailures()}.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the assertions are in ScriptStepFailures
	void callFailures() {
		scriptStepFailures().callFailures();
	}

	/**
	 * Returns the script and call steps' inline failure tests, signed in as this kitchen sink's scripts sign in.
	 */
	private ScriptStepFailures scriptStepFailures() {
		return new ScriptStepFailures("desktop", "<switchMode />", this::sailString);
	}
}

package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * The PrimeFaces kitchen sink tests that need Java: the switch back from SmartClient, and the failure messages of the
 * steps that cannot do what they say. The conformance scripts themselves ({@code pf-kitchensink-sail.xml},
 * {@code pf-kitchensink-row-link.xml}) run in the kitchen sink suite ({@code KitchenSinkSuiteSailIT}).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
@Tag("smoke")
class PrimeFacesKitchenSinkSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	/**
	 * {@code <switchMode />} from SmartClient back to PrimeFaces. SAIL has no step that switches away from the
	 * automation's renderer, so the switch to SmartClient is made here with PrimeFaces' own user menu (real clicks),
	 * between two automations in the same browser session.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void switchModeFromSmartClient() {
		sailString(automation("<login user=\"admin\" password=\"admin\" /><testSuccess /><testMenu module=\"kitchensink\" />"));
		selenium.switchToSmartClient();
		sailString(automation("<switchMode />" +
								"<testMenu module=\"kitchensink\" />" +
								"<navigateMenu module=\"kitchensink\" item=\"Actions Fixture\" />" +
								"<action name=\"RunFromToolbar\" />" +
								"<testValue binding=\"outcome\" value=\"Run From Toolbar ran\" />" +
								"<logout />"));
	}

	/**
	 * The steps added for long lists, options and lookups fail with a reason when the page cannot do what they say:
	 * no such option, operator, lookup row, link or action, a column the Vue list grid does not show, and a sort of an empty data table.
	 * Run under Verify, so that every interaction runs and each failure is reported.
	 */
	@Test
	void unmatchedOptionsColumnsAndRowsFail() {
		String script = """
				<automation uxui="tablet" userAgentType="tablet" testStrategy="Verify" xmlns="http://www.skyve.org/xml/sail">
					<interaction name="Login">
						<method>
							<login user="admin" password="admin" />
						</method>
					</interaction>
					<interaction name="No such option">
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<tabSelect path="Widgets" />
							<dataEnter binding="combo" value="Fourth" match="label" />
						</method>
					</interaction>
					<interaction name="A text column has no greater than">
						<method>
							<navigateList module="kitchensink" document="LookupDescription" />
							<listGridFilter module="kitchensink" document="LookupDescription" column="description" value="x" operator="greaterThan" />
						</method>
					</interaction>
					<interaction name="No such lookup row">
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<tabSelect path="Widgets" />
							<lookupDescriptionPick binding="lookupDescription" rowContaining="no such row {run}" />
						</method>
					</interaction>
					<interaction name="A hidden column">
						<method>
							<navigateList module="kitchensink" document="ListAttributes" />
							<listGridFilter module="kitchensink" document="ListAttributes" column="time" value="10:00" />
						</method>
					</interaction>
					<interaction name="An empty data table">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<listGridFilter module="kitchensink" query="qListGridFixtureFacesAttributes" column="text" value="no such row {run}" />
							<testGridRows module="kitchensink" query="qListGridFixtureFacesAttributes" maxRows="0" />
							<listGridSort module="kitchensink" query="qListGridFixtureFacesAttributes" column="text" descending="true" />
						</method>
					</interaction>
					<interaction name="No such action">
						<method>
							<navigateEdit module="kitchensink" document="ActionsFixture" />
							<testWidget action="NoSuchAction{run}" visible="false" />
						</method>
					</interaction>
					<interaction name="No such link">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<navigateLink text="No such link {run}" />
						</method>
					</interaction>
				</automation>
				""";
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		assertAll(() -> assertTrue(message.contains("with 7 failure(s)"), message),
					() -> assertTrue(message.contains("has no option with the label 'Fourth'"), message),
					() -> assertTrue(message.contains("is a text column, which cannot filter with greaterThan"), message),
					() -> assertTrue(message.contains("has no row containing 'no such row "), message),
					() -> assertTrue(message.contains("column [time] (Time) is not shown in the Vue list grid"), message),
					() -> assertTrue(message.contains("PrimeFaces does not sort an empty data table"), message),
					() -> assertTrue(message.contains("found no displayed link containing it: the page's links are ["), message),
					() -> assertTrue(message.contains("<testWidget /> [NoSuchAction{run}] is not in the view"), message));
	}

	/**
	 * The window steps fail with a reason when the page cannot do what they say: a link that opens its page in this
	 * window rather than a new one, a link that opens a new window without {@code window="new"}, {@code <closeWindow/>}
	 * with no window to close, and {@code <navigateBack/>} with no earlier page; and {@code <listGridTool/>} fails,
	 * because PrimeFaces' list grids have no Refresh or Deselect tool. A window left open by an interaction that failed
	 * is closed, so the next interaction has no window to close either. Run under Verify, so that every interaction runs
	 * and each failure is reported.
	 */
	@Test
	void windowAndToolFailures() {
		String script = """
				<automation uxui="tablet" userAgentType="tablet" testStrategy="Verify" xmlns="http://www.skyve.org/xml/sail">
					<before>
						<login user="admin" password="admin" />
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
					<interaction name="No Refresh tool">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<listGridTool module="kitchensink" query="qListGridFixtureVue" tool="refresh" />
						</method>
					</interaction>
					<interaction name="No Deselect tool">
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<listGridTool module="kitchensink" query="qListGridFixtureFaces" tool="clearSelection" />
						</method>
					</interaction>
					<after>
						<logout />
					</after>
				</automation>
				""";
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		assertAll(() -> assertTrue(message.contains("with 7 failure(s)"), message),
					() -> assertTrue(message.contains("opened its page in this window, not a new one"), message),
					() -> assertTrue(message.contains("opens another window (target _blank): follow it with <navigateLink window=\"new\" />"), message),
					() -> assertTrue(message.contains("There is no earlier page in this window's history"), message),
					// the step follows the harness's own switch into the PDF's window: no focus baseline, so no note of a loss
					() -> assertTrue(java.util.Arrays.stream(message.split("\n  - ")).filter(f -> f.contains("There is no earlier page"))
								.noneMatch(f -> f.contains("lost the OS focus")), message),
					() -> assertTrue(message.contains("\"The failed interaction's window was closed\" step 3 <closeWindow />: MetaDataException: " +
															"<closeWindow /> has no window to close"), message),
					() -> assertEquals(2, message.split("has no window to close", -1).length - 1, message),
					() -> assertTrue(message.contains("<listGridTool tool=\"refresh\" /> on [kitchensink.qListGridFixtureVue] is not available in PrimeFaces: " +
															"its Vue list grids and data tables have no Refresh tool"), message),
					() -> assertTrue(message.contains("<listGridTool tool=\"clearSelection\" /> on [kitchensink.qListGridFixtureFaces] is not available in PrimeFaces: " +
															"its Vue list grids and data tables have no Deselect tool"), message));
	}

	/**
	 * {@code <execute/>} fails when its script throws, with the error's message, and when it keeps a variable but its
	 * script gives no value. Run under Verify: a step that throws ends its interaction, so the step after the throw
	 * does not run.
	 */
	@Test
	void executeFailures() {
		String script = signedIn("Verify", """
				<interaction name="A throw">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute><![CDATA[throw new Error(['execute', 'threw', 'this'].join(' '));]]></execute>
						<testScript message="this never runs after the throw">return false;</testScript>
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute variable="nothing"><![CDATA[window._sailNothing = true;]]></execute>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		String thrown = failureOf(message, "A throw");
		String noValue = failureOf(message, "No value to keep");
		assertAll(() -> assertTrue(message.contains("with 2 failure(s)"), "the count: " + message),
					() -> assertTrue(thrown.contains("JavascriptException"), "A throw: " + message),
					() -> assertTrue(thrown.contains("execute threw this"), "A throw: " + message),
					() -> assertFalse(message.contains("this never runs after the throw"), "A throw: " + message),
					() -> assertTrue(noValue.contains("gave no value to keep in {var:nothing}"), "No value to keep: " + message));
	}

	/**
	 * {@code <executeAsync/>} fails when its promise rejects, with the error's message, when it does not settle within
	 * {@code timeoutMillis}, and when it keeps a variable but resolves to no value. Run under Verify.
	 */
	@Test
	void executeAsyncFailures() {
		String script = signedIn("Verify", """
				<interaction name="A rejection">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync><![CDATA[await Promise.resolve(); throw new Error(['executeAsync', 'rejected', 'this'].join(' '));]]></executeAsync>
					</method>
				</interaction>
				<interaction name="Not settled in time">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync timeoutMillis="300"><![CDATA[await new Promise(function () { /* never settles */ });]]></executeAsync>
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync variable="nothing"><![CDATA[await Promise.resolve();]]></executeAsync>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		String rejected = failureOf(message, "A rejection");
		assertAll(() -> assertTrue(message.contains("with 3 failure(s)"), "the count: " + message),
					() -> assertTrue(rejected.contains("JavascriptException"), "A rejection: " + message),
					() -> assertTrue(rejected.contains("executeAsync rejected this"), "A rejection: " + message),
					() -> assertTrue(failureOf(message, "Not settled in time").contains("<executeAsync /> did not settle within 300 ms"),
										"Not settled in time: " + message),
					() -> assertTrue(failureOf(message, "No value to keep").contains("gave no value to keep in {var:nothing}"),
										"No value to keep: " + message));
	}

	/**
	 * {@code <waitFor/>} fails when its script has not held within {@code timeoutMillis}, giving the script's first
	 * line and, when a poll threw, the last poll's error. It is a wait, not a test: under Verify the timeout ends its
	 * interaction, and under None it still fails the run while a false {@code <testScript/>} before it is skipped.
	 */
	@Test
	void waitForFailures() {
		String verify = signedIn("Verify", """
				<interaction name="A poll that throws">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<waitFor timeoutMillis="300"><![CDATA[throw new Error(['no', 'poll', 'held'].join(' '));]]></waitFor>
					</method>
				</interaction>
				<interaction name="Never true">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<waitFor timeoutMillis="300"><![CDATA[return window._sailNeverSet === true;]]></waitFor>
						<testScript message="this never runs after the wait">return false;</testScript>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(verify));
		String message = e.getMessage();
		String threw = failureOf(message, "A poll that throws");
		String never = failureOf(message, "Never true");

		String none = signedIn("None", """
				<interaction name="A wait under None">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript message="this is skipped under None">return false;</testScript>
						<waitFor timeoutMillis="300">return false;</waitFor>
					</method>
				</interaction>
				""");
		AssertionError n = assertThrows(AssertionError.class, () -> sailString(none));
		String noneMessage = n.getMessage();
		assertAll(() -> assertTrue(message.contains("with 2 failure(s)"), "the count: " + message),
					() -> assertTrue(threw.contains("<waitFor /> did not hold within 300 ms: throw new Error("), "A poll that throws: " + message),
					() -> assertTrue(threw.contains("; the last poll threw: "), "A poll that throws: " + message),
					() -> assertTrue(threw.contains("no poll held"), "A poll that throws: " + message),
					() -> assertTrue(never.contains("<waitFor /> did not hold within 300 ms: return window._sailNeverSet === true;"),
										"Never true: " + message),
					() -> assertFalse(never.contains("the last poll threw"), "Never true: " + message),
					() -> assertFalse(message.contains("this never runs after the wait"), "Never true: " + message),
					() -> assertTrue(noneMessage.contains("<waitFor /> did not hold within 300 ms: return false;"), "under None: " + noneMessage),
					() -> assertFalse(noneMessage.contains("this is skipped under None"), "under None: " + noneMessage));
	}

	/**
	 * {@code <testScript/>} fails as an assertion: with its {@code message}, its variables substituted, when it is
	 * still false at {@code timeoutMillis}; with the script's first line when it has no message; and with what the
	 * last run threw. Run under Verify: a failed assertion is recorded, and the next step runs.
	 */
	@Test
	void testScriptFailures() {
		String script = signedIn("Verify", """
				<interaction name="False, with a message">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute variable="kept"><![CDATA[return ['kept', 'value'].join(' ');]]></execute>
						<testScript message="The kept value was {var:kept}" timeoutMillis="300"><![CDATA[return sail.vars.kept === 'another value';]]></testScript>
						<testScript message="this ran after the failed assertion">return false;</testScript>
					</method>
				</interaction>
				<interaction name="False, without a message">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript><![CDATA[return 1 + 1 === 3;]]></testScript>
					</method>
				</interaction>
				<interaction name="A run that throws">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript><![CDATA[throw new Error(['the', 'test', 'threw'].join(' '));]]></testScript>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		String withMessage = failureOf(message, "False, with a message");
		String withoutMessage = failureOf(message, "False, without a message");
		String threw = failureOf(message, "A run that throws");
		assertAll(() -> assertTrue(message.contains("with 4 failure(s)"), "the count: " + message),
					() -> assertTrue(withMessage.contains("The kept value was kept value"), "False, with a message: " + message),
					() -> assertTrue(withMessage.contains("this ran after the failed assertion"), "False, with a message: " + message),
					() -> assertTrue(withoutMessage.contains("<testScript /> did not hold: return 1 + 1 === 3;"), "False, without a message: " + message),
					() -> assertFalse(withoutMessage.contains("; it threw: "), "False, without a message: " + message),
					() -> assertTrue(threw.contains("<testScript /> did not hold: throw new Error("), "A run that throws: " + message),
					() -> assertTrue(threw.contains("; it threw: "), "A run that throws: " + message),
					() -> assertTrue(threw.contains("the test threw"), "A run that throws: " + message));
	}

	/**
	 * {@code <call/>} fails when its class cannot be loaded, is not a {@code SailCall} or cannot be constructed (the
	 * {@code SailCall} interface itself), when it keeps a variable but the call gives no value, when the call throws,
	 * and when a parameter is named twice. Run under Verify: an {@link AssertionError} from the call is a failed
	 * assertion, so the next step runs; any other exception ends the interaction.
	 */
	@Test
	void callFailures() {
		String script = signedIn("Verify", """
				<interaction name="No such class">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.NoSuchCall" />
					</method>
				</interaction>
				<interaction name="Not a SailCall">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="java.lang.String" />
					</method>
				</interaction>
				<interaction name="Cannot be constructed">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="org.skyve.metadata.sail.execution.SailCall" />
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.EchoCall" variable="nothing" />
					</method>
				</interaction>
				<interaction name="A failed assertion">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.EchoCall">
							<parameter name="text" value="the assertion's text" />
							<parameter name="fail" value="assert" />
						</call>
						<testScript message="this ran after the call's failed assertion">return false;</testScript>
					</method>
				</interaction>
				<interaction name="An error">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.EchoCall">
							<parameter name="text" value="the error's text" />
							<parameter name="fail" value="error" />
						</call>
						<testScript message="this never runs after the call's error">return false;</testScript>
					</method>
				</interaction>
				<interaction name="A duplicate parameter">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.EchoCall">
							<parameter name="text" value="one" />
							<parameter name="text" value="two" />
						</call>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		String assertion = failureOf(message, "A failed assertion");
		String error = failureOf(message, "An error");
		assertAll(() -> assertTrue(message.contains("with 8 failure(s)"), "the count: " + message),
					() -> assertTrue(failureOf(message, "No such class").contains("<call className=\"sail.kitchenSink.NoSuchCall\" /> names a class that cannot be loaded"),
										"No such class: " + message),
					() -> assertTrue(failureOf(message, "Not a SailCall").contains("<call className=\"java.lang.String\" /> names a class that is not a org.skyve.metadata.sail.execution.SailCall"),
										"Not a SailCall: " + message),
					() -> assertTrue(failureOf(message, "Cannot be constructed").contains("<call className=\"org.skyve.metadata.sail.execution.SailCall\" /> names a class that cannot be constructed"),
										"Cannot be constructed: " + message),
					() -> assertTrue(failureOf(message, "No value to keep").contains("gave no value to keep in {var:nothing}"), "No value to keep: " + message),
					() -> assertTrue(assertion.contains("<call className=\"sail.kitchenSink.EchoCall\" /> failed: EchoCall was asked to fail an assertion on the assertion's text"),
										"A failed assertion: " + message),
					() -> assertTrue(assertion.contains("this ran after the call's failed assertion"), "A failed assertion: " + message),
					() -> assertTrue(error.contains("<call className=\"sail.kitchenSink.EchoCall\" /> threw "), "An error: " + message),
					() -> assertTrue(error.contains("IllegalStateException: EchoCall was asked to throw on the error's text"), "An error: " + message),
					() -> assertFalse(message.contains("this never runs after the call's error"), "An error: " + message),
					() -> assertTrue(failureOf(message, "A duplicate parameter").contains("has more than one <parameter name=\"text\" />"),
										"A duplicate parameter: " + message));
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Switch mode\"><method>" + steps + "</method></interaction></automation>";
	}

	/**
	 * Returns an automation with the given test strategy that signs in as admin before the interactions and out after them.
	 *
	 * @param testStrategy	the test strategy
	 * @param interactions	the {@code <interaction/>} elements
	 * @return the automation's XML
	 */
	private static String signedIn(String testStrategy, String interactions) {
		return """
				<automation uxui="tablet" userAgentType="tablet" testStrategy="%s" xmlns="http://www.skyve.org/xml/sail">
					<before>
						<login user="admin" password="admin" />
					</before>
				%s
					<after>
						<logout />
					</after>
				</automation>
				""".formatted(testStrategy, interactions);
	}

	/**
	 * Returns what the named interaction recorded in a Verify run's failure message.
	 *
	 * @param message	the run's failure message, one failure per {@code "\n  - "}
	 * @param interaction	the interaction's name
	 * @return its failures, one per line, or an empty string if it recorded none
	 */
	private static String failureOf(String message, String interaction) {
		String prefix = "Interaction \"" + interaction + "\" step ";
		return Arrays.stream(message.split("\n  - ")).filter(f -> f.startsWith(prefix)).collect(Collectors.joining("\n"));
	}
}

package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import sail.kitchenSink.ScriptStepFailures;
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
					<description>Signs in as admin in the PrimeFaces tablet UX/UI and tries steps that cannot do what they say: picking an option, an operator, a lookup row, a link or an action that does not exist, filtering a column that the Vue list grid does not show, and sorting an empty data table. The test expects the script to fail, reporting all seven problems with a clear reason for each.</description>
					<interaction name="Login">
						<description>Signs in as admin.</description>
						<method>
							<login user="admin" password="admin" />
						</method>
					</interaction>
					<interaction name="No such option">
						<description>Opens the Kitchen Sink Widgets tab and enters an option label that the combo does not have, which is expected to fail by saying the combo has no option with that label.</description>
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<tabSelect path="Widgets" />
							<dataEnter binding="combo" value="Fourth" match="label" />
						</method>
					</interaction>
					<interaction name="A text column has no greater than">
						<description>Filters a text column of the Lookup Description list with greater than, which is expected to fail by saying that a text column cannot filter that way.</description>
						<method>
							<navigateList module="kitchensink" document="LookupDescription" />
							<listGridFilter module="kitchensink" document="LookupDescription" column="description" value="x" operator="greaterThan" />
						</method>
					</interaction>
					<interaction name="No such lookup row">
						<description>Picks a lookup row that does not exist, which is expected to fail by saying that no row contains the text.</description>
						<method>
							<navigateEdit module="kitchensink" document="KitchenSink" />
							<tabSelect path="Widgets" />
							<lookupDescriptionPick binding="lookupDescription" rowContaining="no such row {run}" />
						</method>
					</interaction>
					<interaction name="A hidden column">
						<description>Filters the List Attributes list by a column that the Vue list grid does not show, which is expected to fail by saying so.</description>
						<method>
							<navigateList module="kitchensink" document="ListAttributes" />
							<listGridFilter module="kitchensink" document="ListAttributes" column="time" value="10:00" />
						</method>
					</interaction>
					<interaction name="An empty data table">
						<description>Filters a data table to no rows and then sorts it, which is expected to fail because PrimeFaces does not sort an empty data table.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<listGridFilter module="kitchensink" query="qListGridFixtureFacesAttributes" column="text" value="no such row {run}" />
							<testGridRows module="kitchensink" query="qListGridFixtureFacesAttributes" maxRows="0" />
							<listGridSort module="kitchensink" query="qListGridFixtureFacesAttributes" column="text" descending="true" />
						</method>
					</interaction>
					<interaction name="No such action">
						<description>Tests an action that the Actions Fixture does not have, which is expected to fail by saying the action is not in the view.</description>
						<method>
							<navigateEdit module="kitchensink" document="ActionsFixture" />
							<testWidget action="NoSuchAction{run}" visible="false" />
						</method>
					</interaction>
					<interaction name="No such link">
						<description>Follows a link that is not on the page, which is expected to fail by saying which links the page has.</description>
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
					<description>Signs in as admin in the PrimeFaces tablet UX/UI and tries window and tool steps that cannot do what they say: following links that open the wrong kind of window, closing a window when none is open, going back with no earlier page, and using the Refresh and Deselect tools that PrimeFaces list grids do not have. The test expects the script to fail, reporting each problem with a clear reason.</description>
					<before>
						<login user="admin" password="admin" />
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
						<description>Tests a widget that is not there and then closes a window, which is expected to fail because the window the failed interaction left was closed already.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<testWidget binding="selectedVueId" />
							<closeWindow />
						</method>
					</interaction>
					<interaction name="No Refresh tool">
						<description>Uses the Refresh tool on a Vue list grid, which is expected to fail because PrimeFaces list grids have no Refresh tool.</description>
						<method>
							<navigateEdit module="kitchensink" document="ListGridFixture" />
							<listGridTool module="kitchensink" query="qListGridFixtureVue" tool="refresh" />
						</method>
					</interaction>
					<interaction name="No Deselect tool">
						<description>Uses the Deselect tool on a data table, which is expected to fail because PrimeFaces data tables have no Deselect tool.</description>
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
		return new ScriptStepFailures("tablet", "", this::sailString);
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<description>Runs part of the test that switches from SmartClient back to PrimeFaces, in the tablet UX/UI as admin: signing in and checking the kitchensink menu, and then, once the test has switched to SmartClient, switching back, running an action on the Actions Fixture and signing out. " +
				"Checks that the menu is there and that the action runs after the switch.</description>" +
				"<interaction name=\"Switch mode\"><description>Runs one stage of the switch back from SmartClient: signing in, or, after the test has switched to SmartClient, switching back, running an action and signing out.</description><method>" + steps + "</method></interaction></automation>";
	}
}

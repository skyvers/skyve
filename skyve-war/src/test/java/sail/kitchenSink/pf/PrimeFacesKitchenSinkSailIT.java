package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs automated SAIL tests for the kitchensink module (PrimeFaces).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 * The script's test strategy decides how failures are reported; the test fails with an {@link AssertionError}
 * naming each failed interaction and step.
 */
class PrimeFacesKitchenSinkSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void test() {
		sailResource("sail/kitchenSink/pf/pf-kitchensink-sail.xml");
	}

	/**
	 * {@code <dataGridLink />}: the Actions Fixture's Run From Row link in a row, declined and confirmed.
	 */
	@Test
	void rowLink() {
		sailResource("sail/kitchenSink/pf/pf-kitchensink-row-link.xml");
	}

	/**
	 * {@code <switchMode />} from SmartClient back to PrimeFaces. SAIL has no step that switches away from the
	 * automation's renderer, so the switch to SmartClient is made here with PrimeFaces' own user menu (real clicks),
	 * between two automations in the same browser session.
	 */
	@Test
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

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Switch mode\"><method>" + steps + "</method></interaction></automation>";
	}
}

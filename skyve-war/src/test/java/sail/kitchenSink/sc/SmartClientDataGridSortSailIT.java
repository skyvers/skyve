package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.Locator.Kind;

import com.codeborne.selenide.Selenide;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.commands.impl.SmartClientButtonCommand;

/**
 * Sorting an unordered SmartClient data grid by a column header only changes what the grid shows: saving afterwards
 * keeps the collection's own order.
 * <p>
 * No SAIL step sorts a data grid, so after a script has made a saved fixture with three rows, the header click is made
 * through {@link util.sail.SmartClientSelenide#sortDataGrid}, a real click, and so is the Save that follows. The Save's
 * response lists the collection as the server holds it after the save, which the test checks. (Reopening the record
 * would not show it, as a reload lists unordered rows in the database's order.)
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class SmartClientDataGridSortSailIT extends AbstractSmartClientInterpretedSail {

	private static final String VIEW = "kitchensink.ActionsFixture";

	/** Keeps the response to the next Save request in window._sailSaveResponse. */
	private static final String RECORD_SAVE_RESPONSE = "var send = XMLHttpRequest.prototype.send;" +
														"XMLHttpRequest.prototype.send = function(body) {" +
														"  var xhr = this;" +
														"  if ((typeof body === 'string') && (new URLSearchParams(body).get('_a') === 'Save')) {" +
														"    xhr.addEventListener('load', function() {window._sailSaveResponse = xhr.responseText;});" +
														"    XMLHttpRequest.prototype.send = send;" +
														"  }" +
														"  return send.apply(this, arguments);" +
														"};";

	/** The last letter of each row's name in the Save response's rows, comma separated. */
	private static final String SAVED_ROWS = "var found = null;" +
												"var walk = function(o) {" +
												"  if (found || (! o) || (typeof o !== 'object')) return;" +
												"  if (Array.isArray(o.rows)) {found = o.rows; return;}" +
												"  Object.keys(o).forEach(function(k) {walk(o[k]);});" +
												"};" +
												"walk(JSON.parse(window._sailSaveResponse || '{}'));" +
												"return (found || []).map(function(r) {return String(r.name).slice(-1);}).join(',');";

	/** The last letter of each row's name in the order the grid holds them, comma separated. */
	private static final String GRID_ROWS = "var g = Object.values(isc.BizUtil.getCurrentView()._grids.rows)[0].grid;" +
											"return g.getData().map(function(r) {return String(r.name).slice(-1);}).join(',');";

	public SmartClientDataGridSortSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void aHeaderSortDoesNotReorderTheCollection() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<description>Signs in as admin in the SmartClient UI, opens a new kitchensink ActionsFixture, adds three rows named b, c and a, and saves it. " +
							"Checks that the grid holds exactly three rows after the save. The test then sorts the grid by name in the browser and saves again, to show that sorting does not change the order of the saved collection.</description>" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"Rows b, c and a, saved\"><description>Adds three rows in the order b, c, a, saves, and checks that the grid has three rows.</description><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<tabSelect path=\"Rows\" />" +
							"<dataGridNew binding=\"rows\" /><dataEnter binding=\"name\" value=\"SAIL sort {run} b\" /><zoomOut />" +
							"<dataGridNew binding=\"rows\" /><dataEnter binding=\"name\" value=\"SAIL sort {run} c\" /><zoomOut />" +
							"<dataGridNew binding=\"rows\" /><dataEnter binding=\"name\" value=\"SAIL sort {run} a\" /><zoomOut />" +
							"<save />" +
							"<testGridRows binding=\"rows\" minRows=\"3\" maxRows=\"3\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		// Sort the grid by its Name column with a click on the header: it then shows (and holds) a, b, c
		selenium.sortDataGrid(Locator.sail(VIEW, Kind.dataGrid, "rows", null, null, "rows", null), "name", false);
		Assertions.assertEquals("a,b,c", Selenide.executeJavaScript(GRID_ROWS), "the rows the grid holds after the header sort");

		// Save, and read the collection the server holds afterwards from the response
		Selenide.executeJavaScript(RECORD_SAVE_RESPONSE);
		Assertions.assertTrue(selenium.button(new SmartClientButtonCommand(Locator.sail(VIEW, Kind.button, "Save", null, null, null, null), null)));
		Assertions.assertEquals("b,c,a", Selenide.executeJavaScript(SAVED_ROWS), "the order of the rows the server holds after the save");
	}
}

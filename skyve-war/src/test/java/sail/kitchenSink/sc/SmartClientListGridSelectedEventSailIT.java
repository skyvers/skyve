package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.codeborne.selenide.Selenide;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Selecting a SmartClient list grid row whose selected event rerenders the view runs the event once, keeps the row
 * selected and sets the selected id binding (runtime issue SC-10).
 * <p>
 * The List Grid Fixture's "PrimeFaces data table" list grid has a {@code selectedIdBinding} and an
 * {@code onSelectedHandlers} {@code <rerender>}. The rerender's response refreshes the view's list grids. The refresh
 * deselected the row, which ran the selected event again, whose response refreshed the grid again, for ever.
 * <p>
 * An {@code <execute>} step counts the edit requests and the grid's selection events, then {@code listGridSelect}
 * clicks the row and waits for SmartClient to settle: no request pending and no deferred call due. A loop never
 * settles, as each refresh queues the next selected event. Once settled, there was one selection event and one
 * request, the row is selected and the view's selected id is its bizId; and nothing more happens when it settles again.
 * When the row is then filtered out and the grid refreshed, the view's selected id is cleared, without the event.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class SmartClientListGridSelectedEventSailIT extends AbstractSmartClientInterpretedSail {

	/** Finds the PrimeFaces data table list grid of the current view, as the variable faces. */
	private static final String FACES = "var v = isc.BizUtil.getCurrentView();" +
										"var faces = null;" +
										"Object.values(v._grids).forEach(function(grids) {" +
										"  Object.values(grids).forEach(function(g) {" +
										"    if (g._dataSource && /_qListGridFixtureFaces$/.test(g._dataSource.ID)) faces = g;" +
										"  });" +
										"});";

	/** Counts the edit requests and the grid's selection events in window._sailCounts. */
	private static final String COUNT = FACES +
										"window._sailCounts = {edits: 0, selections: 0};" +
										"var open = XMLHttpRequest.prototype.open;" +
										"XMLHttpRequest.prototype.open = function(method, url) {" +
										"  if (String(url).indexOf('smartedit') >= 0) window._sailCounts.edits++;" +
										"  return open.apply(this, arguments);" +
										"};" +
										"var selectionUpdated = faces.grid.selectionUpdated;" +
										"faces.grid.selectionUpdated = function() {" +
										"  window._sailCounts.selections++;" +
										"  return selectionUpdated.apply(this, arguments);" +
										"};";

	/** The counts, the selected row's description, and whether the view's selected id is the selected row's bizId. */
	private static final String STATE = FACES +
										"var r = faces.grid.getSelectedRecord();" +
										"return 'edits=' + window._sailCounts.edits + ' selections=' + window._sailCounts.selections +" +
										"  ' selected=' + (r ? r.description : null) +" +
										"  ' bound=' + (!!r && (v._vm.getValue('selectedFacesId') === r.bizId));";

	public SmartClientListGridSelectedEventSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void aSelectedEventThatRerendersRunsOnceAndKeepsTheSelection() {
		String description = "SAIL selected event " + System.currentTimeMillis();
		String select = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"A lookup value to select\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"LookupDescription\" />" +
							"<dataEnter binding=\"description\" value=\"" + description + "\" />" +
							"<save />" +
							"</method></interaction>" +
							"<interaction name=\"Select it in the List Grid Fixture's PrimeFaces data table list grid\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ListGridFixture\" />" +
							"<listGridFilter module=\"kitchensink\" query=\"qListGridFixtureFaces\" column=\"description\" value=\"" + description + "\" />" +
							"<testGridRows module=\"kitchensink\" query=\"qListGridFixtureFaces\" minRows=\"1\" maxRows=\"1\" />" +
							"<execute>" + COUNT.replace("&", "&amp;").replace("<", "&lt;") + "</execute>" +
							"<listGridSelect module=\"kitchensink\" query=\"qListGridFixtureFaces\" rowContaining=\"" + description + "\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(select));

		String expected = "edits=1 selections=1 selected=" + description + " bound=true";
		Assertions.assertEquals(expected, Selenide.executeJavaScript(STATE), "after selecting the row");
		selenium.settle();
		Assertions.assertEquals(expected, Selenide.executeJavaScript(STATE), "after settling again");

		// Filter the row out and refresh the grid (as its Refresh button does): no row is selected, the view's selected id
		// is cleared rather than left holding the row, and the selected event does not run
		Selenide.executeJavaScript(FACES + "faces.grid.setFilterEditorCriteria({description: 'no such row'}); faces.refresh();");
		selenium.settle();
		Assertions.assertEquals("edits=1 selections=1 selected=null bound=false", Selenide.executeJavaScript(STATE), "after the row was filtered out");
		Assertions.assertNull(Selenide.executeJavaScript(FACES + "return v._vm.getValue('selectedFacesId') || null;"),
								"the view's selected id after the row was filtered out");

		// Remove the lookup value (a view opened by its URL has no Delete, so zoom into it from its list)
		String remove = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"Remove the lookup value\"><method>" +
							"<navigateList module=\"kitchensink\" document=\"LookupDescription\" />" +
							"<listGridFilter module=\"kitchensink\" document=\"LookupDescription\" column=\"description\" value=\"" + description + "\" />" +
							"<listGridZoom module=\"kitchensink\" document=\"LookupDescription\" rowContaining=\"" + description + "\" />" +
							"<delete />" +
							"<testGridRows module=\"kitchensink\" document=\"LookupDescription\" maxRows=\"0\" />" +
							"</method></interaction>" +
							"<after><logout /></after></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(remove));
	}
}

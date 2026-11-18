package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.codeborne.selenide.Selenide;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * A growl pushed to a SmartClient page is added to the growls showing, and does not replace them.
 * <p>
 * Backup and Restore growl that their job has started, and the job pushes a growl when it finishes, often within a
 * fraction of a second. Whether the admin suites' {@code testMessage} steps read the first growl before the push arrives
 * is a race, so this test shows a growl as an action's response does ({@code isc.BizUtil.growl}), then hands the page a
 * pushed growl as the push stream does ({@code isc.BizUtil.onPushMessage}), and checks that both are in the page.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class SmartClientPushGrowlSailIT extends AbstractSmartClientInterpretedSail {

	/** Shows an action's growl, then a pushed growl, and returns the growls in the page. */
	private static final String GROWL_THEN_PUSH = "isc.BizUtil.growl([{severity: 'info', summary: 'SAIL job started'}]);" +
													"isc.BizUtil.onPushMessage([{type: 'g', severity: 'info', message: 'SAIL job finished'}]);" +
													"return Array.from(document.querySelectorAll('.ui-growl-item'))" +
													"  .map(function(g) {return g.innerText.trim();}).join('|');";

	public SmartClientPushGrowlSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void aPushedGrowlKeepsTheGrowlsShowing() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"A view\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		Assertions.assertEquals("SAIL job started|SAIL job finished", Selenide.executeJavaScript(GROWL_THEN_PUSH));
	}
}

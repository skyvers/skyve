package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.codeborne.selenide.Selenide;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Leaving a SmartClient field whose change handler and blur handler both rerender the view sends the two rerenders one
 * after the other, never two at once on one conversation.
 * <p>
 * The Rerender Fixture's Week Ending Date mirrors the Biz Hub Apps weekly timesheet's week ending date, which no other
 * kitchen sink fixture does: a date shown by a text field, with a rerender in both its change and its blur handlers.
 * Leaving it after a change sends both rerenders, and, as before 16 November 2026, the second is sent only once the
 * first has been answered. The fast-typing change (c42551e257) deferred the change handler's rerender as the blur
 * handler's is, so both were sent at once on one conversation: the server kept whichever finished last, the view showed
 * whichever response arrived last, and a rerender that rebuilds a collection could leave the view showing rows the
 * conversation did not hold (the Apps timesheets' W6b failure).
 * <p>
 * The test counts the view's requests in flight in the browser, as they are sent and answered, and checks that there
 * was never more than one. It does not hold answers back: it measures the overlap there is. The fixture counts the
 * rerenders the server is sent for Week Ending Date outside the conversation, and Show Rerender Count shows the count,
 * so the test also checks that both rerenders reached the server.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class SmartClientRerenderOnceSailIT extends AbstractSmartClientInterpretedSail {

	/**
	 * Counts the view's requests in flight in window._sailInFlight, and the most at once in window._sailMostInFlight.
	 * Every server event of an edit view is sent through its values manager's saveData and answered through the callback
	 * given to it. The answers are not held: each goes to the view as it arrives.
	 */
	private static final String COUNT_IN_FLIGHT = "var vm = isc.BizUtil.getCurrentView()._vm;" +
													"var save = vm.saveData;" +
													"window._sailInFlight = 0;" +
													"window._sailMostInFlight = 0;" +
													"vm.saveData = function(callback, properties) {" +
													"  window._sailInFlight++;" +
													"  window._sailMostInFlight = Math.max(window._sailMostInFlight, window._sailInFlight);" +
													"  return save.call(this, function() {" +
													"    window._sailInFlight--;" +
													"    return callback ? callback.apply(this, arguments) : undefined;" +
													"  }, properties);" +
													"};";

	/** The most requests the view had in flight at once, and the count Show Rerender Count last showed. */
	private static final String MEASURED = "return window._sailMostInFlight + '|' + isc.BizUtil.getCurrentView()._vm.getValue('rerenderCount');";

	public SmartClientRerenderOnceSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	/**
	 * Entering a date and leaving the field with Tab, as SAIL's dataEnter does, sends its change and blur rerenders one
	 * after the other: never more than one request in flight, and both rerenders counted by the server.
	 */
	@Test
	void leavingTheFieldSendsItsRerendersOneAfterTheOther() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<description>Signs in as admin in the SmartClient UI and opens the kitchen sink's Rerender Fixture, whose Week Ending Date mirrors the Biz Hub Apps weekly timesheet's week ending date: a date in a text field that rerenders the view from both its change handler and its blur handler. The test checks that leaving the field after entering a date sends the two rerenders one after the other, as before 16 November 2026, never two at once on one conversation, as the fast-typing change did.</description>" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"Week Ending Date left with Tab\"><description>Counts the view's requests in flight, enters a date into Week Ending Date and leaves it with Tab, which runs its change and blur handlers, then shows the number of rerenders the server was sent for it. The test then reads the most requests in flight at once, which must be 1, and the count, which must be 2.</description><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"RerenderFixture\" />" +
							"<execute><![CDATA[" + COUNT_IN_FLIGHT + "]]></execute>" +
							"<dataEnter binding=\"weekEndingDate\" value=\"07-Nov-2025\" />" +
							"<action name=\"ShowRerenderCount\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		String measured = Selenide.executeJavaScript(MEASURED);
		String[] values = measured.split("\\|");
		String message = "the most requests in flight at once, and the rerenders the server counted: " + values[0] + " and " + values[1];
		Assertions.assertAll(() -> Assertions.assertEquals("1", values[0], message),
								() -> Assertions.assertEquals("2", values[1], message));
	}
}

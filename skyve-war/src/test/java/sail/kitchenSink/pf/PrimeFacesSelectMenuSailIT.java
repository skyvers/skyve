package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Map;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;

import com.codeborne.selenide.WebDriverRunner;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * A select menu step ends only when its drop down has closed. At the end of the drop down's closing transition
 * PrimeFaces hides it and focuses the menu, which scrolls the window back to the menu; if that comes after the step,
 * it can move the page under the next step's click (the final headed gate of 970d4c50f0 failed so: Data Maintenance's
 * Import/Export tab header was clicked under the fixed environment banner).
 * <p>
 * The step used to wait for the menu's {@code aria-expanded} to be false. An item clicked while the drop down is still
 * opening (in a headed browser, whose frames can come late) closes it before {@code aria-expanded} was ever true, so
 * that wait ended at once. Here the drop down's transitions are slowed, so that the item is clicked while it is
 * opening and the drop down is still closing when a step that ends too early returns.
 */
@Tag("sail")
class PrimeFacesSelectMenuSailIT extends AbstractPrimeFacesInterpretedSail {
	public PrimeFacesSelectMenuSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	/**
	 * A select menu low in a long view (Data Maintenance's Evict Option, below the Data Refresh grid) has closed, and
	 * has the focus, when the step that picks an item ends; the page does not move after it.
	 */
	@Test
	void aPickedItemClosesTheDropDownWithinTheStep() {
		sailString("<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
					"<description>Signs in as admin in the PrimeFaces tablet UX/UI and picks an item from a select menu low in the Data Maintenance view, with the menu's drop down slowed down. " +
					"Checks that the step that picks the item ends only when the drop down has closed, so that the page does not move under the next step.</description>" +
					"<interaction name=\"Select menu\"><description>Opens the Data Refresh tab of Data Maintenance and picks an Evict Option while the drop down is still opening.</description><method>" +
					"<login user=\"admin\" password=\"admin\" />" +
					"<testSuccess />" +
					"<navigateEdit module=\"admin\" document=\"DataMaintenance\" />" +
					"<tabSelect path=\"Data Refresh\" />" +
					"<comment>Slow drop downs: the item is clicked while the drop down opens; it then takes 2s to close</comment>" +
					"<execute>var s = document.createElement('style');" +
					"s.textContent = '.ui-selectonemenu-panel.ui-connected-overlay-enter-active, " +
					".ui-selectonemenu-panel.ui-connected-overlay-exit-active {transition-duration: 2s !important;}';" +
					"document.head.appendChild(s);" +
					"document.addEventListener('click', function(e) {var i = e.target.closest('.ui-selectonemenu-item');" +
					"if (i) window.sailClickedWhileOpening = ! i.closest('.ui-selectonemenu-panel').classList.contains('ui-connected-overlay-enter-done');}, true);" +
					"</execute>" +
					"<dataEnter binding=\"evictOption\" value=\"1\" />" +
					"<comment>When the step ends: is the drop down still shown; then go to the top, as the next step's click on a tab would</comment>" +
					"<execute>window.sailAtStepEnd = {shown: jQuery('.ui-selectonemenu-panel:visible').length > 0," +
					"focus: document.activeElement ? document.activeElement.id : null};" +
					"window.scrollTo(0, 0);</execute>" +
					"</method></interaction></automation>");

		// Wait for every drop down to have closed: a step that ended too early leaves its closing transition running,
		// and the focus (and scroll) it ends with is what this test measures
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return jQuery('.ui-selectonemenu-panel:visible').length === 0;")) ? Boolean.TRUE : null);
		@SuppressWarnings("unchecked")
		Map<String, Object> atStepEnd = (Map<String, Object>) js("return window.sailAtStepEnd;");
		Object clickedWhileOpening = js("return window.sailClickedWhileOpening;");
		Object scrollY = js("return Math.round(window.scrollY);");
		Object focus = js("return document.activeElement ? document.activeElement.id : null;");
		selenium.trace("at the step's end " + atStepEnd + "; once closed, scrolled to " + scrollY + " with the focus in " + focus);
		assertAll(() -> assertEquals(Boolean.TRUE, clickedWhileOpening, "the item was clicked while the drop down was opening"),
					() -> assertEquals(Boolean.FALSE, atStepEnd.get("shown"), "the drop down has closed when the step ends"),
					() -> assertEquals(Long.valueOf(0), scrollY, "the page stays where it was put after the step"),
					() -> assertEquals(atStepEnd.get("focus"), focus, "the focus stays where it was when the step ended"));

		sailString("<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
					"<description>Signs out the admin user who the first script signed in, to end the session cleanly.</description>" +
					"<interaction name=\"Sign out\"><description>Signs out.</description><method><logout /></method></interaction></automation>");
	}

	private static Object js(String script) {
		return ((JavascriptExecutor) WebDriverRunner.getWebDriver()).executeScript(script);
	}
}

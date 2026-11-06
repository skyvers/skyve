package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebElement;

import com.codeborne.selenide.Selenide;
import com.codeborne.selenide.WebDriverRunner;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Where PrimeFaces puts the focus (runtime issue P11, docs/sail-runtime-issues.md). Each view's form has a
 * {@code <p:focus/>}, which focuses the first input of a page that has just been loaded (a GET). It used to do so after
 * every ajax response as well, which scrolled the page to its first input and took the focus from the field the user
 * had moved to.
 * <p>
 * The steps that SAIL has gestures for are SAIL steps. The rerender is made with the browser directly, because the
 * test needs to measure the page between the harness's scroll to the field and the response.
 */
class PrimeFacesFocusSailIT extends AbstractPrimeFacesInterpretedSail {
	/** The input PrimeFaces focuses: the first enabled, shown input with a name that is not a button (PrimeFaces.focus). */
	private static final String FIRST_INPUT_JS = "return jQuery(':not(:submit):not(:button):input:visible:enabled[name]').get(0) || null;";

	/**
	 * Counts the deferred focuses PrimeFaces starts ({@code PrimeFaces.focus} focuses from a timer) and the page's script
	 * errors, so the test can wait until each focus has run and check that the response had none.
	 */
	private static final String WATCH_JS = "var focus = PrimeFaces.focus;" +
											"window.sailFocusCalls = 0; window.sailFocusPending = 0; window.sailErrors = [];" +
											"window.addEventListener('error', function(e) {window.sailErrors.push(String(e.message));});" +
											"PrimeFaces.focus = function() {" +
											"  window.sailFocusCalls++;" +
											"  var setTimeout = window.setTimeout;" +
											"  window.setTimeout = function(f, ms) {" +
											"    window.sailFocusPending++;" +
											"    return setTimeout.call(window, function() {try {f();} finally {window.sailFocusPending--;}}, ms);};" +
											"  try {return focus.apply(this, arguments);} finally {window.setTimeout = setTimeout;}};";

	public PrimeFacesFocusSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	/**
	 * A change low in a long view that the server rerenders the form for leaves the page where it was and the focus in
	 * the field the user tabbed to; and a page that is loaded (a new view, and a zoom into an existing row) focuses its
	 * first input.
	 */
	@Test
	void aRerenderKeepsTheScrollAndTheFocus() {
		sailString(automation("<login user=\"admin\" password=\"admin\" />" +
								"<testSuccess />" +
								"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />"));
		assertFirstInputFocused("a new Actions Fixture");

		// A window as small as a tablet's in landscape, so that the view is long: with Event Text in the middle of the
		// window, the first input is out of sight above it. (Headless Chrome is 1920x1080, where it is not.)
		WebDriverRunner.getWebDriver().manage().window().setSize(new Dimension(1024, 700));
		// Event Text runs Run From Event on change, which rerenders the form; it is low in the view
		WebElement eventText = Selenide.$("input[title='Event Text']").getWrappedElement();
		js(WATCH_JS);
		// the harness's click scrolls the field to the middle of the window: measure after it
		selenium.click(Selenide.$(eventText));
		long scrollY = ((Number) js("return Math.round(window.scrollY);")).longValue();
		Object firstInputOutOfSight = js("var f = jQuery(':not(:submit):not(:button):input:visible:enabled[name]').get(0);" +
											"var r = (f.closest('.ui-chkbox') || f).getBoundingClientRect(); return r.bottom < 0;");
		selenium.trace("scrolled to " + scrollY + "; type into Event Text and tab to the next field while its change is sent");
		eventText.sendKeys("focus", Keys.TAB);
		String leftFocusIn = (String) js("return document.activeElement ? document.activeElement.id : null;");
		selenium.waitForAjaxResponse();
		// Wait for any deferred focus the response started (PrimeFaces focuses from a timer) to have run
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return window.sailFocusPending === 0;")) ? Boolean.TRUE : null);

		long scrollAfter = ((Number) js("return Math.round(window.scrollY);")).longValue();
		String focusAfter = (String) js("return document.activeElement ? document.activeElement.id : null;");
		Object outcome = js("return document.querySelector(\"input[title='Outcome']\").value;");
		selenium.trace("left the focus in " + leftFocusIn + "; after the response the page is scrolled to " + scrollAfter + " and the focus is in " + focusAfter);
		Object focusCalls = js("return window.sailFocusCalls;");
		@SuppressWarnings("unchecked")
		List<Object> errors = (List<Object>) js("return window.sailErrors;");
		assertAll(() -> assertEquals(Boolean.TRUE, firstInputOutOfSight, "the first input was out of sight, so focusing it would scroll"),
					() -> assertEquals("Run From Event ran", outcome, "the change was sent and the form rerendered"),
					() -> assertEquals(Long.valueOf(scrollY), Long.valueOf(scrollAfter), "the page stays where the user scrolled it"),
					() -> assertEquals(leftFocusIn, focusAfter, "the focus stays in the field the user tabbed to"),
					() -> assertEquals(Long.valueOf(0), Long.valueOf(((Number) focusCalls).longValue()), "the response focuses nothing"),
					() -> assertEquals(List.of(), errors, "the response has no script errors"));

		sailString(automation("<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
								"<tabSelect path=\"Rows\" />" +
								"<dataGridNew binding=\"rows\" />" +
								"<dataEnter binding=\"name\" value=\"SAIL focus row\" />" +
								"<zoomOut />" +
								"<dataGridZoom binding=\"rows\" rowContaining=\"SAIL focus row\" />"));
		assertFirstInputFocused("a zoom into an existing row");
		sailString(automation("<logout />"));
	}

	/**
	 * Asserts that the page's first input has the focus, which the page's {@code <p:focus/>} gives it from a timer after
	 * the page is ready.
	 */
	private void assertFirstInputFocused(String page) {
		boolean already = Boolean.TRUE.equals(js("var f = jQuery(':not(:submit):not(:button):input:visible:enabled[name]').get(0);" +
													"return (f !== undefined) && (f === document.activeElement);"));
		selenium.trace(page + ": first input " + (already ? "already focused" : "not focused yet") + " when the page was ready");
		// Wait for the page's deferred focus (p:focus focuses from a timer after the page is ready)
		selenium.waitFor(d -> Boolean.TRUE.equals(js("var f = jQuery(':not(:submit):not(:button):input:visible:enabled[name]').get(0);" +
														"return (f !== undefined) && (f === document.activeElement);")) ? Boolean.TRUE : null);
		WebElement first = (WebElement) js(FIRST_INPUT_JS);
		selenium.trace(page + ": the first input " + ((first == null) ? null : first.getDomAttribute("id")) + " has the focus");
	}

	private static Object js(String script) {
		return ((JavascriptExecutor) WebDriverRunner.getWebDriver()).executeScript(script);
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Focus\"><method>" + steps + "</method></interaction></automation>";
	}
}

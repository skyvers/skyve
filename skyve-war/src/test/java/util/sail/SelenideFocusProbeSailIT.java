package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WindowType;

import jakarta.annotation.Nonnull;

/**
 * Tests of {@link Selenide#focusProbe()} in a real browser: what counts as the window losing the OS focus.
 * <p>
 * Focus moving into a child frame (such as SmartClient's upload page, or a rich text editor) blurs the page's window
 * while the browser window keeps the OS focus, so it must not count; another tab taking the focus is a real loss, from
 * the page or from one of its frames. The page is a {@code data:} URL with a same-origin ({@code srcdoc}) frame, so
 * these tests need no server; they need Chrome, like the other SAIL ITs. Headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class SelenideFocusProbeSailIT {

	/** A page with a text field, and a frame with a text field and a button. */
	private static final String PAGE = "<html><body><input id='outer'>" +
										"<iframe id='frame' srcdoc='<input id=inner><button id=button>Button</button>'></iframe>" +
										"</body></html>";

	@Nonnull private final SmartClientSelenide selenium = new SmartClientSelenide();

	@BeforeEach
	void startBrowser() {
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome));
		selenium.driver.get("data:text/html;charset=utf-8," + URLEncoder.encode(PAGE, StandardCharsets.UTF_8).replace("+", "%20"));
		// Waits for the frame's document (srcdoc loads after its page), so that its fields can take the focus
		selenium.waitFor(d -> Boolean.valueOf("complete".equals(((JavascriptExecutor) d).executeScript(
				"var f = document.getElementById('frame').contentDocument;" +
				"return (f && f.getElementById('button')) ? f.readyState : null;"))));
	}

	@AfterEach
	void stopBrowser() {
		selenium.stopBrowser();
	}

	/**
	 * Moving the focus into the page's frame, within it, and back out does not count as a loss of the OS focus: the
	 * page's window blurs as the frame takes the focus, but {@code document.hasFocus()} stays true.
	 */
	@Test
	void focusMovingIntoWithinAndOutOfAFrameIsNoLoss() {
		click("outer");
		Selenide.FocusProbe before = probe();

		clickInFrame("inner");
		clickInFrame("button");
		click("outer");
		clickInFrame("inner");
		Selenide.FocusProbe after = probe();

		assertAll(() -> assertEquals(before.page(), after.page()),
					() -> assertEquals(before.blurs(), after.blurs(), "blurs"),
					() -> assertEquals(Boolean.TRUE, Boolean.valueOf(after.hasFocus()), "hasFocus"));
	}

	/**
	 * Removing the frame that has the focus, as SmartClient does when its upload window closes, is no loss either.
	 */
	@Test
	void removingTheFocusedFrameIsNoLoss() {
		clickInFrame("inner");
		Selenide.FocusProbe before = probe();

		((JavascriptExecutor) selenium.driver).executeScript("document.getElementById('frame').remove()");
		Selenide.FocusProbe after = probe();

		assertAll(() -> assertEquals(before.page(), after.page()),
					() -> assertEquals(before.blurs(), after.blurs(), "blurs"));
	}

	/**
	 * Another tab taking the focus from the page is a loss of the OS focus, as another application taking it would be.
	 */
	@Test
	void anotherTabTakingTheFocusFromThePageIsALoss() {
		click("outer");
		Selenide.FocusProbe before = probe();

		anotherTabTakesTheFocus();
		Selenide.FocusProbe after = probe();

		assertAll(() -> assertEquals(before.page(), after.page()),
					() -> assertEquals(before.blurs() + 1L, after.blurs(), "blurs"));
	}

	/**
	 * Another tab taking the focus from a field in the page's frame is a loss of the OS focus too: the frame's window
	 * blurs, not the page's. The focus moves into the frame after the probe, as it does within a step (the executor
	 * probes only as a step starts and when it fails), so the counter must find the frame as the page's window blurs.
	 */
	@Test
	void anotherTabTakingTheFocusFromAFrameIsALoss() {
		click("outer");
		Selenide.FocusProbe before = probe();

		clickInFrame("inner");
		anotherTabTakesTheFocus();
		Selenide.FocusProbe after = probe();

		assertAll(() -> assertEquals(before.page(), after.page()),
					() -> assertEquals(before.blurs() + 1L, after.blurs(), "blurs"));
	}

	/**
	 * Probes the page, installing its counter at the first probe.
	 *
	 * @return the probe
	 */
	private @Nonnull Selenide.FocusProbe probe() {
		Selenide.FocusProbe result = selenium.focusProbe();
		assertNotNull(result, "no focus probe");
		return result;
	}

	/**
	 * Clicks an element of the page. It does not probe, as a step does not: each probe also finds the frames that hold
	 * the focus, which would hide a counter that cannot find them as the focus moves.
	 *
	 * @param id	the element's id
	 */
	private void click(@Nonnull String id) {
		selenium.driver.findElement(By.id(id)).click();
	}

	/**
	 * Clicks an element of the page's frame, which moves the focus into the frame, as the upload step does. It does not
	 * probe either.
	 *
	 * @param id	the element's id
	 */
	private void clickInFrame(@Nonnull String id) {
		selenium.driver.switchTo().frame(selenium.driver.findElement(By.id("frame")));
		try {
			selenium.driver.findElement(By.id(id)).click();
		}
		finally {
			selenium.driver.switchTo().defaultContent();
		}
	}

	/**
	 * Opens another tab, which takes the focus from the page's tab, closes it, and returns to the page.
	 */
	private void anotherTabTakesTheFocus() {
		String page = selenium.driver.getWindowHandle();
		selenium.driver.switchTo().newWindow(WindowType.TAB);
		selenium.driver.close();
		selenium.driver.switchTo().window(page);
	}
}

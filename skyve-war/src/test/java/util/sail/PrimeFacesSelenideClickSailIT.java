package util.sail;

import static com.codeborne.selenide.Selenide.$;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;

import jakarta.annotation.Nonnull;

/**
 * Tests {@link PrimeFacesSelenide#click(com.codeborne.selenide.SelenideElement)} in a real browser on a page of its own:
 * a side menu fixed over the left of the window, and a strip of tabs wider than the window, scrolled so that a tab sits
 * under the menu, as a PrimeFaces view's tabs can be. A user would scroll the strip and click the tab; so must the
 * harness. The page is a {@code data:} URL, so this needs no server; it needs Chrome, like the other SAIL ITs.
 */
@Tag("sail")
class PrimeFacesSelenideClickSailIT {
	/** A fixed side menu, and a scrolling strip whose tab is scrolled to the strip's left edge, under the menu. */
	private static final String PAGE = "<html><body style='margin:0'>" +
										"<div id='menu' style='position:fixed;left:0;top:0;width:256px;height:100%;background:#ddd;z-index:10'>Menu</div>" +
										"<div id='strip' style='position:absolute;left:0;top:100px;width:1000px;overflow-x:auto;white-space:nowrap'>" +
										"<span style='display:inline-block;width:600px'>before</span>" +
										"<a id='tab' href='#' onclick='window.sailClicked = true; return false;' style='display:inline-block;width:71px'>Data Grids</a>" +
										"<span style='display:inline-block;width:2000px'>after</span>" +
										"</div>" +
										"<script>document.getElementById('strip').scrollLeft = 600;</script>" +
										"</body></html>";

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();

	@BeforeEach
	void startBrowser() {
		// a short timeout: a click that cannot be made fails after it
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome)
														.settings(SailSettings.code().timeoutSeconds(5)));
		selenium.driver.get("data:text/html;charset=utf-8," + URLEncoder.encode(PAGE, StandardCharsets.UTF_8).replace("+", "%20"));
	}

	@AfterEach
	void stopBrowser() {
		selenium.stopBrowser();
	}

	/** The tab under the menu is scrolled across into view and clicked. */
	@Test
	void aTabScrolledUnderTheSideMenuIsClicked() {
		JavascriptExecutor js = (JavascriptExecutor) selenium.driver;
		Object left = js.executeScript("return Math.round(document.getElementById('tab').getBoundingClientRect().left);");
		selenium.click($("#tab"));
		assertEquals(Long.valueOf(0), left, "the tab starts under the menu");
		assertEquals(Boolean.TRUE, js.executeScript("return window.sailClicked === true;"));
	}
}

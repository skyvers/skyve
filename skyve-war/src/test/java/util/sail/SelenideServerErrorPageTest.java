package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.WebDriver;

/**
 * Tests how {@link Selenide} recognises Skyve's error page: a URL whose path ends with
 * {@code /pages/error.jsp}, whatever the context, parameters or fragment.
 */
class SelenideServerErrorPageTest {
	@Test
	@SuppressWarnings("static-method")
	void theErrorPageIsRecognised() {
		List<String> urls = List.of("http://localhost:8180/skyve/pages/error.jsp",
									"http://localhost:8180/skyve/pages/error.jsp?errorReference=9d3e93c2",
									"https://example.com/app/pages/error.jsp#top",
									"/pages/error.jsp");
		assertAll(urls.stream().map(url -> () -> assertTrue(Selenide.isServerErrorPage(url), url)));
	}

	@Test
	@SuppressWarnings("static-method")
	void otherPagesAreNot() {
		List<String> urls = Arrays.asList(null,
											"",
											"http://localhost:8180/skyve/?a=e&m=admin&d=ControlPanel",
											"http://localhost:8180/skyve/pages/expired.jsp",
											"http://localhost:8180/skyve/loggedOut?next=/pages/error.jsp",
											"http://localhost:8180/skyve/#/pages/error.jsp",
											"http://localhost:8180/skyve/pages/error.jspx");
		assertAll(urls.stream().map(url -> () -> assertFalse(Selenide.isServerErrorPage(url), String.valueOf(url))));
	}

	/**
	 * The window's URL is returned when it is the error page, and null for another page or when the driver cannot read
	 * it (a closed window, or a page between loads).
	 */
	@Test
	@SuppressWarnings("static-method")
	void theWindowIsReadFromTheDriver() {
		String errorPage = "http://localhost:8180/skyve/pages/error.jsp?errorReference=9d3e93c2";
		WebDriver driver = mock(WebDriver.class);
		PrimeFacesSelenide selenide = new PrimeFacesSelenide();
		selenide.driver = driver;
		when(driver.getCurrentUrl()).thenReturn(errorPage,
												"http://localhost:8180/skyve/?a=e&m=admin&d=ControlPanel")
									.thenThrow(new NoSuchWindowException("closed"));
		assertEquals(errorPage, selenide.serverErrorPage());
		assertNull(selenide.serverErrorPage());
		assertNull(selenide.serverErrorPage());
	}
}

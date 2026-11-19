package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Tests which URLs {@link Selenide#isServerErrorPage(String)} takes for Skyve's error page: its path ends with
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
}

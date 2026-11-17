package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.web.UserAgentType;

/**
 * Tests the source DownloadSAIL wraps around the generated test methods. {@link DownloadSAILH2Test} compiles the real
 * executor's output against the harness.
 */
@SuppressWarnings("static-method")
class DownloadSAILTest {
	private static final String BASE_URL = "http://localhost:8080/skyve/";
	private static final String METHODS = "\tprivate void testFirst() {\n\t}\n\n\t@Test\n\tpublic void test() {\n\t\ttestFirst();\n\t}\n";

	@Test
	void theSourceIsAJUnit5TestOfTheHarnessAroundTheMethods() {
		String source = DownloadSAIL.sailSource(METHODS, BASE_URL, UserAgentType.tablet);
		assertAll(() -> assertTrue(source.contains("import org.junit.jupiter.api.Test;"), "JUnit 5's @Test"),
					() -> assertTrue(source.contains("import org.junit.jupiter.api.BeforeEach;"), "JUnit 5's @BeforeEach"),
					() -> assertTrue(source.contains("import org.junit.jupiter.api.AfterEach;"), "JUnit 5's @AfterEach"),
					() -> assertFalse(source.contains("org.junit.Test") || source.contains("org.junit.Before") ||
										source.contains("org.junit.After"), "no JUnit 4"),
					() -> assertTrue(source.contains("@Tag(\"sail\")\npublic class Sail extends PrimeFacesSelenium {"),
										"a public SAIL test of the harness's PrimeFacesSelenium"),
					() -> assertTrue(source.contains("\t\tstopBrowser();\n"), "stops the browser after each test"),
					() -> assertTrue(source.endsWith(METHODS + "}\n"), "the methods end the class"));
	}

	@Test
	void theBrowserIsTheUserAgentTypesDefaultWithTheBaseUrlAsItsOwnValue() {
		String tablet = DownloadSAIL.sailSource(METHODS, BASE_URL, UserAgentType.tablet);
		String phone = DownloadSAIL.sailSource(METHODS, BASE_URL, UserAgentType.phone);
		String expected = "startBrowser(SailWorkflow.defaultBrowser(UserAgentType.%s).baseUrl(\"" + BASE_URL + "\"));";
		assertAll(() -> assertTrue(tablet.contains(String.format(expected, "tablet")), tablet),
					() -> assertTrue(phone.contains(String.format(expected, "phone")), phone),
					() -> assertFalse(tablet.contains("chromedriver") || tablet.contains("pathToDriver") ||
										tablet.contains("setupChrome"), "Selenium Manager finds the driver"));
	}

	@Test
	void theBaseUrlIsAJavaLiteral() {
		String source = DownloadSAIL.sailSource(METHODS, "http://x/\"y\\", UserAgentType.desktop);
		assertTrue(source.contains(".baseUrl(\"http://x/\\\"y\\\\\")"), source);
	}
}

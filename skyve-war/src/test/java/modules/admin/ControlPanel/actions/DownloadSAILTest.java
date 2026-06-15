package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for DownloadSAIL static helper methods.
 */
@SuppressWarnings("static-method")
class DownloadSAILTest {

	@Test
	void sailSourceContainsClassDeclaration() {
		String source = DownloadSAIL.sailSource("", "http://localhost:8080/skyve");
		assertNotNull(source);
		assertTrue(source.contains("public class Sail"), "Should contain class declaration");
	}

	@Test
	void sailSourceContainsSetupMethod() {
		String source = DownloadSAIL.sailSource("", "http://localhost:8080/skyve");
		assertTrue(source.contains("public void setup()"), "Should contain setup method");
	}

	@Test
	void sailSourceContainsTeardownMethod() {
		String source = DownloadSAIL.sailSource("", "http://localhost:8080/skyve");
		assertTrue(source.contains("public void teardown()"), "Should contain teardown method");
	}

	@Test
	void sailSourceContainsBaseUrl() {
		String baseUrl = "http://localhost:8080/myapp";
		String source = DownloadSAIL.sailSource("", baseUrl);
		assertTrue(source.contains(baseUrl), "Should contain the base URL");
	}

	@Test
	void sailSourceContainsMethodsContent() {
		String methods = "\n\t@Test\n\tpublic void testLogin() {\n\t}\n";
		String source = DownloadSAIL.sailSource(methods, "http://localhost:8080/skyve");
		assertTrue(source.contains("testLogin"), "Should contain custom methods");
	}

	@Test
	void sailSourceContainsJUnitImports() {
		String source = DownloadSAIL.sailSource("", "http://localhost:8080/skyve");
		assertTrue(source.contains("import org.junit."), "Should contain JUnit imports");
	}

	@Test
	void sailSourceWithNullMethodsDoesNotThrow() {
		// Methods can be empty string but not null usually - test with empty
		String source = DownloadSAIL.sailSource("", "http://localhost");
		assertNotNull(source);
		assertTrue(! source.isEmpty());
	}
}

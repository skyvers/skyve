package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Workflow;

/**
 * Checks that every shipped SAIL script still validates against sail.xsd and unmarshals,
 * so that language changes stay backward compatible. A script whose root is {@code <workflow>} is unmarshalled as a
 * workflow of automations; any other script as an {@code <automation>}, as before.
 */
class SailScriptsUnmarshalTest {
	private static final Path SCRIPTS = Paths.get("src/test/java/sail");

	@Test
	@SuppressWarnings("static-method")
	void everyShippedSailScriptUnmarshals() throws IOException {
		List<Path> scripts;
		try (Stream<Path> files = Files.walk(SCRIPTS)) {
			scripts = files.filter(p -> p.toString().endsWith(".xml")).sorted().toList();
		}
		assertFalse(scripts.isEmpty(), "No SAIL scripts found under " + SCRIPTS.toAbsolutePath());

		assertAll(scripts.stream()
							.map(script -> (Executable) () -> assertDoesNotThrow(() -> unmarshal(script), script.toString())));
	}

	/**
	 * The shipped workflow file holds its automations, and is refused as a single automation.
	 */
	@Test
	@SuppressWarnings("static-method")
	void theKitchenSinkWorkflowFileHoldsItsAutomations() {
		String file = SCRIPTS.resolve("kitchenSink/workflow/kitchensink-workflow.xml").toString();
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowFile(file);
		assertAll(() -> assertEquals(2, workflow.getAutomations().size()),
					() -> assertEquals("external", workflow.getAutomations().get(0).getUxui()),
					() -> assertEquals("desktop", workflow.getAutomations().get(1).getUxui()),
					() -> assertTrue(isWorkflow(Path.of(file))));
	}

	private static void unmarshal(Path script) throws IOException {
		if (isWorkflow(script)) {
			assertFalse(XMLMetaData.unmarshalSAILWorkflowFile(script.toString()).getAutomations().isEmpty(), script.toString());
		}
		else {
			XMLMetaData.unmarshalSAILFile(script.toString());
		}
	}

	/**
	 * Indicates if a script's root element is a {@code <workflow>}.
	 */
	private static boolean isWorkflow(Path script) throws IOException {
		String xml = Files.readString(script, StandardCharsets.UTF_8);
		// the root is the first element after the declaration and any comments
		String body = xml.replaceAll("(?s)<\\?.*?\\?>|<!--.*?-->", "").trim();
		return body.startsWith("<workflow") || body.matches("(?s)^<[A-Za-z0-9_]+:workflow\\b.*");
	}
}

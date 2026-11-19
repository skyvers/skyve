package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.util.XMLMetaData.SAILRoot;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.sail.language.SuiteMember;
import org.skyve.metadata.sail.language.Workflow;

/**
 * Checks that every shipped SAIL script still validates against sail.xsd and unmarshals,
 * so that language changes stay backward compatible. A script whose root is {@code <workflow>} is unmarshalled as a
 * workflow of automations, a {@code <suite>} as a suite whose member files exist next to it, and an
 * {@code <automation>} as before.
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
					() -> assertEquals(SAILRoot.WORKFLOW, XMLMetaData.sailRootOfFile(file)));
	}

	private static void unmarshal(Path script) {
		String file = script.toString();
		SAILRoot root = XMLMetaData.sailRootOfFile(file);
		if (root == SAILRoot.SUITE) {
			Suite suite = XMLMetaData.unmarshalSAILSuiteFile(file);
			for (SuiteMember member : suite.getAllMembers()) {
				Path memberFile = script.resolveSibling(member.getFile());
				assertTrue(Files.isRegularFile(memberFile), file + " names " + member.getFile() + ", which does not exist");
			}
		}
		else if (root == SAILRoot.WORKFLOW) {
			assertFalse(XMLMetaData.unmarshalSAILWorkflowFile(file).getAutomations().isEmpty(), file);
		}
		else {
			XMLMetaData.unmarshalSAILFile(file);
		}
	}
}

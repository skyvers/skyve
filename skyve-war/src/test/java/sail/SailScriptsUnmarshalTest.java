package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;

/**
 * Checks that every shipped SAIL script still validates against sail.xsd and unmarshals,
 * so that language changes stay backward compatible.
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
							.map(script -> (Executable) () -> assertDoesNotThrow(() -> XMLMetaData.unmarshalSAILFile(script.toString()),
																					script.toString())));
	}
}

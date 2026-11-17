package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.web.UserAgentType;

/**
 * Tests the source DownloadSAIL wraps around the generated test methods, and how it compiles it against a compile folder.
 * {@link DownloadSAILH2Test} compiles the real executor's output against the harness.
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

	@Test
	void theClassPathIsTheFoldersJarsInNameOrder(@TempDir Path folder) throws IOException {
		Files.createFile(folder.resolve("b.jar"));
		Files.createFile(folder.resolve("a.jar"));
		Files.createFile(folder.resolve("readme.txt"));
		Files.createDirectories(folder.resolve("classes.jar"));
		assertAll(() -> assertEquals(List.of(folder.resolve("a.jar").toFile().getAbsolutePath(),
												folder.resolve("b.jar").toFile().getAbsolutePath()),
										DownloadSAIL.classPath(folder.toFile())),
					() -> assertEquals(List.of(), DownloadSAIL.classPath(folder.resolve("missing").toFile()), "no folder"));
	}

	@Test
	void noJarsGiveTheSourceAndWhatTheFolderNeeds(@TempDir Path folder) throws IOException {
		String source = DownloadSAIL.sailSource(METHODS, BASE_URL, UserAgentType.tablet);
		Download download = DownloadSAIL.compile(source, folder.toFile());
		String text = text(download);
		assertAll(() -> assertEquals("sail.txt", download.getFileName()),
					() -> assertEquals(MimeType.plain, download.getMimeType()),
					() -> assertTrue(text.startsWith("The test was not compiled: there are no .jar files in " +
														folder.toFile().getAbsolutePath() + "\n"), text),
					() -> assertTrue(text.contains(DownloadSAIL.COMPILE_FOLDER_CONTENTS), "what the folder needs"),
					() -> assertTrue(text.endsWith(source), "the source"));
	}

	@Test
	void aCompiledTestIsAJarOfTheClassAndItsSource(@TempDir Path folder) throws IOException {
		emptyJar(folder.resolve("empty.jar"));
		String source = "public class Sail {\n}\n";
		Download download = DownloadSAIL.compile(source, folder.toFile());
		Map<String, byte[]> entries = entries(download);
		assertAll(() -> assertEquals("sail.jar", download.getFileName()),
					() -> assertEquals(MimeType.zip, download.getMimeType()),
					() -> assertTrue(entries.containsKey("Sail.class"), entries::toString),
					() -> assertArrayEquals(source.getBytes(StandardCharsets.UTF_8), entries.get("Sail.java")),
					() -> assertEquals(List.of(folder.resolve("empty.jar").toFile()), List.of(folder.toFile().listFiles()),
										"the class's folder is deleted"));
	}

	@Test
	void aCompileErrorGivesTheErrorTheJarsTheSourceAndWhatTheFolderNeeds(@TempDir Path folder) throws IOException {
		emptyJar(folder.resolve("empty.jar"));
		String source = "public class Sail extends util.sail.NoSuchHarness {\n}\n";
		Download download = DownloadSAIL.compile(source, folder.toFile());
		String text = text(download);
		assertAll(() -> assertEquals("sail.txt", download.getFileName()),
					() -> assertTrue(text.startsWith("The test did not compile against the jars in " +
														folder.toFile().getAbsolutePath() + " (empty.jar):\n"), text),
					() -> assertTrue(text.contains("util.sail"), "the compiler's error"),
					() -> assertTrue(text.contains(DownloadSAIL.COMPILE_FOLDER_CONTENTS), "what the folder needs"),
					() -> assertTrue(text.endsWith(source), "the source"),
					() -> assertEquals(1, folder.toFile().listFiles().length, "the class's folder is deleted"));
	}

	private static void emptyJar(Path jar) throws IOException {
		try (OutputStream out = Files.newOutputStream(jar); JarOutputStream ignored = new JarOutputStream(out)) {
			// an empty jar: the class path needs a jar, and the test classes need nothing from it
		}
	}

	private static String text(Download download) {
		byte[] bytes = download.getBytes();
		assertNotNull(bytes);
		return new String(bytes, StandardCharsets.UTF_8);
	}

	private static Map<String, byte[]> entries(Download download) throws IOException {
		byte[] bytes = download.getBytes();
		assertNotNull(bytes);
		Map<String, byte[]> result = new TreeMap<>();
		try (JarInputStream in = new JarInputStream(new ByteArrayInputStream(bytes))) {
			for (JarEntry entry = in.getNextJarEntry(); entry != null; entry = in.getNextJarEntry()) {
				result.put(entry.getName(), in.readAllBytes());
			}
		}
		return result;
	}
}

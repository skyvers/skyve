package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.Download;

import jakarta.inject.Inject;
import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import util.AbstractH2TestTruncate;

/**
 * Tests the Control Panel's Download of a SAIL as a JUnit 5 test's source on H2. Above all, it generates the test for a
 * small SAIL automation through the Control Panel's own path (the {@code sailExecutor} enum's executor class, then
 * {@link DownloadSAIL#sailSource}) and compiles it with {@code javax.tools} against this test classpath, which holds the
 * SAIL harness ({@code util.sail}). That fails when the generated code stops compiling against the harness: a renamed
 * executor, a template naming a class or method the harness no longer has, or a generator that writes broken Java.
 * The SAIL user is saved and committed, so the data is truncated after each test.
 */
@SuppressWarnings("java:S120") // Module test packages mirror the generated document package.
class DownloadSAILH2Test extends AbstractH2TestTruncate {
	@Inject
	private DownloadSAIL downloadSAIL;
	@Inject
	private DownloadClientSAIL downloadClientSAIL;

	private ControlPanelExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		bean = ControlPanelSAILFixture.controlPanel();
		webContext = new MockWebContext();
	}

	@Test
	void theDownloadIsTheGeneratedTestsSource() throws Exception {
		downloadSAIL.prepare(bean, webContext);
		Download download = downloadSAIL.download(bean, webContext);

		String source = text(download);
		assertAll(() -> assertEquals("Sail.java", download.getFileName()),
					() -> assertEquals(MimeType.plain, download.getMimeType()),
					() -> assertTrue(source.contains("public class Sail extends PrimeFacesSelenium {"), source),
					() -> assertTrue(source.contains("startBrowser(SailWorkflow.defaultBrowser(UserAgentType.tablet).baseUrl(\"" +
														ControlPanelSAILFixture.BASE_URL + "\"));"), source),
					() -> assertTrue(source.contains("login(null, \"TestUser\", \"pass\\\"word\");"), source),
					() -> assertNull(bean.getUnescapedResults(), "the results are cleared"));
	}

	@Test
	void theDownloadedSourceCompilesAgainstTheHarness(@TempDir Path classes) throws Exception {
		downloadSAIL.prepare(bean, webContext);
		String source = text(downloadSAIL.download(bean, webContext));

		assertCompiles(source, classes);
	}

	/**
	 * The external Control Panel's Download generates its module's menu SAIL for the desktop and downloads it the same
	 * way, as a test that compiles. (The kitchen sink, as the Control Panel offers by default: the admin menu's SAIL
	 * names Control Panel actions, such as StartSAILRecording, that the generated views do not show.)
	 */
	@Test
	void theExternalViewsDownloadIsACompilingSailJavaToo(@TempDir Path classes) throws Exception {
		bean.setSailModuleName("kitchensink");
		bean.setSailTestStrategy(SailTestStrategy.Assert);
		downloadClientSAIL.prepare(bean, webContext);
		Download download = downloadClientSAIL.download(bean, webContext);

		assertEquals("Sail.java", download.getFileName());
		String source = text(download);
		assertTrue(source.contains("SailWorkflow.defaultBrowser(UserAgentType.desktop)"), source);
		assertCompiles(source, classes);
	}

	@Test
	void downloadNeedsAnExecutor() {
		bean.setSailExecutor(null);
		ValidationException e = assertThrows(ValidationException.class, () -> downloadSAIL.prepare(bean, webContext));
		List<String> bindings = new ArrayList<>();
		e.getMessages().get(0).getBindings().forEach(bindings::add);
		assertEquals(List.of(ControlPanel.sailExecutorPropertyName), bindings);
	}

	private static String text(Download download) {
		byte[] bytes = download.getBytes();
		assertNotNull(bytes);
		return new String(bytes, StandardCharsets.UTF_8);
	}

	/**
	 * Compiles the source of the default-package class {@code Sail} in-process against this JVM's class path.
	 *
	 * @throws AssertionError	listing the compiler's errors, with the numbered source, if it does not compile
	 */
	private static void assertCompiles(String source, Path classes) throws IOException {
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		assertNotNull(compiler, "the tests run on a JDK");
		DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
		try (StandardJavaFileManager files = compiler.getStandardFileManager(diagnostics, Locale.ENGLISH, null)) {
			JavaFileObject java = new SimpleJavaFileObject(URI.create("string:///Sail.java"), JavaFileObject.Kind.SOURCE) {
				@Override
				public CharSequence getCharContent(boolean ignoreEncodingErrors) {
					return source;
				}
			};
			List<String> options = List.of("-d", classes.toString(),
											"-cp", System.getProperty("java.class.path") + File.pathSeparator + classes,
											"-proc:none");
			compiler.getTask(null, files, diagnostics, options, null, List.of(java)).call();
		}
		List<String> errors = diagnostics.getDiagnostics().stream()
									.filter(d -> d.getKind() == Diagnostic.Kind.ERROR)
									.map(d -> "line " + d.getLineNumber() + ": " + d.getMessage(Locale.ENGLISH))
									.toList();
		assertAll(() -> assertEquals(List.of(), errors, () -> "the generated Java does not compile:\n" + numbered(source)),
					() -> assertTrue(classes.resolve("Sail.class").toFile().isFile(), "Sail.class is written"));
	}

	private static String numbered(String source) {
		String[] lines = source.split("\n", -1);
		StringBuilder result = new StringBuilder(source.length() + lines.length * 6);
		for (int i = 0; i < lines.length; i++) {
			result.append(i + 1).append(": ").append(lines[i]).append('\n');
		}
		return result.toString();
	}
}

package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

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
import org.mockito.AdditionalAnswers;
import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.Download;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.RuntimeCompiler;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.faces.context.FacesContext;
import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.User.UserExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * Tests the Control Panel's Java SAIL end to end on H2. Above all, it generates the Java test for a small SAIL
 * automation through the Control Panel's own path (the {@code sailExecutor} enum's executor class, then
 * {@link DownloadSAIL#sailSource}) and compiles it with {@code javax.tools} against this test classpath, which holds the
 * SAIL harness ({@code util.sail}). That fails when the generated code stops compiling against the harness: a renamed
 * executor, a template naming a class or method the harness no longer has, or a generator that writes broken Java.
 */
@SuppressWarnings("java:S120") // Module test packages mirror the generated document package.
class DownloadSAILH2Test extends AbstractH2Test {
	private static final String BASE_URL = "http://localhost:8080/skyve/";

	/**
	 * Uses most of what the PrimeFaces generator emits: sign in and out, a comment and messages with quotes, list and
	 * edit navigation, tabs, text, check box and combo entry, Save and a view action, a grid's New, success and failure
	 * tests (with and without a message) and a pause.
	 */
	private static final String AUTOMATION = """
			<automation xmlns="http://www.skyve.org/xml/sail" uxui="tablet" userAgentType="tablet" testStrategy="Assert">
				<before>
					<login user="TestUser" password="pass&quot;word" />
					<testSuccess />
				</before>
				<interaction name="Contacts and users">
					<method>
						<comment>A "quoted" comment with a \\ backslash</comment>
						<navigateList module="admin" document="Contact" />
						<listGridNew module="admin" document="Contact" />
						<dataEnter binding="name" value="SAIL &quot;Contact&quot; {run}" />
						<dataEnter binding="contactType" value="1" />
						<save />
						<testSuccess />
						<testFailure />
						<testFailure message="A &quot;required&quot; value" />
						<pause millis="1" />
						<navigateEdit module="admin" document="ControlPanel" />
						<tabSelect path="Instrumentation" />
						<dataEnter binding="queryTrace" value="true" />
						<tabSelect path="Design" />
						<dataEnter binding="query" value="select bean from {admin.Contact} as bean" />
						<action name="ExecuteQuery" />
					</method>
				</interaction>
				<after>
					<logout />
				</after>
			</automation>
			""";

	private ControlPanelExtension bean;

	@BeforeEach
	void setup() {
		bean = new DataBuilder().fixture(FixtureType.crud).build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		bean.setSailUser(sailUser());
		bean.setSailBaseUrl(BASE_URL);
		bean.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		bean.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		bean.setSail(AUTOMATION);
	}

	@Test
	void theGeneratedJavaCompilesAgainstTheHarness(@TempDir Path classes) throws IOException {
		bean.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		ExecuteSAIL.executeSAIL(bean);
		String methods = bean.getUnescapedResults();
		assertNotNull(methods);
		assertTrue(methods.contains("login("), () -> "the executor generated no Java:\n" + methods);

		String source = DownloadSAIL.sailSource(methods, BASE_URL, UserAgentType.tablet);
		List<String> errors = compile(source, classes);
		assertAll(() -> assertEquals(List.of(), errors, () -> "the generated Java does not compile:\n" + numbered(source)),
					() -> assertTrue(classes.resolve("Sail.class").toFile().isFile(), "Sail.class is written"));
	}

	@Test
	void theSeleneseExecutorGeneratesSeleneseTables() {
		bean.setSailExecutor(SailExecutor.primeFacesInlineSelenese);
		ExecuteSAIL.executeSAIL(bean);
		String results = bean.getUnescapedResults();
		assertNotNull(results);
		assertTrue(results.contains("<table>") && results.contains("<td>type</td>"), () -> "no Selenese tables:\n" + results);
	}

	/**
	 * In a PrimeFaces request (the PrimeFaces Control Panel's), the builders would number the generated components on
	 * from the request page's ids, so the generated ids would match no page: the ids must be the same as outside one.
	 */
	@Test
	void aPrimeFacesRequestsPageDoesNotShiftTheGeneratedIds() throws Exception {
		bean.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		String outsideARequest = ExecuteSAIL.generate(bean);

		FacesView page = new FacesView();
		for (int i = 0; i < 500; i++) {
			page.nextId();
		}
		FacesContext request = primeFacesRequest(page);
		String inARequest;
		try {
			CurrentFacesContext.set(request);
			inARequest = ExecuteSAIL.generate(bean);
			assertSame(request, FacesContext.getCurrentInstance(), "the request's context is put back");
		}
		finally {
			CurrentFacesContext.set(null);
		}
		assertEquals(outsideARequest, inARequest);
	}

	@Test
	void downloadRefusesAnExecutorThatDoesNotMakeJava() {
		bean.setSailExecutor(SailExecutor.primeFacesInlineSelenese);
		DownloadSAIL download = new DownloadSAIL();
		ValidationException e = assertThrows(ValidationException.class, () -> download.prepare(bean, new MockWebContext()));
		Message message = e.getMessages().get(0);
		assertAll(() -> assertEquals(List.of(ControlPanel.sailExecutorPropertyName), toList(message.getBindings())),
					() -> assertTrue(message.getText().contains("only the PrimeFaces Inline Web Driver executor"), message::getText));
	}

	@Test
	void downloadNeedsAnExecutor() {
		bean.setSailExecutor(null);
		DownloadSAIL download = new DownloadSAIL();
		ValidationException e = assertThrows(ValidationException.class, () -> download.prepare(bean, new MockWebContext()));
		assertEquals(List.of(ControlPanel.sailExecutorPropertyName), toList(e.getMessages().get(0).getBindings()));
	}

	@Test
	void downloadWithoutCompileJarsGivesTheGeneratedSource(@TempDir Path compileFolder) throws Exception {
		bean.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		DownloadSAIL action = new DownloadSAIL();
		String compilePath = RuntimeCompiler.COMPILE_PATH;
		Download download;
		try {
			RuntimeCompiler.COMPILE_PATH = compileFolder.toString() + File.separator;
			MockWebContext webContext = new MockWebContext();
			action.prepare(bean, webContext);
			download = action.download(bean, webContext);
		}
		finally {
			RuntimeCompiler.COMPILE_PATH = compilePath;
		}
		byte[] bytes = download.getBytes();
		assertNotNull(bytes);
		String text = new String(bytes, StandardCharsets.UTF_8);
		assertAll(() -> assertEquals("sail.txt", download.getFileName()),
					() -> assertTrue(text.startsWith("The test was not compiled: there are no .jar files in "), text),
					() -> assertTrue(text.contains("startBrowser(SailWorkflow.defaultBrowser(UserAgentType.tablet)"), text),
					() -> assertTrue(text.contains("login(null, \"TestUser\", \"pass\\\"word\");"), text),
					() -> assertNull(bean.getUnescapedResults(), "the results are cleared"));
	}

	/**
	 * Returns a stand-in for a PrimeFaces request's Faces context (not a MockFacesContext, so the builders treat it as
	 * real) whose page bean, {@code skyve}, is the given view; it makes components as the mock context does.
	 */
	@SuppressWarnings("resource") // the delegate is never installed as the current context, so there is nothing to release
	private static FacesContext primeFacesRequest(FacesView page) {
		ELResolver resolver = mock(ELResolver.class);
		ELContext el = mock(ELContext.class);
		when(el.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(el, null, "skyve")).thenReturn(page);
		FacesContext result = mock(FacesContext.class, AdditionalAnswers.delegatesTo(new MockFacesContext()));
		doReturn(el).when(result).getELContext();
		return result;
	}

	/** Sets the thread's current Faces context, which only a FacesContext subclass may do. */
	private abstract static class CurrentFacesContext extends FacesContext {
		static void set(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static List<String> toList(Iterable<String> bindings) {
		List<String> result = new ArrayList<>();
		bindings.forEach(result::add);
		return result;
	}

	/**
	 * Saves and commits a user with the roles the automation's views need, since {@link ExecuteSAIL} generates as the
	 * Control Panel's SAIL user, which it retrieves from the repository (through its own connection).
	 *
	 * @return	the user's proxy
	 */
	private static UserProxyExtension sailUser() {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName("sail.generated." + System.nanoTime());
		user.setInactive(Boolean.FALSE);
		user.getGroups().clear();
		user.getRoles().clear();
		for (String roleName : List.of("admin.BasicUser", "admin.DevOps")) {
			UserRole role = UserRole.newInstance();
			role.setRoleName(roleName);
			user.addRolesElement(role);
		}
		Persistence persistence = CORE.getPersistence();
		user = persistence.save(user);
		persistence.commit(false);
		persistence.begin();
		return persistence.retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, user.getBizId());
	}

	/**
	 * Compiles one source in-process against this JVM's class path.
	 *
	 * @param source	the source of the default-package class {@code Sail}
	 * @param classes	the folder to write the class to
	 * @return	the compiler's errors, each with its line; empty when it compiled
	 */
	private static List<String> compile(String source, Path classes) throws IOException {
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
		return diagnostics.getDiagnostics().stream()
					.filter(d -> d.getKind() == Diagnostic.Kind.ERROR)
					.map(d -> "line " + d.getLineNumber() + ": " + d.getMessage(Locale.ENGLISH))
					.collect(Collectors.toList());
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

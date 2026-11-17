package org.skyve.impl.sail.execution;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListMembershipMove;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomIn;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;

@SuppressWarnings("static-method")
class SailExecutorTest {
	@Test
	void webDriverExecutorRendersTestMethodsCommentsPausesAndHarness() {
		TestWebDriverExecutor executor = new TestWebDriverExecutor();
		Comment comment = new Comment();
		comment.setComment("hello");
		Pause pause = new Pause();
		pause.setMillis(250L);

		executor.open("Create Contact");
		executor.executeComment(comment);
		executor.executePause(pause);
		executor.close();

		String script = executor.toString();
		assertThat(script, containsString("private void testCreateContact()"));
		assertThat(script, containsString("trace(\"hello\");"));
		assertThat(script, containsString("pause(250L);"));
		assertThat(script, containsString("@Test"));
		assertThat(script, containsString("testCreateContact();"));
	}

	/**
	 * A comment goes into a generated string literal, so its quotes, backslashes and line breaks are escaped; no
	 * comment is a null literal.
	 */
	@Test
	void webDriverExecutorCommentsAreJavaLiterals() {
		TestWebDriverExecutor executor = new TestWebDriverExecutor();
		Comment comment = new Comment();
		comment.setComment("say \"hi\" \\ to\nall");

		executor.open("Comments");
		executor.executeComment(comment);
		executor.executeComment(new Comment());
		executor.close();

		String script = executor.toString();
		assertThat(script, containsString("trace(\"say \\\"hi\\\" \\\\ to\\nall\");"));
		assertThat(script, containsString("trace(null);"));
	}

	/**
	 * The generated code cannot follow a new window, so a link into one says so; a link in the same window says nothing.
	 */
	@Test
	void theGeneratorCommentsThatItDoesNotFollowANewWindow() {
		TestWebDriverExecutor webDriver = new TestWebDriverExecutor();
		webDriver.open("Windows");
		NavigateLink newWindow = new NavigateLink();
		newWindow.setHref("?a=e&m=admin&d=Contact");
		newWindow.setWindow(NavigateLink.Window.newWindow);
		webDriver.executeNavigateLink(newWindow);
		webDriver.close();

		assertThat(webDriver.toString(), containsString("trace(\"" + NEW_WINDOW_NOT_FOLLOWED + "\");"));
	}

	private static final String NEW_WINDOW_NOT_FOLLOWED = "<navigateLink window=new/> not supported by this generator: " +
															"the generated code does not follow the new window";

	/**
	 * Interaction names and script values cannot break or inject into the generated Java: a name that ends the method's
	 * comment or holds a Unicode escape, two interactions with one name, a procedure inside an interaction, and an
	 * {@code <execute>} script all give code that compiles.
	 */
	@Test
	void hostileNamesAndScriptsGiveJavaThatCompiles() {
		TestWebDriverExecutor executor = new TestWebDriverExecutor();
		Interaction hostile = new Interaction();
		hostile.setName("x */ static { } /**");
		Procedure before = new Procedure();
		Comment comment = new Comment();
		comment.setComment("first");
		before.getSteps().add(comment);
		hostile.setBefore(before);
		Procedure after = new Procedure();
		after.getSteps().add(comment);
		hostile.setAfter(after);
		Execute execute = new Execute();
		execute.setScript("document.title = \"x\"; // \\u002a/");
		hostile.getSteps().add(execute);
		executor.executeInteraction(hostile);
		Interaction escape = new Interaction();
		escape.setName("y \\u002a\\u002f class Z {} /\\u002a\\u002a");
		executor.executeInteraction(escape);
		Interaction same = new Interaction();
		same.setName("Same");
		executor.executeInteraction(same);
		executor.executeInteraction(same);

		String script = executor.toString();
		assertAll(() -> assertEquals(List.of(), compile(script), script),
					() -> assertTrue(script.contains("execute(\"document.title = \\\"x\\\"; // \\\\u002a/\");"), script),
					() -> assertTrue(script.contains("trace(\"Before x */ static { } /**\");"), script),
					() -> assertTrue(script.contains("trace(\"After x */ static { } /**\");"), script),
					() -> assertTrue(script.contains("private void testSame() {") && script.contains("private void testSame2() {"), script));
	}

	/**
	 * Compiles generated test methods inside a class with the harness methods they call.
	 *
	 * @return	the compiler's errors; empty when the code compiles
	 */
	private static List<String> compile(String methods) throws IOException {
		String source = "class Generated {\n@interface Test {}\nvoid trace(String s) {}\nvoid pause(long millis) {}\n" +
							"void execute(String script) {}\n" + methods + "}\n";
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
		try (StandardJavaFileManager files = compiler.getStandardFileManager(diagnostics, Locale.ENGLISH, null)) {
			JavaFileObject java = new SimpleJavaFileObject(URI.create("string:///Generated.java"), JavaFileObject.Kind.SOURCE) {
				@Override
				public CharSequence getCharContent(boolean ignoreEncodingErrors) {
					return source;
				}
			};
			Path classes = Files.createTempDirectory("sail-generated");
			try {
				compiler.getTask(null, files, diagnostics, List.of("-d", classes.toString(), "-proc:none"), null, List.of(java)).call();
			}
			finally {
				try (Stream<Path> paths = Files.walk(classes)) {
					paths.sorted(Comparator.reverseOrder()).forEach(path -> path.toFile().delete());
				}
			}
		}
		return diagnostics.getDiagnostics().stream()
					.filter(d -> d.getKind() == Diagnostic.Kind.ERROR)
					.map(d -> "line " + d.getLineNumber() + ": " + d.getMessage(Locale.ENGLISH))
					.toList();
	}

	@Test
	void webDriverExecutorCommentsTheStepsItCannotGenerate() {
		TestWebDriverExecutor executor = new TestWebDriverExecutor();

		executor.open("Unsupported");
		executeUnsupportedSteps(executor);
		executor.close();

		String script = executor.toString();
		for (String element : UNSUPPORTED) {
			assertThat(script, containsString("trace(\"<" + element + "/> not supported by this generator\");"));
		}
	}

	private static final String[] UNSUPPORTED = {"upload", "testMessage", "testGridRows", "testWidget", "navigateMenu", "testMenu", "switchMode",
													"listGridFilter", "listGridClearFilter", "listGridSort", "captureValue",
													"captureCell", "captureTitle", "dataGridLink", "dataGridMove", "listMembershipMove",
													"navigateBack", "refresh", "closeWindow", "listGridTool"};

	private static void executeUnsupportedSteps(ScriptExecutor<?> executor) {
		executor.executeUpload(new Upload());
		executor.executeTestMessage(new TestMessage());
		executor.executeTestGridRows(new TestGridRows());
		executor.executeTestWidget(new TestWidget());
		executor.executeNavigateMenu(new NavigateMenu());
		executor.executeTestMenu(new TestMenu());
		executor.executeSwitchMode(new SwitchMode());
		executor.executeListGridFilter(new ListGridFilter());
		executor.executeListGridClearFilter(new ListGridClearFilter());
		executor.executeListGridSort(new ListGridSort());
		executor.executeCaptureValue(new CaptureValue());
		executor.executeCaptureCell(new CaptureCell());
		executor.executeCaptureTitle(new CaptureTitle());
		executor.executeDataGridLink(new DataGridLink());
		executor.executeDataGridMove(new DataGridMove());
		executor.executeListMembershipMove(new ListMembershipMove());
		executor.executeNavigateBack(new NavigateBack());
		executor.executeRefresh(new Refresh());
		NavigateLink link = new NavigateLink();
		link.setHref("?a=e&m=admin&d=Contact");
		link.setWindow(NavigateLink.Window.newWindow);
		executor.executeNavigateLink(link); // opens a window for closeWindow to close
		executor.executeCloseWindow(new CloseWindow());
		executor.executeListGridTool(new ListGridTool());
	}

	private static final class TestContext extends AutomationContext<GenerateListContext, GenerateEditContext> {
		@Override
		public void generate(GenerateListContext listContext) {
			// no-op
		}

		@Override
		public void generate(GenerateEditContext editContext) {
			// no-op
		}
	}

	private static final class TestWebDriverExecutor extends WebDriverExecutor<TestContext> {
		private void open(String heading) {
			startTest(heading);
		}

		private void close() {
			endTest();
		}

		@Override public void executePushListContext(PushListContext push) { /* no-op */ }
		@Override public void executePushEditContext(PushEditContext push) { /* no-op */ }
		@Override public void executePopContext(PopContext pop) { /* no-op */ }
		@Override public void executeClearContext(ClearContext clear) { /* no-op */ }
		@Override public void executeLogin(Login login) { /* no-op */ }
		@Override public void executeLogout(Logout logout) { /* no-op */ }
		@Override public void executeTabSelect(TabSelect tabSelect) { /* no-op */ }
		@Override public void executeTestDataEnter(TestDataEnter testDataEnter) { /* no-op */ }
		@Override public void executeDataEnter(DataEnter dataEnter) { /* no-op */ }
		@Override public void executeOk(Ok ok) { /* no-op */ }
		@Override public void executeSave(Save save) { /* no-op */ }
		@Override public void executeCancel(Cancel cancel) { /* no-op */ }
		@Override public void executeDelete(Delete delete) { /* no-op */ }
		@Override public void executeZoomOut(ZoomOut zoomOut) { /* no-op */ }
		@Override public void executeRemove(Remove remove) { /* no-op */ }
		@Override public void executeAction(Action action) { /* no-op */ }
		@Override public void executeLookupDescriptionAutoComplete(LookupDescriptionAutoComplete complete) { /* no-op */ }
		@Override public void executeLookupDescriptionPick(LookupDescriptionPick pick) { /* no-op */ }
		@Override public void executeLookupDescriptionNew(LookupDescriptionNew nu) { /* no-op */ }
		@Override public void executeLookupDescriptionEdit(LookupDescriptionEdit edit) { /* no-op */ }
		@Override public void executeZoomIn(ZoomIn zoom) { /* no-op */ }
		@Override public void executeDataGridNew(DataGridNew nu) { /* no-op */ }
		@Override public void executeDataGridZoom(DataGridZoom zoom) { /* no-op */ }
		@Override public void executeDataGridEdit(DataGridEdit edit) { /* no-op */ }
		@Override public void executeDataGridRemove(DataGridRemove remove) { /* no-op */ }
		@Override public void executeDataGridSelect(DataGridSelect select) { /* no-op */ }
		@Override public void executeListGridNew(ListGridNew nu) { /* no-op */ }
		@Override public void executeListGridZoom(ListGridZoom zoom) { /* no-op */ }
		@Override public void executeListGridSelect(ListGridSelect select) { /* no-op */ }
		@Override public void executeTestValue(TestValue test) { /* no-op */ }
		@Override public void executeTestSuccess(TestSuccess test) { /* no-op */ }
		@Override public void executeTestFailure(TestFailure test) { /* no-op */ }
	}
}

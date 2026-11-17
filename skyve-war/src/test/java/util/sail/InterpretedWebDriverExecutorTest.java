package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.OutputType;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.impl.sail.execution.RunState;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool.Tool;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.user.User;

/**
 * Unit tests of what the interpreted executors share and need no browser: captured variables, the list grid step
 * checks, the renderer hooks failing until implemented, and the views being built for the signed-in user's roles.
 * The executor is a Mockito spy-like mock that calls the real methods; the renderer hooks are stubbed where a test
 * needs them to succeed.
 */
@SuppressWarnings("unchecked")
class InterpretedWebDriverExecutorTest {
	private InterpretedWebDriverExecutor<SmartClientAutomationContext, SmartClientSelenide> executor;
	private AbstractPersistence persistence;
	private AbstractPersistence previousPersistence;
	private SailViewRolesTest.Fixture fixture;
	/** The users set on the persistence, in order. */
	private List<User> usersSet;

	@BeforeEach
	void setUp() {
		executor = mock(InterpretedWebDriverExecutor.class, withSettings().useConstructor().defaultAnswer(CALLS_REAL_METHODS));
		executor.selenide = mock(SmartClientSelenide.class);
		// settings from no sources: never this JVM's system properties, environment or properties file
		executor.useSettings(SailSettings.resolve(SailSettings.Environment.none(), List.of()));

		fixture = new SailViewRolesTest.Fixture();
		persistence = mock(AbstractPersistence.class);
		usersSet = new ArrayList<>();
		User[] current = {fixture.testUser};
		when(persistence.getUser()).thenAnswer(i -> current[0]);
		doAnswer(i -> {
			current[0] = i.getArgument(0);
			usersSet.add(current[0]);
			return null;
		}).when(persistence).setUser(any());
		previousPersistence = threadLocalPersistence().get();
		threadLocalPersistence().set(persistence);
	}

	@AfterEach
	void restorePersistence() {
		if (previousPersistence == null) {
			threadLocalPersistence().remove();
		}
		else {
			threadLocalPersistence().set(previousPersistence);
		}
	}

	@Test
	void captureValueKeepsTheTrimmedValueForLaterSteps() {
		CaptureValue capture = capture("invoiceNumber", "invoice");
		doReturn("  INV-42 ").when(executor).readWidgetValue(capture);

		executor.executeCaptureValue(capture);

		assertAll(() -> assertEquals("INV-42", executor.getVariable("invoice")),
					() -> assertEquals("Invoice INV-42 of " + executor.getRunToken(), executor.substituteTokens("Invoice {var:invoice} of {run}")));
	}

	@Test
	void captureValueFailsWhenTheWidgetShowsNoValue() {
		CaptureValue blank = capture("invoiceNumber", "invoice");
		doReturn("   ").when(executor).readWidgetValue(blank);
		CaptureValue none = capture("invoiceNumber", "other");
		doReturn(null).when(executor).readWidgetValue(none);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> executor.executeCaptureValue(blank));
		assertAll(() -> assertTrue(e.getMessage().contains("[invoiceNumber] shows no value to keep in {var:invoice}"), e.getMessage()),
					() -> assertThrows(IllegalStateException.class, () -> executor.executeCaptureValue(none)),
					() -> assertEquals(null, executor.getVariable("invoice")));
	}

	@Test
	void usingAnUndefinedVariableFailsTheStep() {
		Login login = new Login();
		login.setUser("{var:client}");
		login.setPassword("x");
		MetaDataException e = assertThrows(MetaDataException.class, () -> executor.executeLogin(login));
		assertTrue(e.getMessage().contains("{var:client} is not defined"), e.getMessage());
	}

	@Test
	void listGridFilterSubstitutesTheValueAndDefaultsTheOperator() {
		bindQuery();
		executor.putVariable("n", "42");
		ListGridFilter filter = filter("name", "Smith {var:n} {run}", null);
		doNothing().when(executor).filterListGrid(any(), anyString(), any(), any());

		executor.executeListGridFilter(filter);

		verify(executor).filterListGrid(filter, "name", "Smith 42 " + executor.getRunToken(), FilterOperator.contains);
	}

	@Test
	void listGridFilterWithoutAValueClearsTheColumn() {
		bindQuery();
		ListGridFilter filter = filter("name", null, null);
		doNothing().when(executor).filterListGrid(any(), anyString(), any(), any());

		executor.executeListGridFilter(filter);

		verify(executor).filterListGrid(eq(filter), eq("name"), isNull(), eq(FilterOperator.contains));
	}

	@Test
	void listGridStepsCheckTheColumnAgainstTheQuery() {
		bindQuery();
		ListGridSort sortName = sort("name");
		ListGridSort sortAmount = sort("amount");
		ListGridFilter filterAmount = filter("amount", "1", FilterOperator.greaterThan);
		ListGridFilter filterMissing = filter("missing", "1", null);
		MetaDataException missing = assertThrows(MetaDataException.class, () -> executor.executeListGridFilter(filterMissing));
		assertAll(() -> assertTrue(missing.getMessage().contains("[missing] is not a column of query kitchensink.qThings: its columns are [name, amount]"),
										missing.getMessage()),
					() -> assertThrows(MetaDataException.class, () -> executor.executeListGridFilter(filterAmount), "amount cannot be filtered"),
					() -> assertThrows(MetaDataException.class, () -> executor.executeListGridSort(sortName), "name cannot be sorted"),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridSort(sortAmount), "checked, then the hook"));
		verify(executor, never()).filterListGrid(any(), anyString(), any(), any());
	}

	@Test
	void listGridSortPassesTheDirection() {
		bindQuery();
		ListGridSort ascending = sort("amount");
		ListGridSort descending = sort("amount");
		descending.setDescending(Boolean.TRUE);
		doNothing().when(executor).sortListGrid(any(), anyString(), org.mockito.ArgumentMatchers.anyBoolean());

		executor.executeListGridSort(ascending);
		executor.executeListGridSort(descending);

		verify(executor).sortListGrid(ascending, "amount", false);
		verify(executor).sortListGrid(descending, "amount", true);
	}

	@Test
	void listGridStepsOnAModelAreNotChecked() {
		ListGridFilter filter = filter("anything", "1", null);
		filter.setModuleName("kitchensink");
		filter.setDocumentName("Thing");
		filter.setModelName("model");
		doNothing().when(executor).filterListGrid(any(), anyString(), any(), any());

		executor.executeListGridFilter(filter);

		verify(executor).filterListGrid(filter, "anything", "1", FilterOperator.contains);
	}

	@Test
	void theRendererHooksFailUntilImplemented() {
		bindQuery();
		DataEnter dataEnter = new DataEnter();
		dataEnter.setBinding("status");
		NavigateLink link = new NavigateLink();
		CaptureCell cell = new CaptureCell();
		CaptureTitle title = new CaptureTitle();
		DataGridLink gridLink = new DataGridLink();
		ListGridFilter listGridFilter = filter("name", "x", null);
		ListGridClearFilter clearFilter = new ListGridClearFilter();
		ListGridSort listGridSort = sort("amount");
		CaptureValue captureValue = capture("b", "v");
		LookupDescriptionPick pick = new LookupDescriptionPick();
		LookupDescriptionAutoComplete autoComplete = new LookupDescriptionAutoComplete();
		assertAll(() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridFilter(listGridFilter)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridClearFilter(clearFilter)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridSort(listGridSort)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeCaptureValue(captureValue)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.enterOption(dataEnter, "x", Match.code)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.pickLookupDescriptionRowContaining(pick, "x")),
					() -> assertThrows(UnsupportedOperationException.class,
										() -> executor.autoCompleteLookupDescriptionAndPick(autoComplete, "x", "y")),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.clickLinkContaining(link, "x")),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.readGridCell(cell)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.readViewTitle(title)),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.clickDataGridLink(gridLink, "x", true)));
	}

	@Test
	void navigateLinkByTextSubstitutesTheTextAndCallsTheHook() {
		executor.putVariable("id", "42");
		NavigateLink link = new NavigateLink();
		link.setText("Request {var:id} of {run}");
		doNothing().when(executor).clickLinkContaining(any(), anyString());

		executor.executeNavigateLink(link);

		verify(executor).clickLinkContaining(link, "Request 42 of " + executor.getRunToken());
		verify(executor.selenide, never()).get(anyString());
	}

	@Test
	void theWindowAndToolHooksFailUntilImplemented() {
		ListGridTool tool = tool(Tool.refresh);
		NavigateLink link = new NavigateLink();
		assertAll(() -> assertThrows(UnsupportedOperationException.class, () -> executor.clickLinkIntoNewWindow(link, "x")),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridTool(tool)));
	}

	/**
	 * A link into a new window, by text or by href, with its tokens substituted; windows nest, and each close returns to
	 * the opener of the window it closes. In the first window there is nothing to close.
	 */
	@Test
	void navigateLinkIntoANewWindowAndCloseWindowReturnToEachOpener() {
		executor.putVariable("n", "7");
		NavigateLink byText = newWindowLink("Print {var:n}");
		NavigateLink byHref = new NavigateLink();
		byHref.setHref("/?a=e&m=admin&d=Contact&i={var:n}");
		byHref.setWindow(NavigateLink.Window.newWindow);
		doReturn("first").when(executor).clickLinkIntoNewWindow(any(), anyString());
		when(executor.selenide.getInNewWindow(anyString())).thenReturn("second");

		executor.executeNavigateLink(byText);
		executor.executeNavigateLink(byHref);
		executor.executeCloseWindow(new CloseWindow());
		executor.executeCloseWindow(new CloseWindow());

		verify(executor).clickLinkIntoNewWindow(byText, "Print 7");
		verify(executor.selenide).getInNewWindow("?a=e&m=admin&d=Contact&i=7");
		InOrder order = inOrder(executor.selenide);
		order.verify(executor.selenide).closeWindowAndReturnTo("second");
		order.verify(executor.selenide).closeWindowAndReturnTo("first");
		CloseWindow close = new CloseWindow();
		MetaDataException e = assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(close));
		assertAll(() -> assertTrue(e.getMessage().contains("has no window to close"), e.getMessage()),
					() -> verify(executor, never()).clickLinkContaining(any(), anyString()),
					() -> verify(executor.selenide, never()).get(anyString()));
	}

	@Test
	void aLinkThatOpensNoNewWindowLeavesTheStepsInThisWindow() {
		doThrow(new IllegalStateException("opened no new window")).when(executor).clickLinkIntoNewWindow(any(), anyString());

		NavigateLink link = newWindowLink("Print");
		assertThrows(IllegalStateException.class, () -> executor.executeNavigateLink(link));

		CloseWindow close = new CloseWindow();
		assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(close));
		verify(executor.selenide, never()).closeWindowAndReturnTo(anyString());
	}

	/**
	 * A window left open is closed when its interaction does not complete, so the next interaction starts in the first
	 * window, and when the automation ends.
	 */
	@Test
	void aWindowLeftOpenIsClosedWhenItsInteractionFailsAndWhenTheAutomationEnds() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"fails\"><method><navigateLink text=\"A\" window=\"new\"/><navigateBack/></method></interaction>" +
							"<interaction name=\"left open\"><method><navigateLink text=\"B\" window=\"new\"/></method></interaction>" +
						"</automation>";
		doReturn("first").when(executor).clickLinkIntoNewWindow(any(), anyString());
		doThrow(new IllegalStateException("There is no earlier page")).when(executor.selenide).back();

		Automation automation = XMLMetaData.unmarshalSAILString(script);
		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeAutomation(automation));
		CloseWindow close = new CloseWindow();

		assertAll(() -> assertTrue(e.getMessage().contains("with 1 failure(s)"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("There is no earlier page"), e.getMessage()),
					() -> verify(executor.selenide, times(2)).closeWindowAndReturnTo("first"),
					() -> assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(close)));
	}

	/**
	 * A failure's screenshot and page source are named by the suite member and the run, so that members and runs that
	 * share the failure directory keep their own files.
	 */
	@Test
	void failureFilesAreNamedByTheMemberAndTheRun(@TempDir Path directory) throws IOException {
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(TakesScreenshot.class));
		when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[] {1});
		when(driver.getPageSource()).thenReturn("<html/>");
		executor.selenide.driver = driver;
		executor.useSettings(SailSettings.resolve(SailSettings.Environment.none(), List.of(SailSettings.code().failureDir(directory).layer())));
		doThrow(new IllegalStateException("There is no earlier page")).when(executor.selenide).back();
		Automation automation = XMLMetaData.unmarshalSAILString("<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\" xmlns=\"http://www.skyve.org/xml/sail\">" +
																	"<interaction name=\"Go back\"><method><navigateBack/></method></interaction></automation>");
		SailMedia.label("pf/01-session.xml");
		try {
			assertThrows(AssertionError.class, () -> executor.executeAutomation(automation));
		}
		finally {
			SailMedia.label(null);
		}
		List<String> files;
		try (Stream<Path> listed = Files.list(directory)) {
			files = listed.map(f -> f.getFileName().toString()).sorted().toList();
		}
		String stem = "pf-01-session.xml-" + executor.getRunToken() + "-01-Go_back-step01";
		assertEquals(List.of(stem + ".html", stem + ".png"), files);
	}

	@Test
	void backRefreshAndListGridToolDriveTheBrowserAndTheHook() {
		ListGridTool tool = tool(Tool.clearSelection);
		doNothing().when(executor).pressListGridTool(any(), any());

		executor.executeNavigateBack(new NavigateBack());
		executor.executeRefresh(new Refresh());
		executor.executeListGridTool(tool);

		InOrder order = inOrder(executor.selenide, executor);
		order.verify(executor.selenide).back();
		order.verify(executor.selenide).reload();
		order.verify(executor).pressListGridTool(tool, Tool.clearSelection);
	}

	private static NavigateLink newWindowLink(String text) {
		NavigateLink result = new NavigateLink();
		result.setText(text);
		result.setWindow(NavigateLink.Window.newWindow);
		return result;
	}

	private static ListGridTool tool(Tool which) {
		ListGridTool result = new ListGridTool();
		result.setModuleName("admin");
		result.setQueryName("qContacts");
		result.setTool(which);
		return result;
	}

	@Test
	void eachLoginBuildsTheViewsWithTheSignedInUsersRoles() {
		executor.useViewRoles(new SailViewRoles().register("client", "todo.ToDoClient").register("jane{run}", "Clients"));

		executor.executeLogin(login("client"));
		UserImpl client = (UserImpl) usersSet.get(0);
		executor.executeLogin(login("admin"));
		UserImpl admin = (UserImpl) usersSet.get(1);
		executor.executeLogin(login("jane{run}"));
		UserImpl jane = (UserImpl) usersSet.get(2);

		assertAll(() -> assertTrue(client.isInRole("todo", "ToDoClient")),
					() -> assertFalse(client.isInRole("todo", "ToDoManager")),
					() -> assertFalse(client.isInRole("admin", "BasicUser")),
					() -> assertTrue(admin.isInRole("todo", "ToDoManager"), "unregistered: every role"),
					() -> assertTrue(admin.isInRole("admin", "BasicUser"), "unregistered: every role"),
					() -> assertTrue(jane.isInRole("admin", "BasicUser"), "the customer role"),
					() -> assertFalse(jane.isInRole("todo", "ToDoManager")),
					() -> assertEquals("tester", client.getName(), "copied from the test JVM's user"));
	}

	@Test
	void theUserFromBeforeTheFirstLoginIsRestoredWhenTheAutomationEnds() {
		executor.useViewRoles(new SailViewRoles().register("client", "todo.ToDoClient"));
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"i\"><method><login user=\"client\" password=\"x\"/><login user=\"admin\" password=\"x\"/></method></interaction>" +
						"</automation>";

		executor.executeAutomation(XMLMetaData.unmarshalSAILString(script));

		assertAll(() -> assertEquals(3, usersSet.size(), usersSet.toString()),
					() -> assertTrue(((UserImpl) usersSet.get(0)).isInRole("todo", "ToDoClient")),
					() -> assertTrue(((UserImpl) usersSet.get(1)).isInRole("todo", "ToDoManager")),
					() -> assertSame(fixture.testUser, usersSet.get(2)),
					() -> assertSame(fixture.testUser, persistence.getUser()));
	}

	/**
	 * A user that is not registered gets the roles the server says it has (sail.recording on); a registration wins,
	 * and the server is not asked; a role the test's customer does not have is left out.
	 */
	@Test
	void anUnregisteredUserGetsTheRolesTheServerSays() {
		executor.useViewRoles(new SailViewRoles().register("client", "todo.ToDoClient"));
		when(executor.selenide.signedInRoleNames()).thenReturn(List.of("todo.ToDoManager", "nope.Nothing"));

		executor.executeLogin(login("manager"));
		UserImpl manager = (UserImpl) usersSet.get(usersSet.size() - 1);
		assertAll(() -> assertTrue(manager.isInRole("todo", "ToDoManager")),
					() -> assertFalse(manager.isInRole("admin", "BasicUser"), "only the server's roles"));

		executor.executeLogin(login("client"));
		UserImpl client = (UserImpl) usersSet.get(usersSet.size() - 1);
		assertAll(() -> assertTrue(client.isInRole("todo", "ToDoClient")),
					() -> assertFalse(client.isInRole("todo", "ToDoManager"), "the registration wins"));
		verify(executor.selenide, times(1)).signedInRoleNames();
	}

	@Test
	void withoutAnAnswerFromTheServerAnUnregisteredUserGetsEveryRole() {
		executor.useViewRoles(new SailViewRoles());
		when(executor.selenide.signedInRoleNames()).thenReturn(null);

		executor.executeLogin(login("admin"));

		assertAll(() -> assertEquals(1, usersSet.size()),
					() -> assertTrue(((UserImpl) usersSet.get(0)).isInRole("todo", "ToDoManager")));
	}

	@Test
	void withoutViewRolesALoginKeepsThePersistenceUser() {
		executor.executeLogin(login("client"));

		verify(persistence, never()).setUser(any());
	}

	@Test
	void anUnknownRegisteredRoleFailsTheLogin() {
		executor.useViewRoles(new SailViewRoles().register("client", "todo.Nope"));

		Login clientLogin = login("client");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> executor.executeLogin(clientLogin));
		assertTrue(e.getMessage().contains("todo.Nope"), e.getMessage());
	}

	private static Login login(String user) {
		Login result = new Login();
		result.setUser(user);
		result.setPassword("x");
		return result;
	}

	@Test
	void navigateEditSubstitutesTheBizIdsTokens() {
		doNothing().when(executor).executePushEditContext(any());
		executor.putVariable("id", "01a1-b2");
		NavigateEdit edit = new NavigateEdit();
		edit.setModuleName("kitchensink");
		edit.setDocumentName("LookupDescription");
		edit.setBizId("{var:id}");

		executor.executeNavigateEdit(edit);

		verify(executor.selenide).get("?a=e&m=kitchensink&d=LookupDescription&i=01a1-b2");
	}

	@Test
	void navigateLinkSubstitutesTheHrefsTokens() {
		executor.putVariable("id", "01a1-b2");
		NavigateLink relative = new NavigateLink();
		relative.setHref("?a=e&m=kitchensink&d=LookupDescription&i={var:id}&r={run}");
		NavigateLink absolute = new NavigateLink();
		absolute.setHref("https://example.com/{var:id}");

		executor.executeNavigateLink(relative);
		executor.executeNavigateLink(absolute);

		assertAll(() -> verify(executor.selenide).get("?a=e&m=kitchensink&d=LookupDescription&i=01a1-b2&r=" + executor.getRunToken()),
					() -> verify(executor.selenide).openUrl("https://example.com/01a1-b2"));
	}

	@Test
	void navigateLinkWithAnUndefinedVariableFailsBeforeNavigating() {
		NavigateLink link = new NavigateLink();
		link.setHref("?a=e&m=kitchensink&d=LookupDescription&i={var:missing}");

		assertThrows(MetaDataException.class, () -> executor.executeNavigateLink(link));
		verify(executor.selenide, never()).get(anyString());
	}

	@Test
	void aCapturePatternKeepsItsFirstGroupOrItsWholeMatch() {
		CaptureValue group = capture("reference", "number");
		group.setPattern("REF-(\\d+)");
		doReturn(" Request REF-42 - printer ").when(executor).readWidgetValue(group);
		CaptureValue whole = capture("reference", "whole");
		whole.setPattern("REF-\\d+");
		doReturn("Request REF-42").when(executor).readWidgetValue(whole);

		executor.executeCaptureValue(group);
		executor.executeCaptureValue(whole);

		assertAll(() -> assertEquals("42", executor.getVariable("number")),
					() -> assertEquals("REF-42", executor.getVariable("whole")));
	}

	@Test
	void aCapturePatternThatDoesNotMatchFailsWithTheTextRead() {
		CaptureValue capture = capture("reference", "number");
		capture.setPattern("REF-(\\d+)");
		doReturn("no reference").when(executor).readWidgetValue(capture);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> executor.executeCaptureValue(capture));
		assertAll(() -> assertTrue(e.getMessage().contains("'no reference'"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("REF-(\\d+)"), e.getMessage()),
					() -> assertEquals(null, executor.getVariable("number")));
	}

	@Test
	void captureCellOfADataGridKeepsTheCellsText() {
		CaptureCell cell = new CaptureCell();
		cell.setBinding("rows");
		cell.setRowContaining("SAIL {run}");
		cell.setColumn("outcome");
		cell.setVariable("outcome");
		cell.setPattern("Run From (\\w+) ran");
		doReturn("Run From Row ran").when(executor).readGridCell(cell);

		executor.executeCaptureCell(cell);

		assertEquals("Row", executor.getVariable("outcome"));
	}

	@Test
	void captureCellOfAListChecksTheColumnAgainstTheQuery() {
		bindQuery();
		CaptureCell cell = new CaptureCell();
		cell.setModuleName("kitchensink");
		cell.setQueryName("qThings");
		cell.setRow(Integer.valueOf(0));
		cell.setColumn("missing");
		cell.setVariable("v");
		CaptureCell blank = new CaptureCell();
		blank.setModuleName("kitchensink");
		blank.setQueryName("qThings");
		blank.setRow(Integer.valueOf(0));
		blank.setColumn("name");
		blank.setVariable("v");
		doReturn(" ").when(executor).readGridCell(blank);

		MetaDataException e = assertThrows(MetaDataException.class, () -> executor.executeCaptureCell(cell));
		IllegalStateException empty = assertThrows(IllegalStateException.class, () -> executor.executeCaptureCell(blank));
		assertAll(() -> assertTrue(e.getMessage().contains("[missing] is not a column of query kitchensink.qThings"), e.getMessage()),
					() -> assertTrue(empty.getMessage().contains("column [name] of row 0 shows no value to keep in {var:v}"), empty.getMessage()),
					() -> verify(executor, never()).readGridCell(cell));
	}

	@Test
	void captureTitleKeepsTheTitle() {
		CaptureTitle title = new CaptureTitle();
		title.setVariable("request");
		title.setPattern("Request (\\d+)");
		doReturn("Request 1234 - printer").when(executor).readViewTitle(title);

		executor.executeCaptureTitle(title);

		assertEquals("1234", executor.getVariable("request"));
	}

	@Test
	void dataGridLinkSubstitutesTheTextAndChecksSuccessUnlessDeclinedOrTold() {
		executor.putVariable("verb", "Revoke");
		DataGridLink checked = link(null);
		DataGridLink unchecked = link(Boolean.FALSE);
		DataGridLink declined = link(null);
		declined.setConfirm(Boolean.FALSE);
		doReturn(Boolean.FALSE).when(executor).clickDataGridLink(any(), anyString(), anyBoolean());
		doReturn(Boolean.TRUE).when(executor).clickDataGridLink(eq(declined), anyString(), anyBoolean());
		when(Boolean.valueOf(executor.selenide.verifySuccess())).thenReturn(Boolean.TRUE);

		executor.executeDataGridLink(checked);
		executor.executeDataGridLink(unchecked);
		executor.executeDataGridLink(declined);

		assertAll(() -> verify(executor).clickDataGridLink(checked, "Revoke Access", true),
					() -> verify(executor.selenide, times(1)).verifySuccess());
	}

	@Test
	void loginSubstitutesTheCustomerAndACommentItsTokens() {
		executor.putVariable("customer", "demo");
		Login login = login("admin");
		login.setCustomer("{var:customer}");
		Comment comment = new Comment();
		comment.setComment("Run {run} for {var:customer}");

		executor.executeLogin(login);
		executor.executeComment(comment);

		assertAll(() -> verify(executor.selenide).login("demo", "admin", "x"),
					() -> verify(executor.selenide).trace("Run " + executor.getRunToken() + " for demo"));
	}

	@Test
	void testFailureAfterARefusedUploadChecksTheRefusal() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"refused\"><method>" +
								"<upload action=\"Up\" file=\"f.txt\" testSuccess=\"false\"/><testFailure message=\"already exists\"/>" +
								"<upload action=\"Up\" file=\"f.txt\" testSuccess=\"false\"/><testFailure message=\"something else\"/>" +
								"<upload action=\"Up\" file=\"f.txt\" testSuccess=\"false\"/><testSuccess/>" +
							"</method></interaction>" +
						"</automation>";
		doAnswer(i -> {
			executor.uploadRefused("The file f.txt already exists");
			return null;
		}).when(executor).executeUpload(any(Upload.class));
		when(executor.selenide.describeMessages()).thenReturn("no messages");

		Automation automation = XMLMetaData.unmarshalSAILString(script);
		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeAutomation(automation));
		assertAll(() -> assertTrue(e.getMessage().contains("with 2 failure(s)"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("containing 'something else' but found: (the upload page said: The file f.txt already exists)"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("expected the upload to succeed but it was refused: The file f.txt already exists"), e.getMessage()));
	}

	@Test
	void aSharedRunCarriesTheTokenAndVariablesIntoTheNextAutomation() {
		RunState run = new RunState();
		run.putVariable("number", "7");
		executor.shareRun(run);
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"i\"><method><login user=\"jane{run}\" password=\"{var:number}\"/></method></interaction>" +
						"</automation>";

		executor.executeAutomation(XMLMetaData.unmarshalSAILString(script));

		verify(executor.selenide).login(null, "jane" + run.getRunToken(), "7");
	}

	private static DataGridLink link(Boolean testSuccess) {
		DataGridLink result = new DataGridLink();
		result.setBinding("rows");
		result.setRow(Integer.valueOf(0));
		result.setText("{var:verb} Access");
		result.setTestSuccess(testSuccess);
		return result;
	}

	private static CaptureValue capture(String binding, String variable) {
		CaptureValue result = new CaptureValue();
		result.setBinding(binding);
		result.setVariable(variable);
		return result;
	}

	private static ListGridFilter filter(String column, String value, FilterOperator operator) {
		ListGridFilter result = new ListGridFilter();
		result.setModuleName("kitchensink");
		result.setQueryName("qThings");
		result.setColumn(column);
		result.setValue(value);
		result.setOperator(operator);
		return result;
	}

	private static ListGridSort sort(String column) {
		ListGridSort result = new ListGridSort();
		result.setModuleName("kitchensink");
		result.setQueryName("qThings");
		result.setColumn(column);
		return result;
	}

	/**
	 * Gives the test user's customer a module {@code kitchensink} with a query {@code qThings} whose columns are
	 * {@code name} (filterable, not sortable) and {@code amount} (sortable, not filterable).
	 */
	private void bindQuery() {
		Module module = mock(Module.class);
		when(fixture.customer.getModule("kitchensink")).thenReturn(module);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("qThings");
		MetaDataQueryProjectedColumn name = mock(MetaDataQueryProjectedColumn.class);
		when(name.getBinding()).thenReturn("name");
		when(Boolean.valueOf(name.isFilterable())).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(name.isSortable())).thenReturn(Boolean.FALSE);
		MetaDataQueryProjectedColumn amount = mock(MetaDataQueryProjectedColumn.class);
		when(amount.getBinding()).thenReturn("amount");
		when(Boolean.valueOf(amount.isFilterable())).thenReturn(Boolean.FALSE);
		when(Boolean.valueOf(amount.isSortable())).thenReturn(Boolean.TRUE);
		List<MetaDataQueryColumn> columns = List.of(name, amount);
		when(query.getColumns()).thenReturn(columns);
		when(module.getMetaDataQuery("qThings")).thenReturn(query);
	}

	private static ThreadLocal<AbstractPersistence> threadLocalPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			return (ThreadLocal<AbstractPersistence>) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void anOrderedMembershipsMembersMatchOnlyInOrder() {
		assertAll(() -> assertTrue(InterpretedWebDriverExecutor.membersMatch(List.of("a", "c", "b"), List.of("a", "c", "b"), true)),
					() -> assertFalse(InterpretedWebDriverExecutor.membersMatch(List.of("a", "b", "c"), List.of("a", "c", "b"), true)),
					() -> assertFalse(InterpretedWebDriverExecutor.membersMatch(List.of("a", "c"), List.of("a", "c", "b"), true)));
	}

	@Test
	@SuppressWarnings("static-method")
	void anUnorderedMembershipsMembersMatchInAnyOrder() {
		assertAll(() -> assertTrue(InterpretedWebDriverExecutor.membersMatch(List.of("a", "b", "c"), List.of("c", "a", "b"), false)),
					() -> assertFalse(InterpretedWebDriverExecutor.membersMatch(List.of("a", "b"), List.of("a", "b", "c"), false)),
					() -> assertFalse(InterpretedWebDriverExecutor.membersMatch(List.of("a", "b", "d"), List.of("a", "b", "c"), false)));
	}

	@Test
	@SuppressWarnings("static-method")
	void membershipItemsSplitsTheValueInOrder() {
		assertAll(() -> assertEquals(List.of("Admin - B", "Admin - A"), InterpretedWebDriverExecutor.membershipItems(" Admin - B | Admin - A ")),
					() -> assertEquals(List.of(), InterpretedWebDriverExecutor.membershipItems(null)),
					() -> assertEquals(List.of("x"), InterpretedWebDriverExecutor.membershipItems("x||")));
	}

	/**
	 * Opening, switching or closing windows blurs pages, and a blur can arrive in the step after the switch, so neither
	 * a step that changes windows nor the step after one says the window lost the OS focus; a step in one window still
	 * does.
	 */
	@Test
	void theHarnesssOwnWindowChangesAreNotAFocusLoss() {
		long[] blurs = {0L};
		// every probe sees one more blur than the last, in the same page: a loss whenever before and after are compared
		when(executor.selenide.focusProbe()).thenAnswer(i -> new Selenide.FocusProbe("p1", blurs[0]++, false, true));
		doReturn("first").when(executor).clickLinkIntoNewWindow(any(), eq("Print"));
		doThrow(new IllegalStateException("opened no new window")).when(executor).clickLinkIntoNewWindow(any(), eq("Nothing"));
		doThrow(new IllegalStateException("There is no earlier page")).when(executor.selenide).back();
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<interaction name=\"after a new window\"><method><navigateLink text=\"Print\" window=\"new\"/><navigateBack/></method></interaction>" +
							"<interaction name=\"opening a window\"><method><navigateLink text=\"Nothing\" window=\"new\"/></method></interaction>" +
							"<interaction name=\"one window\"><method><comment>c</comment><navigateBack/></method></interaction>" +
						"</automation>";

		Automation automation = XMLMetaData.unmarshalSAILString(script);
		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeAutomation(automation));

		String[] failures = e.getMessage().split("\n  - ");
		assertAll(() -> assertEquals(4, failures.length, e.getMessage()),
					() -> assertTrue(failures[1].startsWith("Interaction \"after a new window\" step 2"), failures[1]),
					() -> assertFalse(failures[1].contains("lost the OS focus"), failures[1]),
					() -> assertTrue(failures[2].startsWith("Interaction \"opening a window\" step 1"), failures[2]),
					() -> assertFalse(failures[2].contains("lost the OS focus"), failures[2]),
					() -> assertTrue(failures[3].startsWith("Interaction \"one window\" step 2"), failures[3]),
					() -> assertTrue(failures[3].contains("the browser window lost the OS focus during this step (1 time)"), failures[3]));
	}

	@Test
	@SuppressWarnings("static-method")
	void aFocusLossDuringTheStepIsNoted() {
		Selenide.FocusProbe before = new Selenide.FocusProbe("p1", 2, false, true);
		Selenide.FocusProbe after = new Selenide.FocusProbe("p1", 4, false, true);
		assertEquals("the browser window lost the OS focus during this step (2 times); hint",
						InterpretedWebDriverExecutor.focusNote(before, after, "hint"));
		assertEquals("the browser window lost the OS focus during this step (1 time); hint",
						InterpretedWebDriverExecutor.focusNote(before, new Selenide.FocusProbe("p1", 3, false, true), "hint"));
	}

	@Test
	@SuppressWarnings("static-method")
	void noFocusLossSaysNothing() {
		Selenide.FocusProbe probe = new Selenide.FocusProbe("p1", 2, false, true);
		assertAll(() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(probe, probe, "hint")),
					() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(probe, null, "hint")),
					() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(null, null, "hint")));
	}

	@Test
	@SuppressWarnings("static-method")
	void aNewPageCountsAsUnknownNotAsNoLoss() {
		Selenide.FocusProbe before = new Selenide.FocusProbe("p1", 5, false, true);
		// a page load during the step: a new token, or a counter installed only by the failure's probe
		assertAll(() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(before, new Selenide.FocusProbe("p2", 7, false, true), "hint")),
					() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(before, new Selenide.FocusProbe("p1", 9, true, true), "hint")),
					() -> assertEquals(null, InterpretedWebDriverExecutor.focusNote(null, new Selenide.FocusProbe("p1", 9, false, true), "hint")),
					() -> assertEquals("the browser window did not have the OS focus when this step failed; hint",
										InterpretedWebDriverExecutor.focusNote(before, new Selenide.FocusProbe("p2", 0, true, false), "hint")));
	}
}

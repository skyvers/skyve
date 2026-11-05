package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
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
		assertAll(() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridFilter(filter("name", "x", null))),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridClearFilter(new ListGridClearFilter())),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeListGridSort(sort("amount"))),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.executeCaptureValue(capture("b", "v"))),
					() -> assertThrows(UnsupportedOperationException.class, () -> executor.enterOption(dataEnter, "x", Match.code)),
					() -> assertThrows(UnsupportedOperationException.class,
										() -> executor.pickLookupDescriptionRowContaining(new LookupDescriptionPick(), "x")),
					() -> assertThrows(UnsupportedOperationException.class,
										() -> executor.autoCompleteLookupDescriptionAndPick(new LookupDescriptionAutoComplete(), "x", "y")));
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

	@Test
	void withoutViewRolesALoginKeepsThePersistenceUser() {
		executor.executeLogin(login("client"));

		verify(persistence, never()).setUser(any());
	}

	@Test
	void anUnknownRegisteredRoleFailsTheLogin() {
		executor.useViewRoles(new SailViewRoles().register("client", "todo.Nope"));

		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> executor.executeLogin(login("client")));
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
}

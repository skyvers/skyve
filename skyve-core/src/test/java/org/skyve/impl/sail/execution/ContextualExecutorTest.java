package org.skyve.impl.sail.execution;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;

@SuppressWarnings("static-method")
class ContextualExecutorTest {
	@AfterEach
	void clearPersistence() {
		setPersistenceThreadLocal(null);
	}

	@Test
	void navigationWithoutContextThrowsMetaDataException() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();

		assertThrows(MetaDataException.class, () -> executor.executeNavigateEdit(new NavigateEdit()));
		assertThrows(MetaDataException.class, () -> executor.executeNavigateLink(new NavigateLink()));
		assertThrows(MetaDataException.class, () -> executor.executeNavigateTree(new NavigateTree()));
	}

	@Test
	void newListContextForUndefinedQueryThatIsNotADocumentThrowsMetaDataException() {
		Customer customer = bindCustomer();
		Module module = bindModule(customer);
		when(module.getMetaDataQuery("missing")).thenReturn(null);
		when(module.getDocumentDefaultQuery(customer, "missing")).thenThrow(new MetaDataException("no document ref"));
		PushListContext push = new PushListContext();
		push.setModuleName("sales");
		push.setQueryName("missing");
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();

		assertThrows(MetaDataException.class, () -> executor.newContext(push, newContext()));
	}

	/**
	 * An undefined query name is the name of a document whose default query drives the list,
	 * just as the list grid itself resolves it.
	 */
	@Test
	void newListContextForUndefinedQueryUsesDefaultQueryOfDocumentNamedByQuery() {
		Customer customer = bindCustomer();
		Module module = bindModule(customer);
		when(module.getMetaDataQuery("Contact")).thenReturn(null);
		Module owningModule = mock(Module.class);
		when(owningModule.getName()).thenReturn("admin");
		MetaDataQueryDefinition defaultQuery = mock(MetaDataQueryDefinition.class);
		when(defaultQuery.getOwningModule()).thenReturn(owningModule);
		when(defaultQuery.getDocumentName()).thenReturn("Contact");
		when(module.getDocumentDefaultQuery(customer, "Contact")).thenReturn(defaultQuery);
		PushListContext push = new PushListContext();
		push.setModuleName("sales");
		push.setQueryName("Contact");
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();

		AutomationContext<GenerateListContext, GenerateEditContext> result = executor.newContext(push, newContext());

		assertEquals("admin", result.getModuleName());
		assertEquals("Contact", result.getDocumentName());
		assertEquals(ViewType.list, result.getViewType());
		// list grid steps that name no list default to the list the context shows
		assertSame(push, result.getList());
	}

	/**
	 * Navigating to a list resolves an undefined query name the same way as pushing a list context.
	 */
	@Test
	void navigateListForUndefinedQueryUsesDefaultQueryOfDocumentNamedByQuery() {
		Customer customer = bindCustomer();
		Module module = bindModule(customer);
		when(module.getMetaDataQuery("Contact")).thenReturn(null);
		MetaDataQueryDefinition defaultQuery = mock(MetaDataQueryDefinition.class);
		when(defaultQuery.getDocumentName()).thenReturn("Contact");
		when(module.getDocumentDefaultQuery(customer, "Contact")).thenReturn(defaultQuery);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("Contact");
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		NavigateList list = new NavigateList();
		list.setModuleName("sales");
		list.setQueryName("Contact");
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		AutomationContext<GenerateListContext, GenerateEditContext> context = newContext();
		executor.push(context);

		executor.executeNavigateList(list);

		assertEquals("admin", context.getModuleName());
		assertEquals("Contact", context.getDocumentName());
		assertEquals(ViewType.list, context.getViewType());
	}

	@Test
	void navigateListForUndefinedQueryThatIsNotADocumentThrowsMetaDataException() {
		Customer customer = bindCustomer();
		Module module = bindModule(customer);
		when(module.getMetaDataQuery("missing")).thenReturn(null);
		when(module.getDocumentDefaultQuery(customer, "missing")).thenThrow(new MetaDataException("no document ref"));
		NavigateList list = new NavigateList();
		list.setModuleName("sales");
		list.setQueryName("missing");
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.push(newContext());

		assertThrows(MetaDataException.class, () -> executor.executeNavigateList(list));
	}

	@Test
	void newListContextForModelWithoutDrivingDocumentThrowsIllegalStateException() {
		Customer customer = bindCustomer();
		Module module = bindModule(customer);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		when(module.getDocument(customer, "Customer")).thenReturn(document);
		when(document.<Bean> getListModel(customer, "recent", false)).thenReturn(model);
		when(model.getDrivingDocument()).thenReturn(null);
		PushListContext push = new PushListContext();
		push.setModuleName("sales");
		push.setDocumentName("Customer");
		push.setModelName("recent");
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();

		assertThrows(IllegalStateException.class, () -> executor.newContext(push, newContext()));
	}

	@Test
	void substituteTokensReplacesEveryOccurrenceWithTheSameToken() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();

		String token = executor.getRunToken();
		assertTrue(token.matches("[0-9a-z]{8,}"), token);
		assertEquals("jane" + token + "@" + token + ".com", executor.substituteTokens("jane{run}@{run}.com"));
		assertEquals(token, executor.getRunToken());
	}

	@Test
	void substituteTokensReturnsValueWithoutTokenUnchanged() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		String value = "jane {RUN} run";

		assertSame(value, executor.substituteTokens(value));
	}

	@Test
	void executeAutomationStartsARunToken() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);

		executor.executeAutomation(automation);

		String token = executor.getRunToken();
		assertEquals("u" + token, executor.substituteTokens("u" + ContextualExecutor.RUN_TOKEN));
	}

	@Test
	void substituteTokensReplacesVariablesAndTheRunToken() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.putVariable("invoice", "INV-42");
		executor.putVariable("n", "7");
		String token = executor.getRunToken();

		assertAll(() -> assertEquals("INV-42", executor.substituteTokens("{var:invoice}")),
					() -> assertEquals("a INV-42 b 7 c " + token, executor.substituteTokens("a {var:invoice} b {var:n} c {run}")),
					() -> assertEquals("{x} {} {var} {RUN}", executor.substituteTokens("{x} {} {var} {RUN}")),
					() -> assertEquals("{" + token + "}", executor.substituteTokens("{{run}}")),
					() -> assertEquals("7" + token + "7", executor.substituteTokens("{var:n}{run}{var:n}")));
	}

	@Test
	void substituteTokensDoesNotSubstituteAVariableValue() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.putVariable("literal", "{run} and {var:literal}");

		assertEquals("<{run} and {var:literal}>", executor.substituteTokens("<{var:literal}>"));
	}

	@Test
	void substituteTokensFailsForAnUndefinedVariable() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		MetaDataException none = assertThrows(MetaDataException.class, () -> executor.substituteTokens("x{var:invoice}"));
		executor.putVariable("order", "1");
		MetaDataException some = assertThrows(MetaDataException.class, () -> executor.substituteTokens("x{var:invoice}"));

		assertAll(() -> assertTrue(none.getMessage().contains("{var:invoice} is not defined"), none.getMessage()),
					() -> assertTrue(none.getMessage().contains("no variables are defined"), none.getMessage()),
					() -> assertTrue(some.getMessage().contains("[order]"), some.getMessage()));
	}

	@Test
	void substituteTokensFailsForAnUnterminatedVariable() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.putVariable("invoice", "INV-42");

		assertThrows(MetaDataException.class, () -> executor.substituteTokens("x{var:invoice"));
	}

	@Test
	void putVariableReplacesAnEarlierValue() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.putVariable("invoice", "INV-1");
		executor.putVariable("invoice", "INV-2");

		assertAll(() -> assertEquals("INV-2", executor.getVariable("invoice")),
					() -> assertNull(executor.getVariable("order")));
	}

	@Test
	void executeAutomationStartsWithNoVariables() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.putVariable("invoice", "INV-1");
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);

		executor.executeAutomation(automation);

		assertNull(executor.getVariable("invoice"));
	}

	@Test
	void aSharedRunKeepsItsTokenAndVariablesAcrossAutomationsAndExecutors() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> first = newExecutor();
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> second = newExecutor();
		RunState run = new RunState();
		first.shareRun(run);
		second.shareRun(run);
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);

		first.executeAutomation(automation);
		first.putVariable("invoice", "INV-1");
		second.executeAutomation(automation);
		second.putVariable("order", "7");
		first.executeAutomation(automation);

		assertAll(() -> assertSame(run, first.getRun()),
					() -> assertEquals(run.getRunToken(), first.getRunToken()),
					() -> assertEquals(run.getRunToken(), second.getRunToken()),
					() -> assertEquals("INV-1 7 " + run.getRunToken(), second.substituteTokens("{var:invoice} {var:order} {run}")),
					() -> assertEquals("7", first.getVariable("order")),
					() -> assertEquals("INV-1", run.getVariable("invoice")));
	}

	@Test
	void anUnsharedRunIsReplacedByEachAutomation() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		RunState before = executor.getRun();
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);

		executor.executeAutomation(automation);

		assertAll(() -> assertNotSame(before, executor.getRun()),
					() -> assertEquals(executor.getRun().getRunToken(), executor.getRunToken()));
	}

	/**
	 * A link into a new window sets the opener's contexts aside untouched, and the new window starts with none; windows
	 * nest, and each close drops the closed window's contexts and puts back its opener's.
	 */
	@Test
	void aNewWindowSetsTheOpenersContextsAsideUntilItIsClosed() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		AutomationContext<GenerateListContext, GenerateEditContext> opener = newContext();
		opener.setDocumentName("Contact");
		opener.setViewType(ViewType.edit);
		executor.push(opener);

		executor.executeNavigateLink(newWindowLink());
		assertNull(executor.peek(), "a new window starts with no contexts");
		AutomationContext<GenerateListContext, GenerateEditContext> inNewWindow = newContext();
		executor.push(inNewWindow);
		executor.executeNavigateLink(newWindowLink());
		AutomationContext<GenerateListContext, GenerateEditContext> inNestedWindow = newContext();
		executor.push(inNestedWindow);

		executor.executeCloseWindow(new CloseWindow());
		AutomationContext<GenerateListContext, GenerateEditContext> afterFirstClose = executor.peek();
		executor.executeCloseWindow(new CloseWindow());

		assertAll(() -> assertSame(inNewWindow, afterFirstClose),
					() -> assertSame(opener, executor.peek()),
					() -> assertEquals("Contact", opener.getDocumentName(), "the opener's context is untouched"),
					() -> assertEquals(ViewType.edit, opener.getViewType()),
					() -> assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(new CloseWindow()), "both windows are closed"));
	}

	@Test
	void closeWindowInTheFirstWindowThrowsMetaDataException() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.push(newContext());

		MetaDataException e = assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(new CloseWindow()));
		assertAll(() -> assertTrue(e.getMessage().contains("has no window to close"), e.getMessage()),
					() -> assertTrue(executor.peek() != null, "the contexts are untouched"));
	}

	@Test
	void forgetOpenWindowsAndANewAutomationLeaveOneWindow() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.executeNavigateLink(newWindowLink());
		executor.forgetOpenWindows();
		MetaDataException afterForget = assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(new CloseWindow()));
		executor.executeNavigateLink(newWindowLink());
		executor.executeAutomation(new Automation());

		assertAll(() -> assertTrue(afterForget.getMessage().contains("has no window to close"), afterForget.getMessage()),
					() -> assertThrows(MetaDataException.class, () -> executor.executeCloseWindow(new CloseWindow()), "a new automation starts in one window"));
	}

	/**
	 * Back leads to a page the executor cannot know, so the top context loses its view as after a link; a refresh keeps
	 * it, as the URL is the same; neither needs a context.
	 */
	@Test
	void backForgetsTheTopContextsViewAndRefreshKeepsIt() {
		ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> executor = newExecutor();
		executor.executeNavigateBack(new NavigateBack());
		executor.executeRefresh(new Refresh());
		AutomationContext<GenerateListContext, GenerateEditContext> context = newContext();
		context.setModuleName("admin");
		context.setDocumentName("Contact");
		context.setViewType(ViewType.edit);
		context.setList(new PushListContext());
		executor.push(context);

		executor.executeRefresh(new Refresh());
		String afterRefresh = context.getDocumentName();
		executor.executeNavigateBack(new NavigateBack());

		assertAll(() -> assertEquals("Contact", afterRefresh),
					() -> assertEquals("admin", context.getModuleName()),
					() -> assertNull(context.getDocumentName()),
					() -> assertNull(context.getViewType()),
					() -> assertNull(context.getList()));
	}

	private static NavigateLink newWindowLink() {
		NavigateLink result = new NavigateLink();
		result.setText("Print");
		result.setWindow(NavigateLink.Window.newWindow);
		return result;
	}

	private static ContextualExecutor<AutomationContext<GenerateListContext, GenerateEditContext>> newExecutor() {
		return mock(ContextualExecutor.class, withSettings().useConstructor().defaultAnswer(CALLS_REAL_METHODS));
	}

	private static AutomationContext<GenerateListContext, GenerateEditContext> newContext() {
		return new AutomationContext<>() {
			@Override public void generate(GenerateListContext lc) { /* no-op */ }
			@Override public void generate(GenerateEditContext ec) { /* no-op */ }
		};
	}

	/**
	 * Binds a mocked persistence to the thread whose user has the returned mocked customer.
	 */
	private static Customer bindCustomer() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		setPersistenceThreadLocal(persistence);
		return customer;
	}

	private static Module bindModule(Customer customer) {
		Module module = mock(Module.class);
		when(customer.getModule("sales")).thenReturn(module);
		return module;
	}

	@SuppressWarnings("unchecked")
	private static void setPersistenceThreadLocal(AbstractPersistence persistence) {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			if (persistence == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(persistence);
			}
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}
}

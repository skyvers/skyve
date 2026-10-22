package org.skyve.impl.sail.execution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
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

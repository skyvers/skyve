package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.list.ListModel;

@SuppressWarnings("static-method")
class FacesActionsCoverageTest {
	@AfterEach
	void tearDown() {
		clearThreadPersistence();
	}

	@Test
	void getBeansActionCallbackExecutesModelPath() {
		FacesView facesView = mock(FacesView.class);
		GetBeansAction action = new GetBeansAction(facesView,
										"admin",
										"Contact",
										"q",
										"m",
										null,
										null,
										true);

		RuntimeException thrown = assertThrows(RuntimeException.class, action::callback);
		assertTrue((thrown instanceof IllegalArgumentException) || (thrown.getCause() instanceof IllegalArgumentException));
	}

	@Test
	void populateActionInitialisesListModelWhenDocumentAndQueryPresent() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getListModel(customer, "q", true)).thenReturn(listModel);
		when(listModel.getLocalisedDescription()).thenReturn("List Title");

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("List Title");
		verify(facesView).setModelName("q");
		verify(facesView).setQueryNameParameter(null);
	}

	@Test
	void populateActionResolvesMetadataQueryWhenDocumentMissing() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("q")).thenReturn(query);
		when(query.getDocumentName()).thenReturn("Contact");
		when(query.getLocalisedDescription()).thenReturn("Query Title");

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn(null);
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setBizDocumentParameter("Contact");
		verify(facesView).setTitle("Query Title");
	}

	@Test
	void populateActionUsesDocumentAndQueryBranchWhenBothParametersPresent() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);

		RuntimeException thrown = assertThrows(RuntimeException.class, action::callback);
		assertTrue((thrown instanceof IllegalArgumentException) || (thrown.getCause() instanceof IllegalArgumentException));
	}

	@Test
	void populateActionUsesMetadataQueryBranchWhenDocumentMissing() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn(null);
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);

		RuntimeException thrown = assertThrows(RuntimeException.class, action::callback);
		assertTrue((thrown instanceof IllegalArgumentException) || (thrown.getCause() instanceof IllegalArgumentException));
	}

	@Test
	void setTitleActionLogsAndReturnsWhenBeanIsNull() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(null);

		SetTitleAction action = new SetTitleAction(facesView);

		assertDoesNotThrow(action::callback);
		verify(facesView, never()).setTitle(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void setTitleActionExecutesBeanBranchWhenBeanPresent() {
		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(facesView.getViewBinding()).thenReturn(null);

		SetTitleAction action = new SetTitleAction(facesView);

		RuntimeException thrown = assertThrows(RuntimeException.class, action::callback);
		assertTrue((thrown instanceof IllegalArgumentException) || (thrown.getCause() instanceof IllegalArgumentException));
	}

	@Test
	void setTitleActionSetsTitleWhenBeanAndViewAreResolved() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);

		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));

		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView(org.mockito.ArgumentMatchers.eq("desktop"), org.mockito.ArgumentMatchers.eq(customer), org.mockito.ArgumentMatchers.anyString())).thenReturn(view);
		when(view.getLocalisedTitle()).thenReturn("Fixed title");

		SetTitleAction action = new SetTitleAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("Fixed title");
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException("Unable to clear thread-local persistence", e);
		}
	}
}

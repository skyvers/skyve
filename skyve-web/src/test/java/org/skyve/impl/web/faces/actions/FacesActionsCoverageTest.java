package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.MetaDataException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.web.UserAgentType;

import jakarta.faces.FacesException;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.component.html.HtmlInputText;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "boxing", "java:S1130", "java:S1192", "java:S8692"}) // repeated fixture values aid readability
class FacesActionsCoverageTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() {
		FacesContextBridge.setCurrent(null);
		WebContainer.clear();
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
	void getBeansActionCallbackReturnsLoadedRowsWhenModelResolves() throws Exception {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		Bean bean = mock(Bean.class);
		Page page = new Page();
		page.setRows(List.of(bean));
		page.setTotalRows(1L);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccess(any(), org.mockito.ArgumentMatchers.eq("desktop"))).thenReturn(Boolean.TRUE);
		when(user.canReadDocument(document)).thenReturn(Boolean.TRUE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getListModel(customer, "model", true)).thenReturn(listModel);
		when(listModel.getDrivingDocument()).thenReturn(document);
		when(listModel.fetch()).thenReturn(page);
		when(listModel.getColumns()).thenReturn(Collections.emptyList());

		FacesView facesView = mock(FacesView.class);
		when(facesView.getCurrentBean()).thenReturn(null);
		when(facesView.getWebContext()).thenReturn(null);

		GetBeansAction action = new GetBeansAction(facesView,
									"admin",
									"Contact",
									null,
									"model",
									null,
									null,
									false);

		List<BeanMapAdapter> result = action.callback();

		assertEquals(1, result.size());
		assertEquals(bean, result.get(0).getBean());
	}

	@Test
	void populateActionInitialisesListModelWhenDocumentAndQueryPresent() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document drivingDocument = mock(Document.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getListModel(customer, "q", true)).thenReturn(listModel);
		when(listModel.getLocalisedDescription()).thenReturn("List Title");
		when(listModel.getDrivingDocument()).thenReturn(drivingDocument);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("List Title");
		verify(facesView).setTitleIcon(drivingDocument, null);
		verify(facesView).setModelName("q");
		verify(facesView).setQueryNameParameter(null);
	}

	@Test
	void populateActionResolvesMetadataQueryWhenDocumentMissing() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module documentModule = mock(Module.class);
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("q")).thenReturn(query);
		when(query.getDocumentName()).thenReturn("Contact");
		when(query.getLocalisedDescription()).thenReturn("Query Title");
		when(query.getDocumentModule(customer)).thenReturn(documentModule);
		when(documentModule.getDocument(customer, "Contact")).thenReturn(document);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn(null);
		when(facesView.getQueryNameParameter()).thenReturn("q");

		PopulateAction action = new PopulateAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setBizDocumentParameter("Contact");
		verify(facesView).setTitle("Query Title");
		verify(facesView).setTitleIcon(document, null);
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
		when(facesView.getViewBinding()).thenReturn(null);

		SetTitleAction action = new SetTitleAction(facesView);

		RuntimeException thrown = assertThrows(RuntimeException.class, action::callback);
		assertTrue((thrown instanceof IllegalArgumentException) || (thrown.getCause() instanceof IllegalArgumentException));
	}

	@Test
	void setTitleActionSetsTitleWhenBeanAndViewAreResolved() throws Exception {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		view.setTitle("<img src=x onerror=alert(1)> {bizKey}");
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);

		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn(null);

		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView(org.mockito.ArgumentMatchers.eq("desktop"), org.mockito.ArgumentMatchers.eq(customer), org.mockito.ArgumentMatchers.anyString())).thenReturn(view);
		when(bean.getBizKey()).thenReturn("Fixed title");

		SetTitleAction action = new SetTitleAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("<img src=x onerror=alert(1)> Fixed title");
		verify(facesView).setTitleIcon(document, view);
	}

	@Test
	void setTitleActionAllowsTrustedViewTitleWhenEscapingFalse() throws Exception {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		view.setTitle("<b>{bizKey}</b>");
		view.setEscapeTitle(Boolean.FALSE);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);

		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn(null);

		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizKey()).thenReturn("Trusted title");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView(org.mockito.ArgumentMatchers.eq("desktop"), org.mockito.ArgumentMatchers.eq(customer), org.mockito.ArgumentMatchers.anyString())).thenReturn(view);

		SetTitleAction action = new SetTitleAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("<b>Trusted title</b>");
	}

	@Test
	void setTitleActionUsesEditViewWhenBeanIsCreated() throws Exception {
		installRequestSelectionContext();
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

		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.isCreated()).thenReturn(Boolean.TRUE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView("desktop", customer, "edit")).thenReturn(view);
		when(view.getLocalisedTitle()).thenReturn("Edit title");

		SetTitleAction action = new SetTitleAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView).setTitle("Edit title");
	}

	@Test
	void getContentUrlActionReturnsBlankGifWhenBindingIsMissingAndImageRequested() throws Exception {
		HashMap<String, Object> values = new HashMap<>();
		values.put("contentId", null);
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		GetContentURLAction action = new GetContentURLAction(bean, "contentId", true);

		assertEquals("images/blank.gif", action.callback());
	}

	@Test
	void getContentUrlActionReturnsVoidJavascriptWhenBindingIsMissingAndNotImage() throws Exception {
		HashMap<String, Object> values = new HashMap<>();
		values.put("contentId", null);
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		GetContentURLAction action = new GetContentURLAction(bean, "contentId", false);

		assertEquals("javascript:void(0)", action.callback());
	}

	@Test
	void getContentUrlActionBuildsContentUrlWhenBindingResolves() throws Exception {
		HashMap<String, Object> values = new HashMap<>();
		values.put("contentId", "cid-1");
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		GetContentURLAction action = new GetContentURLAction(bean, "contentId", false);

		String result = action.callback();
		assertTrue(result.startsWith("content?"));
		assertTrue(result.contains("cid-1"));
		assertTrue(result.contains("admin.Contact"));
		assertTrue(result.contains("contentId"));
	}

	@Test
	void getContentFileNameActionReturnsEmptySentinelWhenNoContentIsBound() throws Exception {
		HashMap<String, Object> values = new HashMap<>();
		values.put("contentId", null);
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		GetContentFileNameAction action = new GetContentFileNameAction(bean, "contentId");

		assertEquals("&lt;Empty&gt;", action.callback());
	}

	@Test
	void getContentFileNameActionThrowsWhenContentLookupInfrastructureIsUnavailable() {
		HashMap<String, Object> values = new HashMap<>();
		values.put("contentId", "cid-1");
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		GetContentFileNameAction action = new GetContentFileNameAction(bean, "contentId");

		assertThrows(Throwable.class, action::callback);
	}

	@Test
	void executeActionActionThrowsSecurityExceptionWhenUserCannotExecuteResource() {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		Bean bean = mock(Bean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(user.canExecuteAction(document, "myAction")).thenReturn(Boolean.FALSE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.isCreated()).thenReturn(Boolean.FALSE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
			when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("myAction")).thenReturn(actionMeta);
		when(actionMeta.getResourceName()).thenReturn("myAction");
		when(actionMeta.getClientValidation()).thenReturn(Boolean.FALSE);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		ExecuteActionAction action = new ExecuteActionAction(facesView, "myAction", null, null);

		assertThrows(IllegalArgumentException.class, action::callback);
	}

	@Test
	void executeActionActionThrowsClassCastWhenExecutionAllowed() {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		Bean bean = mock(Bean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(user.canExecuteAction(document, "myAction")).thenReturn(Boolean.TRUE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.isCreated()).thenReturn(Boolean.FALSE);
		when(bean.isNotPersisted()).thenReturn(Boolean.FALSE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
			when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("myAction")).thenReturn(actionMeta);
		when(actionMeta.getResourceName()).thenReturn("myAction");
		when(actionMeta.getClientValidation()).thenReturn(Boolean.FALSE);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getWebContext()).thenReturn(null);

		ExecuteActionAction action = new ExecuteActionAction(facesView, "myAction", null, null);
		assertThrows(ClassCastException.class, action::callback);
	}

	@Test
	void deleteActionThrowsSecurityExceptionWhenDeletePermissionIsDenied() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		PersistentBean bean = mock(PersistentBean.class);
		PersistentBean persisted = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);
		when(user.canDeleteDocument(document)).thenReturn(Boolean.FALSE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizId()).thenReturn("b1");
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(persistence.retrieve(document, "b1")).thenReturn(persisted);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		DeleteAction action = new DeleteAction(facesView);
		assertThrows(IllegalArgumentException.class, action::callback);
	}

	@Test
	void deleteActionThrowsValidationExceptionWhenBeanWasDeletedByAnotherUser() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		PersistentBean bean = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);
		when(user.canDeleteDocument(document)).thenReturn(Boolean.TRUE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizId()).thenReturn("b1");
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(persistence.retrieve(document, "b1")).thenReturn(null);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		DeleteAction action = new DeleteAction(facesView);
		assertThrows(ValidationException.class, action::callback);
	}

	@Test
	void saveActionThrowsClassCastWhenCustomerIsNotInternalType() {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		PersistentBean bean = mock(PersistentBean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.isCreated()).thenReturn(Boolean.FALSE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
			when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("Save")).thenReturn(actionMeta);
		when(actionMeta.getClientValidation()).thenReturn(Boolean.FALSE);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		SaveAction action = new SaveAction(facesView, false);
		assertThrows(ClassCastException.class, action::callback);
	}

	@Test
	void deleteActionTakesANonPersistentBeanThroughPreExecuteWithoutChecksOrDeleting() throws Exception {
		assertDeletedWithoutPersistence(mock(Bean.class));
	}

	@Test
	void deleteActionTakesAnUnpersistedBeanThroughPreExecuteWithoutChecksOrDeleting() throws Exception {
		assertDeletedWithoutPersistence(mock(PersistentBean.class));
	}

	/**
	 * Asserts that, as in Save, a bean that is not persisted is only taken through pre-execute and post-render.
	 */
	private void assertDeletedWithoutPersistence(Bean bean) throws Exception {
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		DocumentImpl document = saveDocument(persistence);
		Customer customer = user.getCustomer();
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		doReturn(bizlet).when(document).getBizlet(customer);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(currentFacesView.getWebContext()).thenReturn(webContext);
		when(bizlet.preExecute(ImplicitActionName.Delete, bean, null, webContext)).thenReturn(bean);

		new DeleteAction(currentFacesView).callback();

		verify(bizlet).preExecute(ImplicitActionName.Delete, bean, null, webContext);
		verify(user, never()).canDeleteDocument(any());
		verify(persistence, never()).delete(any(Document.class), any(PersistentBean.class));
		verify(currentFacesView).setPostRender(bizlet, bean);
	}

	@Test
	void saveActionTakesANonPersistentBeanThroughPreExecuteWithoutSecurityChecksOrSaving() throws Exception {
		Bean bean = mock(Bean.class);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		FacesView facesView = currentFacesView;
		DocumentImpl document = saveDocument(persistence);
		Customer customer = user.getCustomer();
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		doReturn(bizlet).when(document).getBizlet(customer);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);
		when(bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext)).thenReturn(bean);

		new SaveAction(facesView, false).callback();

		verify(bizlet).preExecute(ImplicitActionName.Save, bean, null, webContext);
		verify(user, never()).canCreateDocument(any());
		verify(user, never()).canUpdateDocument(any());
		verify(persistence, never()).save(any(Document.class), any(PersistentBean.class));
		verify(facesView).setBean(currentBean);
		verify(facesView).setPostRender(bizlet, currentBean);
	}

	@Test
	void saveActionSecurityChecksAndSavesAPersistentBean() throws Exception {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.isNotPersisted()).thenReturn(Boolean.TRUE);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		Document document = saveDocument(persistence);
		when(user.canCreateDocument(document)).thenReturn(Boolean.TRUE);

		new SaveAction(currentFacesView, false).callback();

		verify(user).canCreateDocument(document);
		verify(persistence).save(document, bean);
	}

	@Test
	void saveActionSecurityChecksAndSavesAPersistedBean() throws Exception {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		Document document = saveDocument(persistence);
		when(user.canUpdateDocument(document)).thenReturn(Boolean.TRUE);

		new SaveAction(currentFacesView, false).callback();

		verify(user, never()).canCreateDocument(any());
		verify(user).canUpdateDocument(document);
		verify(persistence).save(document, bean);
	}

	@Test
	void saveActionRefusesToCreateAPersistentBeanWithoutCreatePermission() {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.isNotPersisted()).thenReturn(Boolean.TRUE);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		Document document = saveDocument(persistence);

		assertRefusedWithASecurityException(new SaveAction(currentFacesView, false)::callback);
		verify(user).canCreateDocument(document);
		verify(user, never()).canUpdateDocument(any());
		verify(persistence, never()).save(any(Document.class), any(PersistentBean.class));
	}

	@Test
	void saveActionRefusesToUpdateAPersistedBeanWithoutUpdatePermission() {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		User user = persistence.getUser();
		Document document = saveDocument(persistence);

		assertRefusedWithASecurityException(new SaveAction(currentFacesView, false)::callback);
		verify(user, never()).canCreateDocument(any());
		verify(user).canUpdateDocument(document);
		verify(persistence, never()).save(any(Document.class), any(PersistentBean.class));
	}

	/**
	 * Asserts that an action is refused with a security exception.
	 * Constructing one logs it through a new persistence, which these mocks cannot provide,
	 * so the refusal surfaces as the IllegalArgumentException thrown while the SecurityException is constructed.
	 */
	private static void assertRefusedWithASecurityException(Executable action) {
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, action);
		assertTrue(Arrays.stream(e.getStackTrace()).anyMatch(frame -> SecurityException.class.getName().equals(frame.getClassName())),
					"Not thrown while constructing a SecurityException");
	}

	/**
	 * The document that {@link #mockSaveContext(Bean, Boolean)} resolves the bean to.
	 */
	private static DocumentImpl saveDocument(AbstractPersistence persistence) {
		Customer customer = persistence.getUser().getCustomer();
		return (DocumentImpl) customer.getModule("kitchensink").getDocument(customer, "ConditionsFixture");
	}

	private FacesView currentFacesView;
	private Bean currentBean;

	/**
	 * Mocks what a save needs to get past client validation and the internal customer:
	 * a user, an internal customer and document with no Bizlet, and a faces view of the bean.
	 */
	private AbstractPersistence mockSaveContext(Bean bean, Boolean clientValidation) {
		installRequestSelectionContext();
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentImpl document = mock(DocumentImpl.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);
		when(bean.getBizModule()).thenReturn("kitchensink");
		when(bean.getBizDocument()).thenReturn("ConditionsFixture");
		when(customer.getModule("kitchensink")).thenReturn(module);
		when(module.getDocument(customer, "ConditionsFixture")).thenReturn(document);
		when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("Save")).thenReturn(actionMeta);
		when(actionMeta.getClientValidation()).thenReturn(clientValidation);

		currentBean = bean;
		currentFacesView = mock(FacesView.class);
		when(currentFacesView.getBean()).thenReturn(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(currentFacesView.getWebContext()).thenReturn(webContext);
		return persistence;
	}

	@Test
	void saveActionOkBranchResolvesOkActionBeforeInternalCustomerCast() {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		PersistentBean bean = mock(PersistentBean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.isCreated()).thenReturn(Boolean.FALSE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
			when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("OK")).thenReturn(actionMeta);
		when(actionMeta.getClientValidation()).thenReturn(Boolean.FALSE);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		SaveAction action = new SaveAction(facesView, true);
		assertThrows(ClassCastException.class, action::callback);
		verify(view).getAction("OK");
	}

	@Test
	@SuppressWarnings("java:S2143") // OptimisticLock's public contract still uses java.util.Date
	void deleteActionThrowsOptimisticLockWhenRetrievedLockDiffers() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		PersistentBean bean = mock(PersistentBean.class);
		PersistentBean persisted = mock(PersistentBean.class);
		OptimisticLock lock1 = mock(OptimisticLock.class);
		OptimisticLock lock2 = mock(OptimisticLock.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);
		when(user.canDeleteDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canReadBean(org.mockito.ArgumentMatchers.anyString(),
								org.mockito.ArgumentMatchers.anyString(),
								org.mockito.ArgumentMatchers.anyString(),
								org.mockito.ArgumentMatchers.anyString(),
								org.mockito.ArgumentMatchers.anyString(),
								org.mockito.ArgumentMatchers.anyString())).thenReturn(Boolean.TRUE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizId()).thenReturn("b1");
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		when(bean.getBizLock()).thenReturn(lock1);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(persistence.retrieve(document, "b1")).thenReturn(persisted);
		when(persisted.getBizLock()).thenReturn(lock2);
		when(lock2.getTimestamp()).thenReturn(new Date());
		when(persisted.getBizId()).thenReturn("b1");
		when(persisted.getBizModule()).thenReturn("admin");
		when(persisted.getBizDocument()).thenReturn("Contact");
		when(persisted.getBizCustomer()).thenReturn("demo");
		when(persisted.getBizDataGroupId()).thenReturn("dg");
		when(persisted.getBizUserId()).thenReturn("u1");

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		DeleteAction action = new DeleteAction(facesView);
		assertThrows(OptimisticLockException.class, action::callback);
	}

	@Test
	void removeActionThrowsWhenZoomBindingResolvesNoTargetBean() {
		HashMap<String, Object> values = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn("missingBinding");

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(MetaDataException.class, action::callback);
	}

	@Test
	void addActionReturnsNullWhenRequiredValidationFailsForNonInlineAdd() throws Exception {
		UIViewRoot root = mock(UIViewRoot.class);
		doReturn(Boolean.TRUE).when(root).isRendered();

		HtmlInputText requiredInput = mock(HtmlInputText.class);
		doReturn(Boolean.TRUE).when(requiredInput).isRendered();
		when(requiredInput.getRequiredMessage()).thenReturn("Required");
		when(requiredInput.getValue()).thenReturn(null);
		when(requiredInput.getStyle()).thenReturn("");
		when(requiredInput.getFacetsAndChildren()).thenAnswer(i -> java.util.Arrays.asList().iterator());

		when(root.getFacetsAndChildren()).thenAnswer(i -> java.util.Arrays.asList(requiredInput).iterator());

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", false);
		assertDoesNotThrow(action::callback);
	}

	@Test
	void zoomInActionReturnsWhenRequiredValidationFails() {
		installFailingRequiredValidationContext();

		FacesView facesView = mock(FacesView.class);

		ZoomInAction action = new ZoomInAction(facesView, "items", "e1");
		assertDoesNotThrow(action::callback);

		verify(facesView, never()).getCurrentBean();
	}

	@Test
	void zoomOutActionReturnsWhenRequiredValidationFails() {
		installFailingRequiredValidationContext();

		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());

		ZoomOutAction action = new ZoomOutAction(facesView);
		assertDoesNotThrow(action::callback);

		verify(facesView, never()).getBean();
	}

	/**
	 * Gives the thread a persistence with a user, so an add action gets past {@code CORE.getUser()} to the missing bean.
	 */
	private static void setThreadPersistenceWithAUser() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(mock(User.class));
		persistence.setForThread();
	}

	@Test
	void addActionInlineBypassesValidationAndThrowsWhenBeanMissing() {
		setThreadPersistenceWithAUser();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", true);
		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void addActionNonInlineContinuesAfterValidationPassesThenThrowsWhenBeanMissing() {
		setThreadPersistenceWithAUser();
		UIViewRoot root = mock(UIViewRoot.class);
		doReturn(Boolean.TRUE).when(root).isRendered();
		when(root.getFacetsAndChildren()).thenAnswer(i -> java.util.Arrays.asList().iterator());

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", false);
		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void addActionWithExistingViewBindingStillThrowsWhenBeanMissing() {
		setThreadPersistenceWithAUser();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn("parent.child");
		when(facesView.getBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", true);
		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void editActionSetupViewForZoomInUpdatesViewWhenBindingResolves() throws Exception {
		HashMap<String, Object> childValues = new HashMap<>();
		DynamicBean child = new DynamicBean("admin", "Child", childValues);
		HashMap<String, Object> parentValues = new HashMap<>();
		parentValues.put("child", child);
		DynamicBean parent = new DynamicBean("admin", "Parent", parentValues);
		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("parent", parent);
		DynamicBean root = new DynamicBean("admin", "Root", rootValues);
		ArrayDeque<String> zoomInBindings = new ArrayDeque<>();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(zoomInBindings);

		invokeSetupViewForZoomIn(new EditAction(facesView), root, "parent,child");

		assertEquals(List.of("parent", "child"), List.copyOf(zoomInBindings));
		verify(facesView).setViewBinding("parent.child");
		verify(facesView).setBizModuleParameter("admin");
		verify(facesView).setBizDocumentParameter("Child");
	}

	@Test
	void editActionSetupViewForZoomInLeavesViewAloneWhenBindingCannotResolve() throws Exception {
		DynamicBean root = new DynamicBean("admin", "Root", new HashMap<>());
		FacesView facesView = mock(FacesView.class);

		invokeSetupViewForZoomIn(new EditAction(facesView), root, "missing");

		verify(facesView, never()).getZoomInBindings();
		verify(facesView, never()).setViewBinding(org.mockito.ArgumentMatchers.anyString());
		verify(facesView, never()).setBizModuleParameter(org.mockito.ArgumentMatchers.anyString());
		verify(facesView, never()).setBizDocumentParameter(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void addActionResolvesUserAndDocumentThenThrowsWhenCurrentBeanMissing() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Bean bean = mock(Bean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");

		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getCurrentBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", true);
		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void removeActionInlineRemovesElementAndReturns() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		Bean element = mock(Bean.class);
		when(element.getBizId()).thenReturn("e1");
		when(element.getBizModule()).thenReturn("admin");
		when(element.getBizDocument()).thenReturn("Contact");

		HashMap<String, Object> values = new HashMap<>();
		values.put("items", new java.util.ArrayList<>(List.of(element)));
		DynamicBean rootBean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(rootBean);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getWebContext()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, "items", "e1", null);
		assertThrows(MetaDataException.class, action::callback);
	}

	@Test
	void removeActionZoomedThrowsClassCastAfterTargetResolution() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		bindPersistenceForUser(user);
		when(user.getCustomer()).thenReturn(customer);

		Bean child = mock(Bean.class);
		HashMap<String, Object> values = new HashMap<>();
		values.put("child", child);
		DynamicBean rootBean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(rootBean);
		when(facesView.getViewBinding()).thenReturn("child");
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(ClassCastException.class, action::callback);
	}

	@Test
	void removeActionWithNullCollectionNameAndElementIdUsesZoomedPath() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(null);
		when(facesView.getViewBinding()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void removeActionZoomedReturnsWhenCustomerInterceptorVetoes() throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);

		Bean child = mock(Bean.class);
		when(child.getBizModule()).thenReturn("admin");
		when(child.getBizDocument()).thenReturn("Contact");

		HashMap<String, Object> values = new HashMap<>();
		values.put("child", child);
		DynamicBean rootBean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(rootBean);
		when(facesView.getViewBinding()).thenReturn("child");
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(NullPointerException.class, action::callback);
	}

	@Test
	void setTitleActionReturnsWhenViewBindingYieldsNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		HashMap<String, Object> values = new HashMap<>();
		values.put("child", null);
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getViewBinding()).thenReturn("child");

		SetTitleAction action = new SetTitleAction(facesView);

		assertDoesNotThrow(action::callback);
		verify(facesView, never()).setTitle(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void populateActionThrowsWhenQueryNameMissing() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBizModuleParameter()).thenReturn("admin");
		when(facesView.getBizDocumentParameter()).thenReturn("Contact");
		when(facesView.getQueryNameParameter()).thenReturn(null);

		PopulateAction action = new PopulateAction(facesView);

		assertThrows(FacesException.class, action::callback);
		verify(facesView, never()).setTitle(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void executeActionActionThrowsWhenSelectedRowIsNotInTheGrid() {
		bindPersistenceForUser(mock(User.class));
		HashMap<String, Object> values = new HashMap<>();
		values.put("items", new java.util.ArrayList<>());
		DynamicBean bean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		ExecuteActionAction action = new ExecuteActionAction(facesView, "myAction", "items", "missing");

		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void executeActionActionThrowsWhenActionIsNotInTheView() {
		View view = mockActionView();
		when(view.getAction("myAction")).thenReturn(null);

		ExecuteActionAction action = new ExecuteActionAction(currentFacesView, "myAction", null, null);

		assertThrows(MetaDataException.class, action::callback);
	}

	@Test
	void executeActionActionRefusesAnImplicitAction() {
		View view = mockActionView();
		Action viewAction = mock(Action.class);
		when(view.getAction("myAction")).thenReturn(viewAction);

		assertRefusedWithASecurityException(new ExecuteActionAction(currentFacesView, "myAction", null, null)::callback);
	}

	@Test
	void executeDownloadActionThrowsWhenActionIsNotInTheView() {
		View view = mockActionView();
		when(view.getAction("myAction")).thenReturn(null);

		ExecuteDownloadAction action = new ExecuteDownloadAction(currentFacesView, "myAction", null, null);

		assertThrows(MetaDataException.class, action::callback);
	}

	@Test
	void executeDownloadActionRefusesAnImplicitAction() {
		View view = mockActionView();
		Action viewAction = mock(Action.class);
		when(view.getAction("myAction")).thenReturn(viewAction);

		assertRefusedWithASecurityException(new ExecuteDownloadAction(currentFacesView, "myAction", null, null)::callback);
	}

	/**
	 * Mocks a user, customer, module and document for a faces view of a bean, returning the document's create view
	 * for stubbing its actions.
	 */
	private View mockActionView() {
		installRequestSelectionContext();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Bean bean = mock(Bean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getName()).thenReturn("create");

		currentBean = bean;
		currentFacesView = mock(FacesView.class);
		when(currentFacesView.getBean()).thenReturn(bean);
		return view;
	}

	@Test
	void removeActionThrowsWhenInlineElementIsNotInTheCollection() {
		HashMap<String, Object> values = new HashMap<>();
		values.put("items", new java.util.ArrayList<>());
		DynamicBean rootBean = new DynamicBean("admin", "Contact", values);

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(rootBean);
		when(facesView.getViewBinding()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, "items", "missing", null);

		assertThrows(IllegalStateException.class, action::callback);
	}

	@Test
	void chartActionRefusesAViewWithNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);

		ChartAction action = new ChartAction(facesView, "model", ChartType.bar);

		assertIllegalState("Cannot chart model as the FacesView has no target bean", action::callback);
	}

	@Test
	void completeActionRefusesAViewWithNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);

		CompleteAction action = new CompleteAction(facesView, "q", "text", CompleteType.previous);

		assertIllegalState("Cannot complete text as the FacesView has no target bean", action::callback);
	}

	@Test
	void deleteActionRefusesAViewWithNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);

		assertIllegalState("Cannot delete as the FacesView has no target bean", new DeleteAction(facesView)::callback);
	}

	@Test
	void deleteActionRefusesAViewWithNoWebContext() {
		Bean bean = mock(Bean.class);
		AbstractPersistence persistence = mockSaveContext(bean, Boolean.FALSE);
		when(currentFacesView.getWebContext()).thenReturn(null);

		assertIllegalState("Cannot delete as the FacesView has no web context", new DeleteAction(currentFacesView)::callback);
		verify(persistence, never()).delete(any(Document.class), any(PersistentBean.class));
	}

	@Test
	void deleteActionRefusesAPersistedBeanWithNoOptimisticLock() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		PersistentBean bean = mock(PersistentBean.class);
		PersistentBean persisted = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		when(user.getCustomer()).thenReturn(customer);
		when(user.canDeleteDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canReadBean("b1", "admin", "Contact", "demo", "dg", "u1")).thenReturn(Boolean.TRUE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizId()).thenReturn("b1");
		when(bean.isPersisted()).thenReturn(Boolean.TRUE);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(persistence.retrieve(document, "b1")).thenReturn(persisted);
		when(persisted.getBizId()).thenReturn("b1");
		when(persisted.getBizModule()).thenReturn("admin");
		when(persisted.getBizDocument()).thenReturn("Contact");
		when(persisted.getBizCustomer()).thenReturn("demo");
		when(persisted.getBizDataGroupId()).thenReturn("dg");
		when(persisted.getBizUserId()).thenReturn("u1");

		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(bean);

		assertIllegalState("Persisted bean admin.Contact#b1 has no optimistic lock", new DeleteAction(facesView)::callback);
		verify(persistence, never()).delete(any(Document.class), any(PersistentBean.class));
	}

	@Test
	void editActionRefusesASessionViewWithNoWebContext() {
		FacesView sessionView = mock(FacesView.class);
		when(sessionView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		Map<String, Object> sessionMap = new HashMap<>();
		sessionMap.put(FacesUtil.MANAGED_BEAN_NAME_KEY, sessionView);
		installValidRequestContext(sessionMap);
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());

		assertIllegalState("Cannot edit as the session FacesView has no web context", new EditAction(facesView)::callback);
		verify(facesView).setWebContext(null);
		verify(facesView).setBean(null);
	}

	@Test
	void editActionRestoresTheCurrentBeanOfASessionView() throws Exception {
		Bean bean = mockContactBean();
		when(bean.getBizId()).thenReturn("b1");
		MockWebContext webContext = new MockWebContext();
		webContext.setCurrentBean(bean);
		FacesView sessionView = mock(FacesView.class);
		when(sessionView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		when(sessionView.getWebContext()).thenReturn(webContext);
		Map<String, Object> sessionMap = new HashMap<>();
		sessionMap.put(FacesUtil.MANAGED_BEAN_NAME_KEY, sessionView);
		installValidRequestContext(sessionMap);
		User user = mock(User.class);
		when(user.canAccess(any(), org.mockito.ArgumentMatchers.eq("desktop"))).thenReturn(Boolean.TRUE);
		bindPersistenceForUser(user);
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());

		new EditAction(facesView).callback();

		assertTrue(sessionMap.isEmpty());
		verify(facesView).setBizModuleParameter("admin");
		verify(facesView).setBizDocumentParameter("Contact");
		verify(facesView).setWebContext(webContext);
		verify(facesView).setBean(bean);
	}

	@Test
	void executeActionActionRefusesAViewWithNoWebContext() {
		Bean bean = mock(Bean.class);
		FacesView facesView = mockExecutableActionView(bean);

		assertIllegalState("Cannot execute action myAction as the FacesView has no web context",
							new ExecuteActionAction(facesView, "myAction", null, null)::callback);
	}

	@Test
	void executeActionActionRefusesAViewWhoseBeanIsGoneBeforeExecuting() throws Exception {
		Bean bean = mock(Bean.class);
		FacesView facesView = mockExecutableActionView(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);
		when(facesView.getBean()).thenReturn(bean, (Bean) null);

		assertIllegalState("Cannot execute action myAction as the FacesView has no bean",
							new ExecuteActionAction(facesView, "myAction", null, null)::callback);
		verify(currentServerSideAction, never()).execute(any(), any());
	}

	@Test
	void executeActionActionRefusesAViewWhoseBeanIsGoneAfterExecuting() throws Exception {
		Bean bean = mock(Bean.class);
		Bean resultBean = mock(Bean.class);
		FacesView facesView = mockExecutableActionView(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);
		// the target bean, the context bean, the bean the result replaces, then no bean
		when(facesView.getBean()).thenReturn(bean, bean, bean, null);
		when(currentServerSideAction.execute(bean, webContext)).thenReturn(new ServerSideActionResult<>(resultBean));

		assertIllegalState("The FacesView has no bean after executing action myAction",
							new ExecuteActionAction(facesView, "myAction", null, null)::callback);
		verify(facesView).setBean(resultBean);
	}

	@Test
	void executeDownloadActionRefusesAViewWithNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);

		assertIllegalState("Cannot execute download action myAction as there is no target bean for data widget null with selected row null",
							new ExecuteDownloadAction(facesView, "myAction", null, null)::callback);
	}

	@Test
	void executeDownloadActionRefusesAViewWithNoWebContext() {
		FacesView facesView = mockExecutableActionView(mock(Bean.class));

		assertIllegalState("Cannot execute download action myAction as the FacesView has no web context",
							new ExecuteDownloadAction(facesView, "myAction", null, null)::callback);
	}

	@Test
	void populateActionRefusesAViewWithNoModuleParameter() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getQueryNameParameter()).thenReturn("q");

		assertIllegalState("Cannot populate a list as the FacesView has no module parameter", new PopulateAction(facesView)::callback);
		verify(facesView, never()).setTitle(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void populateActionRefusesAListModelWithNoDrivingDocument() {
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

		assertIllegalState("List model q of document admin.Contact has no driving document", new PopulateAction(facesView)::callback);
		verify(facesView).setTitle("List Title");
		verify(facesView, never()).setModelName(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void removeActionRefusesAZoomedRemoveWithNoViewBinding() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(new DynamicBean("admin", "Contact", new HashMap<>()));

		assertIllegalState("Cannot remove on a zoomed view as the FacesView has no view binding",
							new RemoveAction(facesView, null, null, null)::callback);
	}

	@Test
	void removeActionRefusesAZoomedRemoveWithNoWebContext() {
		HashMap<String, Object> values = new HashMap<>();
		values.put("child", mock(Bean.class));
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(new DynamicBean("admin", "Contact", values));
		when(facesView.getViewBinding()).thenReturn("child");

		assertIllegalState("Cannot remove child as the FacesView has no web context",
							new RemoveAction(facesView, null, null, null)::callback);
	}

	@Test
	void saveActionRefusesAViewWithNoTargetBean() {
		bindPersistenceForUser(mock(User.class));
		FacesView facesView = mock(FacesView.class);

		assertIllegalState("Cannot save as the FacesView has no target bean", new SaveAction(facesView, false)::callback);
	}

	@Test
	void saveActionRefusesAViewWithNoWebContext() {
		AbstractPersistence persistence = mockSaveContext(mock(PersistentBean.class), Boolean.FALSE);
		when(currentFacesView.getWebContext()).thenReturn(null);

		assertIllegalState("Cannot save as the FacesView has no web context", new SaveAction(currentFacesView, false)::callback);
		verify(persistence, never()).save(any(Document.class), any(PersistentBean.class));
	}

	@Test
	void zoomInActionRefusesAViewWithNoCurrentBean() {
		installValidRequestContext(new HashMap<>());
		FacesView facesView = mock(FacesView.class);

		assertIllegalState("Cannot zoom in to items as the FacesView has no current bean",
							new ZoomInAction(facesView, "items", "e1")::callback);
		verify(facesView, never()).setViewBinding(org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void zoomInActionRefusesToInstantiateABeanWithNoParentBean() {
		installValidRequestContext(new HashMap<>());
		ArrayDeque<String> zoomInBindings = new ArrayDeque<>();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getCurrentBean()).thenReturn(new BeanMapAdapter(null, null));
		when(facesView.getZoomInBindings()).thenReturn(zoomInBindings);
		User user = mock(User.class);
		when(facesView.getUser()).thenReturn(user);

		assertIllegalState("Cannot instantiate itemsElementById(e1) as the FacesView has no parent bean",
							new ZoomInAction(facesView, "items", "e1")::callback);
		assertEquals(List.of("itemsElementById(e1)"), List.copyOf(zoomInBindings));
		verify(facesView).setViewBinding("itemsElementById(e1)");
	}

	@Test
	void zoomInActionRefusesAViewWithNoWebContext() {
		installValidRequestContext(new HashMap<>());
		mockInternalDocument();
		Bean bean = mockContactBean();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getCurrentBean()).thenReturn(new BeanMapAdapter(bean, null));
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		when(facesView.getBean()).thenReturn(bean);

		assertIllegalState("Cannot zoom in to items as the FacesView has no web context",
							new ZoomInAction(facesView, "items", null)::callback);
		verify(facesView, never()).setPostRender(any(), any());
	}

	@Test
	void zoomOutActionRefusesAViewWithNoTargetBean() {
		installValidRequestContext(new HashMap<>());
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());

		assertIllegalState("Cannot zoom out as the FacesView has no target bean", new ZoomOutAction(facesView)::callback);
	}

	@Test
	void zoomOutActionRefusesAViewWithNoWebContext() {
		installValidRequestContext(new HashMap<>());
		mockInternalDocument();
		Bean bean = mockContactBean();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		when(facesView.getBean()).thenReturn(bean);

		assertIllegalState("Cannot zoom out as the FacesView has no web context", new ZoomOutAction(facesView)::callback);
	}

	@Test
	void zoomOutActionRefusesAViewWithNoViewBinding() {
		installValidRequestContext(new HashMap<>());
		mockInternalDocument();
		Bean bean = mockContactBean();
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		when(facesView.getBean()).thenReturn(bean);
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		assertIllegalState("Cannot zoom out as the FacesView has no view binding", new ZoomOutAction(facesView)::callback);
		verify(facesView).setBean(bean);
	}

	@Test
	void zoomOutActionOrdersTheCollectionOfAZoomedOutElementInTheConversationBean() {
		installValidRequestContext(new HashMap<>());
		mockInternalDocument();
		Bean element = mockContactBean();
		when(element.getBizId()).thenReturn("e1");
		HashMap<String, Object> values = new HashMap<>();
		values.put("items", new java.util.ArrayList<>(List.of(element)));
		DynamicBean root = new DynamicBean("admin", "Contact", values);
		Bean conversationBean = mock(Bean.class);
		when(conversationBean.getBizId()).thenReturn("c1");
		IllegalStateException ordering = new IllegalStateException("ordering");
		when(conversationBean.getModuleMetaData()).thenThrow(ordering);
		MockWebContext webContext = new MockWebContext();
		webContext.setCurrentBean(conversationBean);
		FacesView facesView = mock(FacesView.class);
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());
		when(facesView.getBean()).thenReturn(root);
		when(facesView.getViewBinding()).thenReturn("itemsElementById(e1)");
		when(facesView.getWebContext()).thenReturn(webContext);

		// The collection is ordered by the metadata of the conversation's current bean
		IllegalStateException e = assertThrows(IllegalStateException.class, new ZoomOutAction(facesView)::callback);

		assertSame(ordering, e, "Not thrown while ordering the conversation bean's collection");
		assertSame(element, ((List<?>) values.get("items")).get(0), "The zoomed out element is not set back in its collection");
	}

	@Test
	void zoomOutRefusesToZoomOutToABindingWithNoTargetBean() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn("child");
		when(facesView.getZoomInBindings()).thenReturn(new ArrayDeque<>());

		assertIllegalState("Cannot zoom out to null as the FacesView has no target bean",
							() -> ZoomOutAction.zoomOut(facesView, mock(CustomerImpl.class)));
		verify(facesView).setViewBinding(null);
	}

	@Test
	void addActionRefusesAViewWithNoWebContext() {
		setThreadPersistenceWithAUser();
		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getCurrentBean()).thenReturn(new BeanMapAdapter(mock(Bean.class), null));

		assertIllegalState("Cannot add to items as the FacesView has no web context", new AddAction(facesView, "items", true)::callback);
	}

	@Test
	void addActionAddsTheNewElementToTheCurrentBean() {
		User user = mock(User.class);
		IllegalStateException adding = new IllegalStateException("adding");
		when(user.getCustomer()).thenThrow(adding);
		bindPersistenceForUser(user);
		FacesView facesView = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		when(facesView.getBean()).thenReturn(bean);
		when(facesView.getCurrentBean()).thenReturn(new BeanMapAdapter(mock(Bean.class), null));
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		when(facesView.getWebContext()).thenReturn(webContext);

		// WebUtil.addNewElement() starts by resolving the user's customer
		IllegalStateException e = assertThrows(IllegalStateException.class, new AddAction(facesView, "items", true)::callback);

		assertSame(adding, e, "Not thrown while adding the new element");
		verify(facesView, never()).setViewBinding(org.mockito.ArgumentMatchers.anyString());
	}

	private static void assertIllegalState(String expectedMessage, Executable action) {
		IllegalStateException e = assertThrows(IllegalStateException.class, action);
		assertEquals(expectedMessage, e.getMessage());
	}

	/**
	 * Mocks a user with an internal customer and an internal admin.Contact document with no Bizlet.
	 */
	private static void mockInternalDocument() {
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentImpl document = mock(DocumentImpl.class);
		bindPersistenceForUser(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
	}

	private static Bean mockContactBean() {
		Bean result = mock(Bean.class);
		when(result.getBizModule()).thenReturn("admin");
		when(result.getBizDocument()).thenReturn("Contact");
		return result;
	}

	private ServerSideAction<Bean> currentServerSideAction;

	/**
	 * Mocks a faces view of the bean whose create view has the server-side action "myAction" with no client validation,
	 * which the user can execute.
	 */
	private FacesView mockExecutableActionView(Bean bean) {
		installValidRequestContext(new HashMap<>());
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);
		Action actionMeta = mock(Action.class);
		currentServerSideAction = mock(ServerSideAction.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(user.canExecuteAction(document, "myAction")).thenReturn(Boolean.TRUE);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getView("desktop", customer, "create")).thenReturn(view);
		when(view.getAction("myAction")).thenReturn(actionMeta);
		when(actionMeta.getResourceName()).thenReturn("myAction");
		when(actionMeta.getClientValidation()).thenReturn(Boolean.FALSE);
		doReturn(currentServerSideAction).when(actionMeta).getServerSideAction(customer, document);

		FacesView result = mock(FacesView.class);
		when(result.getBean()).thenReturn(bean);
		return result;
	}

	/**
	 * Installs a desktop request whose view root is not rendered, so its required fields are valid.
	 */
	private static void installValidRequestContext(Map<String, Object> sessionMap) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		RequestUxUiSelectionTestUtil.install(request,
				UserAgentType.desktop,
				false,
				UxUi.newPrimeFaces(UxUi.DESKTOP_NAME, "editorial", "skyve", "blue"));
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContext context = mock(FacesContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(context.getViewRoot()).thenReturn(viewRoot);
		FacesContextBridge.setCurrent(context);
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	private static void installRequestSelectionContext() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		RequestUxUiSelectionTestUtil.install(request,
				UserAgentType.desktop,
				false,
				UxUi.newPrimeFaces(UxUi.DESKTOP_NAME, "editorial", "skyve", "blue"));
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		FacesContext context = mock(FacesContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		FacesContextBridge.setCurrent(context);
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));
	}

	@SuppressWarnings("java:S3011") // Exercises private view-binding setup without widening its production visibility
	private static void invokeSetupViewForZoomIn(EditAction action, Bean bean, String bindingParameter)
	throws ReflectiveOperationException {
		Method method = EditAction.class.getDeclaredMethod("setupViewForZoomIn", Bean.class, String.class);
		method.setAccessible(true);
		method.invoke(action, bean, bindingParameter);
	}

	private static void installFailingRequiredValidationContext() {
		UIViewRoot root = mock(UIViewRoot.class);
		doReturn(Boolean.TRUE).when(root).isRendered();

		HtmlInputText requiredInput = mock(HtmlInputText.class);
		doReturn(Boolean.TRUE).when(requiredInput).isRendered();
		when(requiredInput.getRequiredMessage()).thenReturn("Required");
		when(requiredInput.getValue()).thenReturn(null);
		when(requiredInput.getStyle()).thenReturn("");
		when(requiredInput.getFacetsAndChildren()).thenAnswer(i -> java.util.Arrays.asList().iterator());

		when(root.getFacetsAndChildren()).thenAnswer(i -> java.util.Arrays.asList(requiredInput).iterator());

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);
	}

	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
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

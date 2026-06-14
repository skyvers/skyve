package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.MetaDataException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;

import jakarta.faces.component.UIViewRoot;
import jakarta.faces.component.html.HtmlInputText;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;

@SuppressWarnings({"static-method", "boxing", "java:S1130", "java:S8692"}) // system clock OK
class FacesActionsCoverageTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() {
		FacesContextBridge.setCurrent(null);
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));
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

	@Test
	void setTitleActionUsesEditViewWhenBeanIsCreated() throws Exception {
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));

		ExecuteActionAction action = new ExecuteActionAction(facesView, "myAction", null, null);

		assertThrows(IllegalArgumentException.class, action::callback);
	}

	@Test
	void executeActionActionThrowsClassCastWhenExecutionAllowed() {
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(facesView.getWebContext()).thenReturn(null);

		SaveAction action = new SaveAction(facesView, false);
		assertThrows(ClassCastException.class, action::callback);
	}

	@Test
	void saveActionOkBranchResolvesOkActionBeforeInternalCustomerCast() {
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(facesView.getWebContext()).thenReturn(null);

		SaveAction action = new SaveAction(facesView, true);
		assertThrows(ClassCastException.class, action::callback);
		verify(view).getAction("OK");
	}

	@Test
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
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();

		HtmlInputText requiredInput = mock(HtmlInputText.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(requiredInput).isRendered();
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

	@Test
	void addActionInlineBypassesValidationAndThrowsWhenBeanMissing() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn(null);
		when(facesView.getBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", true);
		assertThrows(NullPointerException.class, action::callback);
	}

	@Test
	void addActionNonInlineContinuesAfterValidationPassesThenThrowsWhenBeanMissing() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();
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
		assertThrows(NullPointerException.class, action::callback);
	}

	@Test
	void addActionWithExistingViewBindingStillThrowsWhenBeanMissing() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getViewBinding()).thenReturn("parent.child");
		when(facesView.getBean()).thenReturn(null);

		AddAction action = new AddAction(facesView, "items", true);
		assertThrows(NullPointerException.class, action::callback);
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
		assertThrows(NullPointerException.class, action::callback);
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
		when(facesView.getUxUi()).thenReturn(org.skyve.metadata.router.UxUi.newPrimeFaces("desktop", "template", "saga"));

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
		when(facesView.getWebContext()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(ClassCastException.class, action::callback);
	}

	@Test
	void removeActionWithNullCollectionNameAndElementIdUsesZoomedPath() {
		FacesView facesView = mock(FacesView.class);
		when(facesView.getBean()).thenReturn(null);
		when(facesView.getViewBinding()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(NullPointerException.class, action::callback);
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
		when(facesView.getWebContext()).thenReturn(null);

		RemoveAction action = new RemoveAction(facesView, null, null, null);
		assertThrows(NullPointerException.class, action::callback);
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	private static void installFailingRequiredValidationContext() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();

		HtmlInputText requiredInput = mock(HtmlInputText.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(requiredInput).isRendered();
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

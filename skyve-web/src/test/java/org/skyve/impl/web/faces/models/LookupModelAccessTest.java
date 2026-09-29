package org.skyve.impl.web.faces.models;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.web.UserAgentType;

import jakarta.faces.component.UIPanel;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

class LookupModelAccessTest {
	private ThreadLocal<AbstractPersistence> persistenceThread;
	private AbstractPersistence previousPersistence;
	private User user;
	private Customer customer;
	private Document owner;
	private Document target;
	private ListModel<Bean> model;
	private FacesView view;
	private Bean bean;

	@BeforeEach
	@SuppressWarnings("unchecked")
	void setUp() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		persistenceThread = (ThreadLocal<AbstractPersistence>) field.get(null);
		previousPersistence = persistenceThread.get();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		persistence.setForThread();
		user = mock(User.class);
		customer = mock(Customer.class);
		Module module = mock(Module.class);
		owner = mock(Document.class);
		target = mock(Document.class);
		model = mock(ListModel.class);
		view = mock(FacesView.class);
		bean = mock(Bean.class);
		when(view.getCurrentBean()).thenReturn(new BeanMapAdapter(bean, null));
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("testUser");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(owner);
		when(owner.getOwningModuleName()).thenReturn("sales");
		when(owner.getName()).thenReturn("Order");
		when(owner.getListModel(customer, "ContactsModel", true)).thenReturn(model);
		when(model.getDrivingDocument()).thenReturn(target);
		when(target.getOwningModuleName()).thenReturn("admin");
		when(target.getName()).thenReturn("Contact");
		HttpServletRequest request = mock(HttpServletRequest.class);
		UxUi uxui = UxUi.newSmartClient("desktop", "Tahoe", "saga");
		RequestUxUiSelectionTestUtil.install(request, UserAgentType.desktop, false, uxui);
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));
	}

	@AfterEach
	void tearDown() {
		WebContainer.clear();
		if (previousPersistence == null) {
			persistenceThread.remove();
		}
		else {
			persistenceThread.set(previousPersistence);
		}
	}

	@Test
	void deniedModelAccessDoesNotInstantiateModel() {
		// SecurityException logs through a separate persistence instance, unavailable in this unit fixture.
		SkyveLazyDataModel lookup = lookup();
		IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, () -> lookup.load(0, 20, null, null));
		assertTrue(java.util.Arrays.stream(failure.getStackTrace()).anyMatch(frame ->
				"org.skyve.domain.messages.SecurityException".equals(frame.getClassName())));
		verify(user).canAccess(UserAccess.modelAggregate("sales", "Order", "ContactsModel"), "desktop");
		verify(owner, never()).getListModel(customer, "ContactsModel", true);
	}

	@Test
	@SuppressWarnings("boxing")
	void deniedDrivingDocumentReadDoesNotFetchRows() throws Exception {
		when(user.canAccess(UserAccess.modelAggregate("sales", "Order", "ContactsModel"), "desktop")).thenReturn(true);
		// SecurityException logs through a separate persistence instance, unavailable in this unit fixture.
		SkyveLazyDataModel lookup = lookup();
		IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, () -> lookup.load(0, 20, null, null));
		assertTrue(java.util.Arrays.stream(failure.getStackTrace()).anyMatch(frame ->
				"org.skyve.domain.messages.SecurityException".equals(frame.getClassName())));
		verify(user).canReadDocument(target);
		verify(model, never()).fetch();
	}

	@Test
	@SuppressWarnings("boxing")
	void permittedModelReceivesCurrentBeanAndPagination() throws Exception {
		when(user.canAccess(UserAccess.modelAggregate("sales", "Order", "ContactsModel"), "desktop")).thenReturn(true);
		when(user.canReadDocument(target)).thenReturn(true);
		Page page = new Page();
		page.setRows(List.of());
		when(model.fetch()).thenReturn(page);
		assertEquals(List.of(), lookup().load(0, 20, null, null));
		verify(model).setBean(bean);
		verify(model).setEndRow(20);
		verify(model).fetch();
	}

	@Test
	@SuppressWarnings("boxing")
	void facesLookupUsesItsOwnModelAndSeparatesModelCaches() throws Exception {
		when(user.canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.eq("desktop"))).thenReturn(true);
		when(user.canReadDocument(target)).thenReturn(true);
		when(owner.getListModel(customer, "OtherContactsModel", true)).thenReturn(model);
		Page page = new Page();
		page.setRows(List.of());
		when(model.fetch()).thenReturn(page);
		FacesView faces = spy(new FacesView());
		doReturn(new BeanMapAdapter(bean, null)).when(faces).getCurrentBean();
		faces.setModelName("UnrelatedPageModel");
		try (MockFacesContext context = MockFacesContext.get()) {
			UIPanel component = new UIPanel();
			component.getAttributes().put("module", "sales");
			component.getAttributes().put("document", "Order");
			component.getAttributes().put("model", "ContactsModel");
			component.getAttributes().put("display", "bizKey");
			component.pushComponentToEL(context, component);
			try {
				assertEquals(List.of(), faces.lookup(""));
				assertEquals(List.of(), faces.lookup(""));
				component.getAttributes().put("model", "OtherContactsModel");
				assertEquals(List.of(), faces.lookup(""));
				verify(owner).getListModel(customer, "ContactsModel", true);
				verify(owner).getListModel(customer, "OtherContactsModel", true);
				verify(owner, never()).getListModel(customer, "UnrelatedPageModel", true);
			}
			finally {
				component.popComponentFromEL(context);
			}
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void lookupSearchMatchesEitherColumnAndPreservesExistingFilter() throws Exception {
		when(user.canAccess(UserAccess.modelAggregate("sales", "Order", "ContactsModel"), "desktop")).thenReturn(true);
		when(user.canReadDocument(target)).thenReturn(true);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);
		org.skyve.metadata.model.Attribute name = mock(org.skyve.metadata.model.Attribute.class);
		org.skyve.metadata.model.Attribute email = mock(org.skyve.metadata.model.Attribute.class);
		when(target.getAttribute("name")).thenReturn(name);
		when(target.getAttribute("email")).thenReturn(email);
		doReturn(String.class).when(name).getImplementingType();
		doReturn(String.class).when(email).getImplementingType();
		when(name.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		when(email.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		org.skyve.metadata.view.model.list.InMemoryFilter filter = new org.skyve.metadata.view.model.list.InMemoryFilter();
		filter.addEquals("enabled", Boolean.TRUE);
		when(model.getFilter()).thenReturn(filter);
		when(model.newFilter()).thenAnswer(invocation -> new org.skyve.metadata.view.model.list.InMemoryFilter());
		Bean byName = new org.skyve.domain.DynamicBean("admin", "Contact", new java.util.TreeMap<>(java.util.Map.of("name", "Alice", "email", "other", "enabled", Boolean.TRUE)));
		Bean byEmail = new org.skyve.domain.DynamicBean("admin", "Contact", new java.util.TreeMap<>(java.util.Map.of("name", "Bob", "email", "alice@example.com", "enabled", Boolean.TRUE)));
		Bean excluded = new org.skyve.domain.DynamicBean("admin", "Contact", new java.util.TreeMap<>(java.util.Map.of("name", "Alice", "email", "alice@example.com", "enabled", Boolean.FALSE)));
		when(model.fetch()).thenAnswer(invocation -> {
			List<Bean> rows = new java.util.ArrayList<>(List.of(byName, byEmail, excluded));
			filter.filter(rows);
			Page page = new Page();
			page.setRows(rows);
			return page;
		});
		SkyveLazyDataModel lookup = lookup();
		lookup.setLookupFilter(List.of("name", "email"), "  ALICE  ");
		assertEquals(List.of(byName, byEmail), lookup.load(0, 20, null, null).stream().map(BeanMapAdapter::getBean).toList());
		verify(model).setEndRow(20);
	}

	@Test
	@SuppressWarnings("boxing")
	void blankLookupSearchDoesNotAddAnyFilter() throws Exception {
		when(user.canAccess(UserAccess.modelAggregate("sales", "Order", "ContactsModel"), "desktop")).thenReturn(true);
		when(user.canReadDocument(target)).thenReturn(true);
		Page page = new Page();
		page.setRows(List.of());
		when(model.fetch()).thenReturn(page);
		SkyveLazyDataModel lookup = lookup();
		lookup.setLookupFilter(List.of("name", "email"), "  ");
		assertEquals(List.of(), lookup.load(0, 20, null, null));
		verify(model, never()).newFilter();
	}

	@Test
	@SuppressWarnings("boxing")
	void facesLookupSeparatesSearchFieldsAndSearchTextInCache() throws Exception {
		when(user.canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.eq("desktop"))).thenReturn(true);
		when(user.canReadDocument(target)).thenReturn(true);
		Module adminModule = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(adminModule);
		when(model.newFilter()).thenAnswer(invocation -> mock(org.skyve.metadata.view.model.list.Filter.class));
		org.skyve.metadata.view.model.list.Filter filter = mock(org.skyve.metadata.view.model.list.Filter.class);
		when(model.getFilter()).thenReturn(filter);
		Page page = new Page();
		page.setRows(List.of());
		when(model.fetch()).thenReturn(page);
		FacesView faces = spy(new FacesView());
		doReturn(new BeanMapAdapter(bean, null)).when(faces).getCurrentBean();
		try (MockFacesContext context = MockFacesContext.get()) {
			UIPanel component = new UIPanel();
			component.setId("lookup");
			component.getAttributes().put("module", "sales");
			component.getAttributes().put("document", "Order");
			component.getAttributes().put("model", "ContactsModel");
			component.getAttributes().put("display", "bizKey");
			component.pushComponentToEL(context, component);
			try {
				faces.lookup("alice");
				faces.lookup("alice");
				component.getAttributes().put("filterFields", List.of("bizId"));
				faces.lookup("alice");
				faces.lookup("bob");
				verify(model, times(3)).fetch();
			}
			finally {
				component.popComponentFromEL(context);
			}
		}
	}

	private SkyveLazyDataModel lookup() {
		return new SkyveLazyDataModel(view, "sales", "Order", null, "ContactsModel", List.of(), List.of(), false);
	}
}

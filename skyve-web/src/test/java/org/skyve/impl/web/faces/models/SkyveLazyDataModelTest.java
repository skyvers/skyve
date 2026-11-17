package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.model.FilterMeta;
import org.primefaces.model.SortMeta;
import org.primefaces.model.SortOrder;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.web.SortParameter;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class SkyveLazyDataModelTest {
	@AfterEach
	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
	void tearDown() throws ReflectiveOperationException {
		WebContainer.clear();
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	@Test
	void countAlwaysReturnsZero() {
		@SuppressWarnings("null") // passes null on purpose
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), null, null, null, null, null, null, false);
		assertEquals(0, model.count(Map.of()));
	}

	@Test
	void getRowKeyFormatsBizIdDocumentAndModule() {
		Bean rowBean = mock(Bean.class);
		when(rowBean.getBizId()).thenReturn("abc123");
		when(rowBean.getBizDocument()).thenReturn("Order");
		when(rowBean.getBizModule()).thenReturn("sales");

		BeanMapAdapter adapter = new BeanMapAdapter(rowBean, null);
		@SuppressWarnings("null") // passes null on purpose
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), null, null, null, null, null, null, false);

		assertEquals("abc123#Order.sales", model.getRowKey(adapter));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void getRowDataParsesRowKeyToDynamicBean() {
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), null, null, null, null, null, null, false);

		BeanMapAdapter adapter = model.getRowData("row42#Invoice.admin");
		assertNotNull(adapter);
		assertEquals("row42", adapter.getBean().getBizId());
		assertEquals("Invoice", adapter.getBean().getBizDocument());
		assertEquals("admin", adapter.getBean().getBizModule());
	}

	@Test
	void sortBuildsSortParametersForAscendingAndDescending() throws Exception {
		SortMeta ascending = mock(SortMeta.class);
		when(ascending.getField()).thenReturn("name");
		when(ascending.getOrder()).thenReturn(SortOrder.ASCENDING);
		SortMeta descending = mock(SortMeta.class);
		when(descending.getField()).thenReturn("created");
		when(descending.getOrder()).thenReturn(SortOrder.DESCENDING);

		Map<String, SortMeta> multiSort = new LinkedHashMap<>();
		multiSort.put("name", ascending);
		multiSort.put("created", descending);

		ListModel<Bean> listModel = mock(ListModel.class);

		invokeSort(multiSort, listModel);

		org.mockito.ArgumentCaptor<SortParameter[]> captor = org.mockito.ArgumentCaptor.forClass(SortParameter[].class);
		verify(listModel).setSortParameters(captor.capture());
		SortParameter[] sortParameters = captor.getValue();
		assertEquals(2, sortParameters.length);
		assertEquals("name", sortParameters[0].getBy());
		assertEquals(null, sortParameters[0].getDirection());
		assertEquals("created", sortParameters[1].getBy());
		assertEquals(SortDirection.descending, sortParameters[1].getDirection());
	}

	@Test
	void filterSkipsNullFilterValuesWithoutMutatingModelFilter() throws Exception {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn(null);
		Map<String, FilterMeta> filters = Map.of("description", filterMeta);

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		when(listModel.getFilter()).thenReturn(modelFilter);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("sales");
		when(listModel.getDrivingDocument()).thenReturn(document);

		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("sales")).thenReturn(module);

		invokeFilter(filters, listModel, customer);

		verifyNoInteractions(modelFilter);
	}

	private static void invokeSort(Map<String, SortMeta> sortMeta, ListModel<Bean> model) throws Exception {
		Method sort = SkyveLazyDataModel.class.getDeclaredMethod("sort", Map.class, ListModel.class);
		sort.setAccessible(true);
		sort.invoke(null, sortMeta, model);
	}

	private static void invokeFilter(Map<String, FilterMeta> filters, ListModel<Bean> model, Customer customer) throws Exception {
		Method filter = SkyveLazyDataModel.class.getDeclaredMethod("filter", Map.class, ListModel.class, Customer.class);
		filter.setAccessible(true);
		try {
			filter.invoke(null, filters, model, customer);
		}
		catch (java.lang.reflect.InvocationTargetException e) {
			if (e.getCause() instanceof Exception cause) {
				throw cause;
			}
			throw e;
		}
	}

	@Test
	void filterWithEmptyMapLeavesModelFilterUntouched() throws Exception {
		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		when(listModel.getFilter()).thenReturn(modelFilter);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("sales");
		when(listModel.getDrivingDocument()).thenReturn(document);

		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("sales")).thenReturn(module);

		invokeFilter(Map.of(), listModel, customer);

		verifyNoInteractions(modelFilter);
	}

	@Test
	void filterAddsContainsForImplicitBizKey() throws Exception {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn("abc");

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = listModelWithFilter(modelFilter);
		Customer customer = customerForSalesModule();

		invokeFilter(Map.of(Bean.BIZ_KEY, filterMeta), listModel, customer);

		verify(modelFilter).addContains(Bean.BIZ_KEY, "abc");
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void filterAddsContainsForTextAttributeWithoutDomainType() throws Exception {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn("widget");

		Attribute attribute = mock(Attribute.class);
		when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		doReturn(String.class).when(attribute).getImplementingType();

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = listModelWithFilter(modelFilter);
		when(listModel.getDrivingDocument().getAttribute("description")).thenReturn(attribute);
		Customer customer = customerForSalesModule();

		invokeFilter(Map.of("description", filterMeta), listModel, customer);

		verify(modelFilter).addContains("description", "widget");
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void filterCoercesStringValueForIntegerAttribute() throws Exception {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn("42");

		Attribute attribute = mock(Attribute.class);
		when(attribute.getAttributeType()).thenReturn(AttributeType.integer);
		doReturn(Integer.class).when(attribute).getImplementingType();

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = listModelWithFilter(modelFilter);
		when(listModel.getDrivingDocument().getAttribute("quantity")).thenReturn(attribute);
		Customer customer = customerForSalesModule();

		invokeFilter(Map.of("quantity", filterMeta), listModel, customer);

		verify(modelFilter).addEquals("quantity", Integer.valueOf(42));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void filterAddsContainsAgainstBizKeyForAssociationAttribute() throws Exception {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn("Acme");

		Attribute attribute = mock(Attribute.class);
		when(attribute.getAttributeType()).thenReturn(AttributeType.association);
		doReturn(Bean.class).when(attribute).getImplementingType();

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = listModelWithFilter(modelFilter);
		when(listModel.getDrivingDocument().getAttribute("customer")).thenReturn(attribute);
		Customer customer = customerForSalesModule();

		invokeFilter(Map.of("customer", filterMeta), listModel, customer);

		verify(modelFilter).addContains("customer." + Bean.BIZ_KEY, "Acme");
	}

	@Test
	void filterThrowsForUnsupportedValueType() {
		FilterMeta filterMeta = mock(FilterMeta.class);
		Object unsupportedValue = new Object();
		when(filterMeta.getFilterValue()).thenReturn(unsupportedValue);

		Filter modelFilter = mock(Filter.class);
		ListModel<Bean> listModel = listModelWithFilter(modelFilter);
		Customer customer = customerForSalesModule();
		Map<String, FilterMeta> filters = Map.of(Bean.DOCUMENT_ID, filterMeta);

		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> invokeFilter(filters, listModel, customer));

		assertEquals(unsupportedValue + " is not a valid value for param " + Bean.DOCUMENT_ID, exception.getMessage());
	}

	private static ListModel<Bean> listModelWithFilter(Filter modelFilter) {
		ListModel<Bean> listModel = mock(ListModel.class);
		when(listModel.getFilter()).thenReturn(modelFilter);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(listModel.getDrivingDocument()).thenReturn(document);
		return listModel;
	}

	private static Customer customerForSalesModule() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("sales")).thenReturn(module);
		return customer;
	}

	@Test
	void getRowKeyThrowsForARowWithNoBean() {
		@SuppressWarnings("null") // passes null on purpose
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), null, null, null, null, null, null, false);
		BeanMapAdapter row = new BeanMapAdapter(null, null);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> model.getRowKey(row));

		assertEquals("Cannot make a row key for a row with no bean", e.getMessage());
	}

	@Test
	void loadThrowsWhenTheListModelHasNoDrivingDocument() throws Exception {
		ListModel<Bean> listModel = bindModelLoad();
		when(listModel.getDrivingDocument()).thenReturn(null);
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), "sales", "Order", null, "recent", null, null, false);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> model.load(0, 10, null, null));

		assertEquals("List model " + listModel.getClass().getName() + " has no driving document", e.getMessage());
		verify(listModel, never()).fetch();
	}

	@Test
	@SuppressWarnings("boxing")
	void loadThrowsWhenTheFetchedPageHasNoRowsList() throws Exception {
		ListModel<Bean> listModel = bindModelLoad();
		Document drivingDocument = mock(Document.class);
		when(listModel.getDrivingDocument()).thenReturn(drivingDocument);
		User user = AbstractPersistence.get().getUser();
		when(user.canReadDocument(drivingDocument)).thenReturn(Boolean.TRUE);
		Page page = new Page();
		page.setTotalRows(3L);
		when(listModel.fetch()).thenReturn(page);
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), "sales", "Order", null, "recent", null, null, false);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> model.load(0, 10, null, null));

		assertEquals("List model " + listModel.getClass().getName() + " fetched a page with no rows list", e.getMessage());
		assertEquals(3, model.getRowCount());
	}

	@Test
	void applyLookupFilterThrowsWhenTheListModelCannotMakeAFilter() {
		ListModel<Bean> listModel = expressionColumnListModel();
		when(listModel.newFilter()).thenReturn(null);
		SkyveLazyDataModel model = lookupModel();
		Customer customer = customerForSalesModule();
		Document drivingDocument = salesDocument();

		IllegalStateException e = assertThrows(IllegalStateException.class,
												() -> invokeApplyLookupFilter(model, listModel, customer, drivingDocument));

		assertEquals("List model " + listModel.getClass().getName() + " does not support filtering", e.getMessage());
	}

	@Test
	void applyLookupFilterThrowsWhenTheListModelHasNoFilterToAddTheSearchTo() {
		ListModel<Bean> listModel = expressionColumnListModel();
		Filter fieldFilter = mock(Filter.class);
		when(listModel.newFilter()).thenReturn(fieldFilter);
		when(listModel.getFilter()).thenReturn(null);
		SkyveLazyDataModel model = lookupModel();
		Customer customer = customerForSalesModule();
		Document drivingDocument = salesDocument();

		IllegalStateException e = assertThrows(IllegalStateException.class,
												() -> invokeApplyLookupFilter(model, listModel, customer, drivingDocument));

		assertEquals("List model " + listModel.getClass().getName() + " does not support filtering", e.getMessage());
		// the expression alias is searched without resolving a document attribute
		verify(fieldFilter).addContains("total", "abc");
	}

	@Test
	void filterThrowsWhenTheListModelHasNoDrivingDocument() {
		FilterMeta filterMeta = mock(FilterMeta.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		when(listModel.getDrivingDocument()).thenReturn(null);
		Map<String, FilterMeta> filters = Map.of(Bean.BIZ_KEY, filterMeta);
		Customer customer = customerForSalesModule();

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> invokeFilter(filters, listModel, customer));

		assertEquals("List model " + listModel.getClass().getName() + " has no driving document", e.getMessage());
		verifyNoInteractions(filterMeta);
	}

	@Test
	void filterThrowsWhenTheListModelHasNoFilter() {
		FilterMeta filterMeta = mock(FilterMeta.class);
		when(filterMeta.getFilterValue()).thenReturn("abc");
		ListModel<Bean> listModel = listModelWithFilter(null);
		Map<String, FilterMeta> filters = Map.of(Bean.BIZ_KEY, filterMeta);
		Customer customer = customerForSalesModule();

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> invokeFilter(filters, listModel, customer));

		assertEquals("List model " + listModel.getClass().getName() + " does not support filtering", e.getMessage());
	}

	/**
	 * Binds a desktop request and a user who can access the "recent" model of sales.Order, returning that model.
	 */
	@SuppressWarnings("boxing")
	private static ListModel<Bean> bindModelLoad() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		RequestUxUiSelectionTestUtil.install(request,
												UserAgentType.desktop,
												false,
												UxUi.newPrimeFaces(UxUi.DESKTOP_NAME, "editorial", "skyve", "blue"));
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));

		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccess(any(), eq(UxUi.DESKTOP_NAME))).thenReturn(Boolean.TRUE);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(document.getListModel(customer, "recent", true)).thenReturn(listModel);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		return listModel;
	}

	/**
	 * A list model whose only column, "total", is an expression with no binding.
	 */
	private static ListModel<Bean> expressionColumnListModel() {
		MetaDataQueryColumn column = mock(MetaDataQueryColumn.class);
		when(column.getName()).thenReturn("total");
		when(column.getBinding()).thenReturn(null);
		ListModel<Bean> listModel = mock(ListModel.class);
		when(listModel.getColumns()).thenReturn(List.of(column));
		return listModel;
	}

	private static SkyveLazyDataModel lookupModel() {
		SkyveLazyDataModel result = new SkyveLazyDataModel(mock(FacesView.class), "sales", "Order", null, null, null, null, false);
		result.setLookupFilter(List.of("total"), "abc");
		return result;
	}

	private static Document salesDocument() {
		Document result = mock(Document.class);
		when(result.getOwningModuleName()).thenReturn("sales");
		return result;
	}

	private static void invokeApplyLookupFilter(SkyveLazyDataModel model, ListModel<Bean> listModel, Customer customer, Document drivingDocument)
	throws Exception {
		Method method = SkyveLazyDataModel.class.getDeclaredMethod("applyLookupFilter", ListModel.class, Customer.class, Document.class);
		method.setAccessible(true);
		try {
			method.invoke(model, listModel, customer, drivingDocument);
		}
		catch (InvocationTargetException e) {
			if (e.getCause() instanceof Exception cause) {
				throw cause;
			}
			throw e;
		}
	}
}

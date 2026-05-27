package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.primefaces.model.FilterMeta;
import org.primefaces.model.SortMeta;
import org.primefaces.model.SortOrder;
import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.SortParameter;

@SuppressWarnings("static-method")
class SkyveLazyDataModelTest {

	@Test
	void countAlwaysReturnsZero() {
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
		SkyveLazyDataModel model = new SkyveLazyDataModel(mock(FacesView.class), null, null, null, null, null, null, false);

		assertEquals("abc123#Order.sales", model.getRowKey(adapter));
	}

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
}
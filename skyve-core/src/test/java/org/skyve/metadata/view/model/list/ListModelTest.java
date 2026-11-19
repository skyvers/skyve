package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.persistence.AutoClosingIterable;

/**
 * Tests for the static filter-helper methods on {@link ListModel}.
 */
@SuppressWarnings("java:S8692") // system clock OK
class ListModelTest {

	// ----- addEquals --------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "name", "hello");
		verify(filter).addEquals("name", "hello");
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addEquals(filter, "created", d);
		verify(filter).addEquals("created", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "count", Integer.valueOf(42));
		verify(filter).addEquals("count", Integer.valueOf(42));
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "size", Long.valueOf(100L));
		verify(filter).addEquals("size", Long.valueOf(100L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("3.14");
		ListModel.addEquals(filter, "price", d);
		verify(filter).addEquals("price", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithBooleanCallsBooleanOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "active", Boolean.TRUE);
		verify(filter).addEquals("active", Boolean.TRUE);
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithEnumCallsEnumOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "status", Thread.State.WAITING);
		verify(filter).addEquals("status", Thread.State.WAITING);
	}

	@Test
	@SuppressWarnings("static-method")
	void addEqualsWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addEquals(filter, "val", value));
	}

	// ----- addNotEquals -----------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "name", "hello");
		verify(filter).addNotEquals("name", "hello");
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addNotEquals(filter, "created", d);
		verify(filter).addNotEquals("created", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "count", Integer.valueOf(5));
		verify(filter).addNotEquals("count", Integer.valueOf(5));
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "size", Long.valueOf(200L));
		verify(filter).addNotEquals("size", Long.valueOf(200L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("1.5");
		ListModel.addNotEquals(filter, "rate", d);
		verify(filter).addNotEquals("rate", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithBooleanCallsBooleanOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "flag", Boolean.FALSE);
		verify(filter).addNotEquals("flag", Boolean.FALSE);
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithEnumCallsEnumOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "status", Thread.State.NEW);
		verify(filter).addNotEquals("status", Thread.State.NEW);
	}

	@Test
	@SuppressWarnings("static-method")
	void addNotEqualsWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addNotEquals(filter, "val", value));
	}

	// ----- addGreaterThan ---------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "name", "B");
		verify(filter).addGreaterThan("name", "B");
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addGreaterThan(filter, "startDate", d);
		verify(filter).addGreaterThan("startDate", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "age", Integer.valueOf(18));
		verify(filter).addGreaterThan("age", Integer.valueOf(18));
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "bytes", Long.valueOf(1024L));
		verify(filter).addGreaterThan("bytes", Long.valueOf(1024L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("10.00");
		ListModel.addGreaterThan(filter, "amount", d);
		verify(filter).addGreaterThan("amount", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addGreaterThan(filter, "val", value));
	}

	// ----- addGreaterThanOrEqualTo ------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "name", "A");
		verify(filter).addGreaterThanOrEqualTo("name", "A");
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addGreaterThanOrEqualTo(filter, "date", d);
		verify(filter).addGreaterThanOrEqualTo("date", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "count", Integer.valueOf(0));
		verify(filter).addGreaterThanOrEqualTo("count", Integer.valueOf(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "size", Long.valueOf(0L));
		verify(filter).addGreaterThanOrEqualTo("size", Long.valueOf(0L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("0.0");
		ListModel.addGreaterThanOrEqualTo(filter, "price", d);
		verify(filter).addGreaterThanOrEqualTo("price", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addGreaterThanOrEqualToWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addGreaterThanOrEqualTo(filter, "val", value));
	}

	// ----- addLessThan ------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "name", "Z");
		verify(filter).addLessThan("name", "Z");
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addLessThan(filter, "endDate", d);
		verify(filter).addLessThan("endDate", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "count", Integer.valueOf(100));
		verify(filter).addLessThan("count", Integer.valueOf(100));
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "id", Long.valueOf(999L));
		verify(filter).addLessThan("id", Long.valueOf(999L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("100.00");
		ListModel.addLessThan(filter, "budget", d);
		verify(filter).addLessThan("budget", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addLessThan(filter, "val", value));
	}

	// ----- addLessThanOrEqualTo ---------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "name", "Z");
		verify(filter).addLessThanOrEqualTo("name", "Z");
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date d = new Date();
		ListModel.addLessThanOrEqualTo(filter, "date", d);
		verify(filter).addLessThanOrEqualTo("date", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "count", Integer.valueOf(10));
		verify(filter).addLessThanOrEqualTo("count", Integer.valueOf(10));
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "id", Long.valueOf(50L));
		verify(filter).addLessThanOrEqualTo("id", Long.valueOf(50L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 d = new Decimal2("50.00");
		ListModel.addLessThanOrEqualTo(filter, "amount", d);
		verify(filter).addLessThanOrEqualTo("amount", d);
	}

	@Test
	@SuppressWarnings("static-method")
	void addLessThanOrEqualToWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addLessThanOrEqualTo(filter, "val", value));
	}

	// ----- addBetween -------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithStringCallsStringOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "name", "A", "Z");
		verify(filter).addBetween("name", "A", "Z");
	}

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithDateCallsDateOverload() {
		Filter filter = mock(Filter.class);
		Date start = new Date(0L);
		Date end = new Date();
		ListModel.addBetween(filter, "date", start, end);
		verify(filter).addBetween("date", start, end);
	}

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithIntegerCallsIntegerOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "count", Integer.valueOf(1), Integer.valueOf(10));
		verify(filter).addBetween("count", Integer.valueOf(1), Integer.valueOf(10));
	}

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithLongCallsLongOverload() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "id", Long.valueOf(1L), Long.valueOf(100L));
		verify(filter).addBetween("id", Long.valueOf(1L), Long.valueOf(100L));
	}

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithDecimalCallsDecimalOverload() {
		Filter filter = mock(Filter.class);
		Decimal2 lo = new Decimal2("1.00");
		Decimal2 hi = new Decimal2("9.99");
		ListModel.addBetween(filter, "price", lo, hi);
		verify(filter).addBetween("price", lo, hi);
	}

	@Test
	@SuppressWarnings("static-method")
	void addBetweenWithUnsupportedTypeThrowsIllegalArgumentException() {
		Filter filter = mock(Filter.class);
		Object start = new Object();
		Object end = new Object();
		assertThrows(IllegalArgumentException.class,
				() -> ListModel.addBetween(filter, "val", start, end));
	}

	// ----- models without filtering -----------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void addParametersWithFilterParametersThrowsWhenModelHasNoFilter() {
		FilterParameter parameter = mock(FilterParameter.class);
		when(parameter.getFilterBinding()).thenReturn(Bean.BIZ_KEY);
		when(parameter.getOperator()).thenReturn(FilterOperator.isNull);
		Document document = mockDrivingDocument();
		ListModel<Bean> model = new NoFilterListModel(null, null, document);
		List<FilterParameter> parameters = List.of(parameter);

		withCustomer(() -> assertThrows(IllegalStateException.class,
											() -> model.addParameters(document, parameters, null)));
	}

	@Test
	@SuppressWarnings("static-method")
	void addParametersWithoutValuesSucceedsWhenModelHasNoFilter() {
		FilterParameter parameter = mock(FilterParameter.class);
		when(parameter.getFilterBinding()).thenReturn(Bean.BIZ_KEY);
		when(parameter.getOperator()).thenReturn(FilterOperator.equal);
		Document document = mockDrivingDocument();
		ListModel<Bean> model = new NoFilterListModel(null, null, document);
		List<FilterParameter> parameters = List.of(parameter);

		withCustomer(() -> assertDoesNotThrow(() -> model.addParameters(document, parameters, null)));
	}

	@Test
	@SuppressWarnings("static-method")
	void addParametersWithNullOrOperatorThrowsWhenModelCannotCreateFilters() {
		FilterParameter parameter = mock(FilterParameter.class);
		when(parameter.getFilterBinding()).thenReturn(Bean.BIZ_KEY);
		when(parameter.getOperator()).thenReturn(FilterOperator.nullOrEqual);
		when(parameter.getValue()).thenReturn("value");
		Document document = mockDrivingDocument();
		ListModel<Bean> model = new NoFilterListModel(mock(Filter.class), null, document);
		List<FilterParameter> parameters = List.of(parameter);

		withCustomer(() -> assertThrows(IllegalStateException.class,
											() -> model.addParameters(document, parameters, null)));
	}

	@Test
	@SuppressWarnings("static-method")
	void addParametersWithNullOrOperatorsAddsANullOrSomethingFilterForEachOperator() {
		Filter filter = mock(Filter.class);
		Filter newFilter = mock(Filter.class);
		List<FilterOperator> operators = List.of(FilterOperator.nullOrEqual,
													FilterOperator.nullOrGreater,
													FilterOperator.nullOrGreaterEqual,
													FilterOperator.nullOrLess,
													FilterOperator.nullOrLessEqual,
													FilterOperator.nullOrLike,
													FilterOperator.nullOrNotEqual,
													FilterOperator.nullOrNotLike);
		List<FilterParameter> parameters = new ArrayList<>();
		for (FilterOperator operator : operators) {
			FilterParameter parameter = mock(FilterParameter.class);
			when(parameter.getFilterBinding()).thenReturn(Bean.BIZ_KEY);
			when(parameter.getOperator()).thenReturn(operator);
			when(parameter.getValue()).thenReturn("value");
			parameters.add(parameter);
		}
		Document document = mockDrivingDocument();
		ListModel<Bean> model = new NoFilterListModel(filter, newFilter, document);

		withCustomer(() -> model.addParameters(document, parameters, null));

		int count = operators.size();
		verify(newFilter).addEquals(Bean.BIZ_KEY, "value");
		verify(newFilter).addGreaterThan(Bean.BIZ_KEY, "value");
		verify(newFilter).addGreaterThanOrEqualTo(Bean.BIZ_KEY, "value");
		verify(newFilter).addLessThan(Bean.BIZ_KEY, "value");
		verify(newFilter).addLessThanOrEqualTo(Bean.BIZ_KEY, "value");
		verify(newFilter).addContains(Bean.BIZ_KEY, "value");
		verify(newFilter).addNotEquals(Bean.BIZ_KEY, "value");
		verify(newFilter).addNotContains(Bean.BIZ_KEY, "value");
		verify(newFilter, times(count)).addNull(Bean.BIZ_KEY);
		verify(newFilter, times(count)).addOr(newFilter);
		verify(filter, times(count)).addAnd(newFilter);
	}

	@Test
	@SuppressWarnings("static-method")
	void getTop100VariantDomainValueCodesReturnsNoCodesWhenThereAreNoDomainValues() {
		CustomerImpl customer = mock(CustomerImpl.class);
		Attribute attribute = mock(Attribute.class);
		DocumentImpl document = mock(DocumentImpl.class);
		when(document.getDomainValues(customer, DomainType.variant, attribute, null, true)).thenReturn(List.of());
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class,
														withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			assertEquals(0, ListModel.getTop100VariantDomainValueCodesFromDescriptionFilter(document, attribute, "x").length);
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void determineColumnTitleThrowsWhenBoundColumnHasNoDrivingDocument() {
		MetaDataQueryColumn column = mock(MetaDataQueryColumn.class);
		when(column.getBinding()).thenReturn(Bean.BIZ_KEY);
		ListModel<Bean> model = new NoFilterListModel(null, null, null);

		assertThrows(IllegalStateException.class, () -> model.determineColumnTitle(column));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineColumnTitleUsesColumnNameWhenUnboundWithoutDrivingDocument() {
		MetaDataQueryColumn column = mock(MetaDataQueryColumn.class);
		when(column.getName()).thenReturn("unbound");
		ListModel<Bean> model = new NoFilterListModel(null, null, null);

		assertEquals("unbound", model.determineColumnTitle(column));
	}

	private static Document mockDrivingDocument() {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("test");
		when(document.getName()).thenReturn("Test");
		return document;
	}

	/**
	 * Runs the given test body with a mocked customer available through {@code CORE.getCustomer()}.
	 */
	private static void withCustomer(Executable body) {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("test")).thenReturn(module);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class,
														withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			body.execute();
		}
		catch (Throwable t) {
			throw new AssertionError(t);
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> tl = (ThreadLocal<AbstractPersistence>) field.get(null);
			tl.remove();
		}
		catch (@SuppressWarnings("unused") Exception ignored) {
			// ignore
		}
	}

	/**
	 * A list model whose filter, new filter and driving document are given (and may be null).
	 */
	private static final class NoFilterListModel extends ListModel<Bean> {
		private final Filter filter;
		private final Filter newFilter;
		private final Document drivingDocument;

		private NoFilterListModel(Filter filter, Filter newFilter, Document drivingDocument) {
			this.filter = filter;
			this.newFilter = newFilter;
			this.drivingDocument = drivingDocument;
		}

		@Override
		public void postConstruct(Customer customer, boolean runtime) {
			// nothing to construct
		}

		@Override
		public String getDescription() {
			return null;
		}

		@Override
		public Document getDrivingDocument() {
			return drivingDocument;
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return Collections.emptyList();
		}

		@Override
		public Set<String> getProjections() {
			return Collections.emptySet();
		}

		@Override
		public Filter getFilter() {
			return filter;
		}

		@Override
		public Filter newFilter() {
			return newFilter;
		}

		@Override
		public void putParameter(String name, Object value) {
			// no parameters
		}

		@Override
		public Page fetch() {
			return new Page();
		}

		@Override
		public AutoClosingIterable<Bean> iterate() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Bean update(String bizId, SortedMap<String, Object> properties) {
			return null;
		}

		@Override
		public void remove(String bizId) {
			// nothing to remove
		}
	}
}

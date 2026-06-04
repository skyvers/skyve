package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;

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
}

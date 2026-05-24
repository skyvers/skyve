package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.Date;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;

/**
 * Tests for the static type-dispatch methods in {@link ListModel}:
 * addEquals, addNotEquals, addGreaterThan, addGreaterThanOrEqualTo,
 * addLessThan, addLessThanOrEqualTo, addBetween.
 *
 * Uses a mocked {@link Filter} to verify the correct overload is dispatched.
 */
@SuppressWarnings("static-method")
class ListModelStaticMethodsTest {

	private Filter filter;

	@BeforeEach
	void setUp() {
		filter = mock(Filter.class);
	}

	// ---- addEquals ----

	@Test
	void addEqualsString() {
		ListModel.addEquals(filter, "name", "foo");
		verify(filter).addEquals("name", "foo");
	}

	@Test
	void addEqualsDate() {
		Date d = new Date();
		ListModel.addEquals(filter, "date", d);
		verify(filter).addEquals("date", d);
	}

	@Test
	void addEqualsInteger() {
		ListModel.addEquals(filter, "count", Integer.valueOf(5));
		verify(filter).addEquals("count", Integer.valueOf(5));
	}

	@Test
	void addEqualsLong() {
		ListModel.addEquals(filter, "count", Long.valueOf(100L));
		verify(filter).addEquals("count", Long.valueOf(100L));
	}

	@Test
	void addEqualsDecimal() {
		Decimal2 d = new Decimal2("1.23");
		ListModel.addEquals(filter, "amount", d);
		verify(filter).addEquals("amount", d);
	}

	@Test
	void addEqualsBoolean() {
		ListModel.addEquals(filter, "active", Boolean.TRUE);
		verify(filter).addEquals("active", Boolean.TRUE);
	}

	@Test
	void addEqualsThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addEquals(filter, "x", new Object()));
	}

	// ---- addNotEquals ----

	@Test
	void addNotEqualsString() {
		ListModel.addNotEquals(filter, "name", "bar");
		verify(filter).addNotEquals("name", "bar");
	}

	@Test
	void addNotEqualsDate() {
		Date d = new Date();
		ListModel.addNotEquals(filter, "date", d);
		verify(filter).addNotEquals("date", d);
	}

	@Test
	void addNotEqualsInteger() {
		ListModel.addNotEquals(filter, "count", Integer.valueOf(3));
		verify(filter).addNotEquals("count", Integer.valueOf(3));
	}

	@Test
	void addNotEqualsLong() {
		ListModel.addNotEquals(filter, "count", Long.valueOf(99L));
		verify(filter).addNotEquals("count", Long.valueOf(99L));
	}

	@Test
	void addNotEqualsDecimal() {
		Decimal2 d = new Decimal2("9.99");
		ListModel.addNotEquals(filter, "amount", d);
		verify(filter).addNotEquals("amount", d);
	}

	@Test
	void addNotEqualsBoolean() {
		ListModel.addNotEquals(filter, "active", Boolean.FALSE);
		verify(filter).addNotEquals("active", Boolean.FALSE);
	}

	@Test
	void addNotEqualsThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addNotEquals(filter, "x", new Object()));
	}

	// ---- addGreaterThan ----

	@Test
	void addGreaterThanString() {
		ListModel.addGreaterThan(filter, "name", "a");
		verify(filter).addGreaterThan("name", "a");
	}

	@Test
	void addGreaterThanDate() {
		Date d = new Date();
		ListModel.addGreaterThan(filter, "date", d);
		verify(filter).addGreaterThan("date", d);
	}

	@Test
	void addGreaterThanInteger() {
		ListModel.addGreaterThan(filter, "count", Integer.valueOf(1));
		verify(filter).addGreaterThan("count", Integer.valueOf(1));
	}

	@Test
	void addGreaterThanLong() {
		ListModel.addGreaterThan(filter, "count", Long.valueOf(2L));
		verify(filter).addGreaterThan("count", Long.valueOf(2L));
	}

	@Test
	void addGreaterThanDecimal() {
		Decimal2 d = new Decimal2("5.00");
		ListModel.addGreaterThan(filter, "amount", d);
		verify(filter).addGreaterThan("amount", d);
	}

	@Test
	void addGreaterThanThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addGreaterThan(filter, "x", new Object()));
	}

	// ---- addGreaterThanOrEqualTo ----

	@Test
	void addGreaterThanOrEqualToString() {
		ListModel.addGreaterThanOrEqualTo(filter, "name", "a");
		verify(filter).addGreaterThanOrEqualTo("name", "a");
	}

	@Test
	void addGreaterThanOrEqualToDate() {
		Date d = new Date();
		ListModel.addGreaterThanOrEqualTo(filter, "date", d);
		verify(filter).addGreaterThanOrEqualTo("date", d);
	}

	@Test
	void addGreaterThanOrEqualToInteger() {
		ListModel.addGreaterThanOrEqualTo(filter, "count", Integer.valueOf(1));
		verify(filter).addGreaterThanOrEqualTo("count", Integer.valueOf(1));
	}

	@Test
	void addGreaterThanOrEqualToLong() {
		ListModel.addGreaterThanOrEqualTo(filter, "count", Long.valueOf(2L));
		verify(filter).addGreaterThanOrEqualTo("count", Long.valueOf(2L));
	}

	@Test
	void addGreaterThanOrEqualToDecimal() {
		Decimal2 d = new Decimal2("5.00");
		ListModel.addGreaterThanOrEqualTo(filter, "amount", d);
		verify(filter).addGreaterThanOrEqualTo("amount", d);
	}

	@Test
	void addGreaterThanOrEqualToThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addGreaterThanOrEqualTo(filter, "x", new Object()));
	}

	// ---- addLessThan ----

	@Test
	void addLessThanString() {
		ListModel.addLessThan(filter, "name", "z");
		verify(filter).addLessThan("name", "z");
	}

	@Test
	void addLessThanDate() {
		Date d = new Date();
		ListModel.addLessThan(filter, "date", d);
		verify(filter).addLessThan("date", d);
	}

	@Test
	void addLessThanInteger() {
		ListModel.addLessThan(filter, "count", Integer.valueOf(10));
		verify(filter).addLessThan("count", Integer.valueOf(10));
	}

	@Test
	void addLessThanLong() {
		ListModel.addLessThan(filter, "count", Long.valueOf(20L));
		verify(filter).addLessThan("count", Long.valueOf(20L));
	}

	@Test
	void addLessThanDecimal() {
		Decimal2 d = new Decimal2("100.00");
		ListModel.addLessThan(filter, "amount", d);
		verify(filter).addLessThan("amount", d);
	}

	@Test
	void addLessThanThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addLessThan(filter, "x", new Object()));
	}

	// ---- addLessThanOrEqualTo ----

	@Test
	void addLessThanOrEqualToString() {
		ListModel.addLessThanOrEqualTo(filter, "name", "z");
		verify(filter).addLessThanOrEqualTo("name", "z");
	}

	@Test
	void addLessThanOrEqualToDate() {
		Date d = new Date();
		ListModel.addLessThanOrEqualTo(filter, "date", d);
		verify(filter).addLessThanOrEqualTo("date", d);
	}

	@Test
	void addLessThanOrEqualToInteger() {
		ListModel.addLessThanOrEqualTo(filter, "count", Integer.valueOf(10));
		verify(filter).addLessThanOrEqualTo("count", Integer.valueOf(10));
	}

	@Test
	void addLessThanOrEqualToLong() {
		ListModel.addLessThanOrEqualTo(filter, "count", Long.valueOf(20L));
		verify(filter).addLessThanOrEqualTo("count", Long.valueOf(20L));
	}

	@Test
	void addLessThanOrEqualToDecimal() {
		Decimal2 d = new Decimal2("100.00");
		ListModel.addLessThanOrEqualTo(filter, "amount", d);
		verify(filter).addLessThanOrEqualTo("amount", d);
	}

	@Test
	void addLessThanOrEqualToThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addLessThanOrEqualTo(filter, "x", new Object()));
	}

	// ---- addBetween ----

	@Test
	void addBetweenString() {
		ListModel.addBetween(filter, "name", "a", "z");
		verify(filter).addBetween("name", "a", "z");
	}

	@Test
	void addBetweenDate() {
		Date start = new Date(0);
		Date end = new Date();
		ListModel.addBetween(filter, "date", start, end);
		verify(filter).addBetween("date", start, end);
	}

	@Test
	void addBetweenInteger() {
		ListModel.addBetween(filter, "count", Integer.valueOf(1), Integer.valueOf(10));
		verify(filter).addBetween("count", Integer.valueOf(1), Integer.valueOf(10));
	}

	@Test
	void addBetweenLong() {
		ListModel.addBetween(filter, "count", Long.valueOf(1L), Long.valueOf(10L));
		verify(filter).addBetween("count", Long.valueOf(1L), Long.valueOf(10L));
	}

	@Test
	void addBetweenDecimal() {
		Decimal2 start = new Decimal2("1.00");
		Decimal2 end = new Decimal2("9.99");
		ListModel.addBetween(filter, "amount", start, end);
		verify(filter).addBetween("amount", start, end);
	}

	@Test
	void addBetweenThrowsForUnknownType() {
		assertThrows(IllegalArgumentException.class, () -> ListModel.addBetween(filter, "x", new Object(), new Object()));
	}
}

package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;

/**
 * Tests for the static type-dispatch methods of {@link ListModel}.
 */
@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class ListModelStaticMethodTest {

	// ===== addEquals =====

	@Test
	void addEqualsWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "text", "hello");
		verify(filter).addEquals("text", "hello");
	}

	@Test
	void addEqualsWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addEquals(filter, "date", date);
		verify(filter).addEquals("date", date);
	}

	@Test
	void addEqualsWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "normalInteger", Integer.valueOf(42));
		verify(filter).addEquals("normalInteger", Integer.valueOf(42));
	}

	@Test
	void addEqualsWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "longInteger", Long.valueOf(123L));
		verify(filter).addEquals("longInteger", Long.valueOf(123L));
	}

	@Test
	void addEqualsWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal2 decimal = new Decimal2("1.23");
		ListModel.addEquals(filter, "decimal2", decimal);
		verify(filter).addEquals("decimal2", decimal);
	}

	@Test
	void addEqualsWithBoolean() {
		Filter filter = mock(Filter.class);
		ListModel.addEquals(filter, "booleanFlag", Boolean.TRUE);
		verify(filter).addEquals("booleanFlag", Boolean.TRUE);
	}

	@Test
	void addEqualsWithEnum() {
		Filter filter = mock(Filter.class);
		TestEnum value = TestEnum.VALUE_A;
		ListModel.addEquals(filter, "enumField", value);
		verify(filter).addEquals("enumField", value);
	}

	@Test
	void addEqualsWithGeometry() {
		Filter filter = mock(Filter.class);
		Geometry geometry = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(0, 0));
		ListModel.addEquals(filter, "geometry", geometry);
		verify(filter).addEquals("geometry", geometry);
	}

	@Test
	void addEqualsWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class, () -> ListModel.addEquals(filter, "field", value));
	}

	// ===== addNotEquals =====

	@Test
	void addNotEqualsWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "text", "hello");
		verify(filter).addNotEquals("text", "hello");
	}

	@Test
	void addNotEqualsWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addNotEquals(filter, "date", date);
		verify(filter).addNotEquals("date", date);
	}

	@Test
	void addNotEqualsWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "normalInteger", Integer.valueOf(42));
		verify(filter).addNotEquals("normalInteger", Integer.valueOf(42));
	}

	@Test
	void addNotEqualsWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "longInteger", Long.valueOf(123L));
		verify(filter).addNotEquals("longInteger", Long.valueOf(123L));
	}

	@Test
	void addNotEqualsWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal5 decimal = new Decimal5("2.50000");
		ListModel.addNotEquals(filter, "decimal5", decimal);
		verify(filter).addNotEquals("decimal5", decimal);
	}

	@Test
	void addNotEqualsWithBoolean() {
		Filter filter = mock(Filter.class);
		ListModel.addNotEquals(filter, "booleanFlag", Boolean.FALSE);
		verify(filter).addNotEquals("booleanFlag", Boolean.FALSE);
	}

	@Test
	void addNotEqualsWithEnum() {
		Filter filter = mock(Filter.class);
		TestEnum value = TestEnum.VALUE_B;
		ListModel.addNotEquals(filter, "enumField", value);
		verify(filter).addNotEquals("enumField", value);
	}

	@Test
	void addNotEqualsWithGeometry() {
		Filter filter = mock(Filter.class);
		Geometry geometry = new GeometryFactory().createPoint(new org.locationtech.jts.geom.Coordinate(1, 2));
		ListModel.addNotEquals(filter, "geometry", geometry);
		verify(filter).addNotEquals("geometry", geometry);
	}

	@Test
	void addNotEqualsWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		Object value = new Object();
		assertThrows(IllegalArgumentException.class, () -> ListModel.addNotEquals(filter, "field", value));
	}

	// ===== addGreaterThan =====

	@Test
	void addGreaterThanWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "text", "a");
		verify(filter).addGreaterThan("text", "a");
	}

	@Test
	void addGreaterThanWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addGreaterThan(filter, "date", date);
		verify(filter).addGreaterThan("date", date);
	}

	@Test
	void addGreaterThanWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "normalInteger", Integer.valueOf(5));
		verify(filter).addGreaterThan("normalInteger", Integer.valueOf(5));
	}

	@Test
	void addGreaterThanWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThan(filter, "longInteger", Long.valueOf(10L));
		verify(filter).addGreaterThan("longInteger", Long.valueOf(10L));
	}

	@Test
	void addGreaterThanWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal10 decimal = new Decimal10("100.0000000000");
		ListModel.addGreaterThan(filter, "decimal10", decimal);
		verify(filter).addGreaterThan("decimal10", decimal);
	}

	@Test
	void addGreaterThanWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		assertThrows(IllegalArgumentException.class, () -> ListModel.addGreaterThan(filter, "field", Boolean.TRUE));
	}

	// ===== addGreaterThanOrEqualTo =====

	@Test
	void addGreaterThanOrEqualToWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "text", "a");
		verify(filter).addGreaterThanOrEqualTo("text", "a");
	}

	@Test
	void addGreaterThanOrEqualToWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addGreaterThanOrEqualTo(filter, "date", date);
		verify(filter).addGreaterThanOrEqualTo("date", date);
	}

	@Test
	void addGreaterThanOrEqualToWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "normalInteger", Integer.valueOf(5));
		verify(filter).addGreaterThanOrEqualTo("normalInteger", Integer.valueOf(5));
	}

	@Test
	void addGreaterThanOrEqualToWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addGreaterThanOrEqualTo(filter, "longInteger", Long.valueOf(10L));
		verify(filter).addGreaterThanOrEqualTo("longInteger", Long.valueOf(10L));
	}

	@Test
	void addGreaterThanOrEqualToWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal2 decimal = new Decimal2("5.00");
		ListModel.addGreaterThanOrEqualTo(filter, "decimal2", decimal);
		verify(filter).addGreaterThanOrEqualTo("decimal2", decimal);
	}

	@Test
	void addGreaterThanOrEqualToWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		assertThrows(IllegalArgumentException.class, () -> ListModel.addGreaterThanOrEqualTo(filter, "field", Boolean.TRUE));
	}

	// ===== addLessThan =====

	@Test
	void addLessThanWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "text", "z");
		verify(filter).addLessThan("text", "z");
	}

	@Test
	void addLessThanWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addLessThan(filter, "date", date);
		verify(filter).addLessThan("date", date);
	}

	@Test
	void addLessThanWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "normalInteger", Integer.valueOf(100));
		verify(filter).addLessThan("normalInteger", Integer.valueOf(100));
	}

	@Test
	void addLessThanWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThan(filter, "longInteger", Long.valueOf(200L));
		verify(filter).addLessThan("longInteger", Long.valueOf(200L));
	}

	@Test
	void addLessThanWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal2 decimal = new Decimal2("9.99");
		ListModel.addLessThan(filter, "decimal2", decimal);
		verify(filter).addLessThan("decimal2", decimal);
	}

	@Test
	void addLessThanWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		assertThrows(IllegalArgumentException.class, () -> ListModel.addLessThan(filter, "field", Boolean.TRUE));
	}

	// ===== addLessThanOrEqualTo =====

	@Test
	void addLessThanOrEqualToWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "text", "z");
		verify(filter).addLessThanOrEqualTo("text", "z");
	}

	@Test
	void addLessThanOrEqualToWithDate() {
		Filter filter = mock(Filter.class);
		Date date = new Date();
		ListModel.addLessThanOrEqualTo(filter, "date", date);
		verify(filter).addLessThanOrEqualTo("date", date);
	}

	@Test
	void addLessThanOrEqualToWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "normalInteger", Integer.valueOf(100));
		verify(filter).addLessThanOrEqualTo("normalInteger", Integer.valueOf(100));
	}

	@Test
	void addLessThanOrEqualToWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addLessThanOrEqualTo(filter, "longInteger", Long.valueOf(200L));
		verify(filter).addLessThanOrEqualTo("longInteger", Long.valueOf(200L));
	}

	@Test
	void addLessThanOrEqualToWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal2 decimal = new Decimal2("9.99");
		ListModel.addLessThanOrEqualTo(filter, "decimal2", decimal);
		verify(filter).addLessThanOrEqualTo("decimal2", decimal);
	}

	@Test
	void addLessThanOrEqualToWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		assertThrows(IllegalArgumentException.class, () -> ListModel.addLessThanOrEqualTo(filter, "field", Boolean.TRUE));
	}

	// ===== addBetween =====

	@Test
	void addBetweenWithString() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "text", "a", "z");
		verify(filter).addBetween("text", "a", "z");
	}

	@Test
	void addBetweenWithDate() {
		Filter filter = mock(Filter.class);
		Date start = new Date(0L);
		Date end = new Date();
		ListModel.addBetween(filter, "date", start, end);
		verify(filter).addBetween("date", start, end);
	}

	@Test
	void addBetweenWithInteger() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "normalInteger", Integer.valueOf(1), Integer.valueOf(10));
		verify(filter).addBetween("normalInteger", Integer.valueOf(1), Integer.valueOf(10));
	}

	@Test
	void addBetweenWithLong() {
		Filter filter = mock(Filter.class);
		ListModel.addBetween(filter, "longInteger", Long.valueOf(1L), Long.valueOf(100L));
		verify(filter).addBetween("longInteger", Long.valueOf(1L), Long.valueOf(100L));
	}

	@Test
	void addBetweenWithDecimal() {
		Filter filter = mock(Filter.class);
		Decimal2 start = new Decimal2("1.00");
		Decimal2 end = new Decimal2("9.99");
		ListModel.addBetween(filter, "decimal2", start, end);
		verify(filter).addBetween("decimal2", start, end);
	}

	@Test
	void addBetweenWithUnsupportedTypeThrows() {
		Filter filter = mock(Filter.class);
		assertThrows(IllegalArgumentException.class, () -> ListModel.addBetween(filter, "field", Boolean.TRUE, Boolean.FALSE));
	}

	// ===== helper =====

	private enum TestEnum {
		VALUE_A, VALUE_B
	}
}

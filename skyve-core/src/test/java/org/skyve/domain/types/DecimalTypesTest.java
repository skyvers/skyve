package org.skyve.domain.types;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DecimalTypesTest {

	// --- Decimal2 ---

	@Test
	void decimal2FromDoubleStoresValue() {
		Decimal2 d = new Decimal2(1.5);
		assertTrue(1.5 == d.doubleValue());
	}

	@Test
	void decimal2FromStringStoresValue() {
		Decimal2 d = new Decimal2("2.50");
		assertThat(d.toString(), is("2.50"));
	}

	@Test
	void decimal2FromBigDecimalStoresValue() {
		Decimal2 d = new Decimal2(new BigDecimal("3.14"));
		assertThat(d.toString(), is("3.14"));
	}

	@Test
	void decimal2FromDecimalStoresValue() {
		Decimal2 source = new Decimal2(4.0);
		Decimal2 d = new Decimal2(source);
		assertTrue(4.0 == d.doubleValue());
	}

	@Test
	void decimal2AddReturnsCorrectResult() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		Decimal2 result = a.add(b);
		assertTrue(3.0 == result.doubleValue());
	}

	@Test
	void decimal2SubtractReturnsCorrectResult() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		Decimal2 result = a.subtract(b);
		assertTrue(3.0 == result.doubleValue());
	}

	@Test
	void decimal2MultiplyReturnsCorrectResult() {
		Decimal2 a = new Decimal2(3.0);
		Decimal2 b = new Decimal2(4.0);
		Decimal2 result = a.multiply(b);
		assertTrue(12.0 == result.doubleValue());
	}

	@Test
	void decimal2DivideReturnsCorrectResult() {
		Decimal2 a = new Decimal2(10.0);
		Decimal2 b = new Decimal2(4.0);
		Decimal2 result = a.divide(b);
		assertTrue(2.5 == result.doubleValue());
	}

	@Test
	void decimal2AbsReturnsPositive() {
		Decimal2 d = new Decimal2(-3.5);
		Decimal2 result = d.abs();
		assertTrue(3.5 == result.doubleValue());
	}

	@Test
	void decimal2NegateChangesSign() {
		Decimal2 d = new Decimal2(5.0);
		Decimal2 result = d.negate();
		assertTrue(-5.0 == result.doubleValue());
	}

	@Test
	void decimal2PowSquared() {
		Decimal2 d = new Decimal2(3.0);
		Decimal2 result = d.pow(2);
		assertTrue(9.0 == result.doubleValue());
	}

	@Test
	void decimal2MinReturnsSmaller() {
		Decimal2 a = new Decimal2(2.0);
		Decimal2 b = new Decimal2(5.0);
		assertTrue(2.0 == a.min(b).doubleValue());
	}

	@Test
	void decimal2MaxReturnsLarger() {
		Decimal2 a = new Decimal2(2.0);
		Decimal2 b = new Decimal2(5.0);
		assertTrue(5.0 == a.max(b).doubleValue());
	}

	@Test
	void decimal2ScaleIsTwo() {
		Decimal2 d = new Decimal2(1.0);
		assertEquals(2, d.scale());
	}

	@Test
	void decimal2CompareToLess() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		assertTrue(a.compareTo(b) < 0);
	}

	@Test
	void decimal2CompareToEqual() {
		Decimal2 a = new Decimal2(3.0);
		Decimal2 b = new Decimal2(3.0);
		assertEquals(0, a.compareTo(b));
	}

	@Test
	void decimal2CompareToGreater() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		assertTrue(a.compareTo(b) > 0);
	}

	@Test
	void decimal2LessThan() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		assertTrue(a.lessThan(b));
	}

	@Test
	void decimal2GreaterThan() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		assertTrue(a.greaterThan(b));
	}

	@Test
	void decimal2IntValue() {
		Decimal2 d = new Decimal2(7.9);
		assertEquals(7, d.intValue());
	}

	@Test
	void decimal2LongValue() {
		Decimal2 d = new Decimal2(42.0);
		assertEquals(42L, d.longValue());
	}

	@Test
	void decimal2Constants() {
		assertTrue(0.0 == Decimal2.ZERO.doubleValue());
		assertTrue(1.0 == Decimal2.ONE.doubleValue());
		assertTrue(10.0 == Decimal2.TEN.doubleValue());
		assertTrue(100.0 == Decimal2.ONE_HUNDRED.doubleValue());
		assertTrue(1000.0 == Decimal2.ONE_THOUSAND.doubleValue());
	}

	@Test
	void decimal2BigDecimalValue() {
		Decimal2 d = new Decimal2(1.5);
		assertThat(d.bigDecimalValue(), notNullValue());
	}

	@Test
	void decimal2Signum() {
		assertEquals(-1, new Decimal2(-1.0).signum());
		assertEquals(0, new Decimal2(0.0).signum());
		assertEquals(1, new Decimal2(1.0).signum());
	}

	// --- Decimal5 ---

	@Test
	void decimal5FromDoubleStoresValue() {
		Decimal5 d = new Decimal5(1.5);
		assertTrue(1.5 == d.doubleValue());
	}

	@Test
	void decimal5ScaleIsFive() {
		Decimal5 d = new Decimal5(1.0);
		assertEquals(5, d.scale());
	}

	@Test
	void decimal5AddReturnsCorrectResult() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(2.0);
		Decimal5 result = a.add(b);
		assertTrue(3.0 == result.doubleValue());
	}

	@Test
	void decimal5SubtractReturnsCorrectResult() {
		Decimal5 a = new Decimal5(10.0);
		Decimal5 b = new Decimal5(3.5);
		Decimal5 result = a.subtract(b);
		assertTrue(6.5 == result.doubleValue());
	}

	@Test
	void decimal5MultiplyReturnsCorrectResult() {
		Decimal5 a = new Decimal5(2.5);
		Decimal5 b = new Decimal5(4.0);
		Decimal5 result = a.multiply(b);
		assertTrue(10.0 == result.doubleValue());
	}

	@Test
	void decimal5DivideReturnsCorrectResult() {
		Decimal5 a = new Decimal5(9.0);
		Decimal5 b = new Decimal5(3.0);
		Decimal5 result = a.divide(b);
		assertTrue(3.0 == result.doubleValue());
	}

	@Test
	void decimal5AbsPositive() {
		Decimal5 d = new Decimal5(-4.0);
		assertTrue(4.0 == d.abs().doubleValue());
	}

	@Test
	void decimal5NegateChangesSign() {
		Decimal5 d = new Decimal5(3.0);
		assertTrue(-3.0 == d.negate().doubleValue());
	}

	@Test
	void decimal5Constants() {
		assertTrue(0.0 == Decimal5.ZERO.doubleValue());
		assertTrue(1.0 == Decimal5.ONE.doubleValue());
		assertTrue(60.0 == Decimal5.SIXTY.doubleValue());
		assertTrue(100.0 == Decimal5.ONE_HUNDRED.doubleValue());
	}

	@Test
	void decimal5MinReturnsSmaller() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(3.0);
		assertTrue(1.0 == a.min(b).doubleValue());
	}

	@Test
	void decimal5MaxReturnsLarger() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(3.0);
		assertTrue(3.0 == a.max(b).doubleValue());
	}

	// --- Decimal10 ---

	@Test
	void decimal10FromDoubleStoresValue() {
		Decimal10 d = new Decimal10(1.5);
		assertTrue(1.5 == d.doubleValue());
	}

	@Test
	void decimal10ScaleIsTen() {
		Decimal10 d = new Decimal10(1.0);
		assertEquals(10, d.scale());
	}

	@Test
	void decimal10AddReturnsCorrectResult() {
		Decimal10 a = new Decimal10(1.0);
		Decimal10 b = new Decimal10(2.0);
		Decimal10 result = a.add(b);
		assertTrue(3.0 == result.doubleValue());
	}

	@Test
	void decimal10Constants() {
		assertTrue(0.0 == Decimal10.ZERO.doubleValue());
		assertTrue(1.0 == Decimal10.ONE.doubleValue());
		assertTrue(100.0 == Decimal10.ONE_HUNDRED.doubleValue());
	}

	@Test
	void decimal10AbsPositive() {
		Decimal10 d = new Decimal10(-7.5);
		assertTrue(7.5 == d.abs().doubleValue());
	}

	@Test
	void decimal10NegateChangesSign() {
		Decimal10 d = new Decimal10(6.0);
		assertTrue(-6.0 == d.negate().doubleValue());
	}

	@Test
	void decimal10MinReturnsSmaller() {
		Decimal10 a = new Decimal10(2.0);
		Decimal10 b = new Decimal10(8.0);
		assertTrue(2.0 == a.min(b).doubleValue());
	}

	@Test
	void decimal10MaxReturnsLarger() {
		Decimal10 a = new Decimal10(2.0);
		Decimal10 b = new Decimal10(8.0);
		assertTrue(8.0 == a.max(b).doubleValue());
	}

	// --- Cross-type comparisons ---

	@Test
	void decimal2EqualsDecimal2SameValue() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(5.0);
		assertTrue(a.equals(b));
	}

	@Test
	void decimal2NotEqualsDecimal2DifferentValue() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(6.0);
		assertFalse(a.equals(b));
	}

	@Test
	void decimal5LessThanOrEqual() {
		Decimal5 a = new Decimal5(3.0);
		Decimal5 b = new Decimal5(3.0);
		assertTrue(a.lessThanOrEqual(b));
	}

	@Test
	void decimal5GreaterThanOrEqual() {
		Decimal5 a = new Decimal5(5.0);
		Decimal5 b = new Decimal5(5.0);
		assertTrue(a.greaterThanOrEqual(b));
	}

	// ---- Decimal base class uncovered paths ----

	@Test
	void decimal2FloatValueReturnsFloat() {
		Decimal2 d = new Decimal2(3.5);
		assertEquals(3.5f, d.floatValue(), 0.0f);
	}

	@Test
	@SuppressWarnings("unlikely-arg-type")
	void decimal2EqualsBigDecimalReturnsTrue() {
		Decimal2 d = new Decimal2("2.50");
		assertTrue(d.equals(new BigDecimal("2.50")));
	}

	@Test
	@SuppressWarnings("unlikely-arg-type")
	void decimal2EqualsNonDecimalReturnsFalse() {
		Decimal2 d = new Decimal2(1.0);
		assertFalse(d.equals("1.0"));
	}

	@Test
	void decimal2HashCodeIsConsistent() {
		Decimal2 d = new Decimal2(4.0);
		assertEquals(d.hashCode(), d.hashCode());
	}

	@Test
	void decimal2ApproximatelyReturnsTrueWithinTolerance() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(1.0001);
		assertTrue(a.approximately(b, 0.01));
	}

	@Test
	void decimal2ApproximatelyReturnsFalseOutsideTolerance() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		assertFalse(a.approximately(b, 0.01));
	}

	@Test
	void decimal2CompareToBigDecimalReturnsZeroForEqual() {
		Decimal2 d = new Decimal2("3.00");
		assertEquals(0, d.compareTo(new BigDecimal("3.00")));
	}

	@Test
	void decimal2CompareToIllegalTypeThrows() {
		Decimal2 d = new Decimal2(1.0);
		assertThrows(IllegalArgumentException.class, () -> d.compareTo(Integer.valueOf(1)));
	}
}

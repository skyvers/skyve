package org.skyve.domain.types;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DecimalTypesTest {

	// --- Decimal2 ---

	@Test
	void decimal2FromDoubleStoresValue() {
		Decimal2 d = new Decimal2(1.5);
		assertThat(d.doubleValue(), is(1.5));
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
		assertThat(d.doubleValue(), is(4.0));
	}

	@Test
	void decimal2AddReturnsCorrectResult() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		Decimal2 result = (Decimal2) a.add(b);
		assertThat(result.doubleValue(), is(3.0));
	}

	@Test
	void decimal2SubtractReturnsCorrectResult() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		Decimal2 result = (Decimal2) a.subtract(b);
		assertThat(result.doubleValue(), is(3.0));
	}

	@Test
	void decimal2MultiplyReturnsCorrectResult() {
		Decimal2 a = new Decimal2(3.0);
		Decimal2 b = new Decimal2(4.0);
		Decimal2 result = (Decimal2) a.multiply(b);
		assertThat(result.doubleValue(), is(12.0));
	}

	@Test
	void decimal2DivideReturnsCorrectResult() {
		Decimal2 a = new Decimal2(10.0);
		Decimal2 b = new Decimal2(4.0);
		Decimal2 result = (Decimal2) a.divide(b);
		assertThat(result.doubleValue(), is(2.5));
	}

	@Test
	void decimal2AbsReturnsPositive() {
		Decimal2 d = new Decimal2(-3.5);
		Decimal2 result = (Decimal2) d.abs();
		assertThat(result.doubleValue(), is(3.5));
	}

	@Test
	void decimal2NegateChangesSign() {
		Decimal2 d = new Decimal2(5.0);
		Decimal2 result = (Decimal2) d.negate();
		assertThat(result.doubleValue(), is(-5.0));
	}

	@Test
	void decimal2PowSquared() {
		Decimal2 d = new Decimal2(3.0);
		Decimal2 result = (Decimal2) d.pow(2);
		assertThat(result.doubleValue(), is(9.0));
	}

	@Test
	void decimal2MinReturnsSmaller() {
		Decimal2 a = new Decimal2(2.0);
		Decimal2 b = new Decimal2(5.0);
		assertThat(((Decimal2) a.min(b)).doubleValue(), is(2.0));
	}

	@Test
	void decimal2MaxReturnsLarger() {
		Decimal2 a = new Decimal2(2.0);
		Decimal2 b = new Decimal2(5.0);
		assertThat(((Decimal2) a.max(b)).doubleValue(), is(5.0));
	}

	@Test
	void decimal2ScaleIsTwo() {
		Decimal2 d = new Decimal2(1.0);
		assertThat(d.scale(), is(2));
	}

	@Test
	void decimal2CompareToLess() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		assertThat(a.compareTo(b) < 0, is(true));
	}

	@Test
	void decimal2CompareToEqual() {
		Decimal2 a = new Decimal2(3.0);
		Decimal2 b = new Decimal2(3.0);
		assertThat(a.compareTo(b), is(0));
	}

	@Test
	void decimal2CompareToGreater() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		assertThat(a.compareTo(b) > 0, is(true));
	}

	@Test
	void decimal2LessThan() {
		Decimal2 a = new Decimal2(1.0);
		Decimal2 b = new Decimal2(2.0);
		assertThat(a.lessThan(b), is(true));
	}

	@Test
	void decimal2GreaterThan() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(2.0);
		assertThat(a.greaterThan(b), is(true));
	}

	@Test
	void decimal2IntValue() {
		Decimal2 d = new Decimal2(7.9);
		assertThat(d.intValue(), is(7));
	}

	@Test
	void decimal2LongValue() {
		Decimal2 d = new Decimal2(42.0);
		assertThat(d.longValue(), is(42L));
	}

	@Test
	void decimal2Constants() {
		assertThat(Decimal2.ZERO.doubleValue(), is(0.0));
		assertThat(Decimal2.ONE.doubleValue(), is(1.0));
		assertThat(Decimal2.TEN.doubleValue(), is(10.0));
		assertThat(Decimal2.ONE_HUNDRED.doubleValue(), is(100.0));
		assertThat(Decimal2.ONE_THOUSAND.doubleValue(), is(1000.0));
	}

	@Test
	void decimal2BigDecimalValue() {
		Decimal2 d = new Decimal2(1.5);
		assertThat(d.bigDecimalValue(), notNullValue());
	}

	@Test
	void decimal2Signum() {
		assertThat(new Decimal2(-1.0).signum(), is(-1));
		assertThat(new Decimal2(0.0).signum(), is(0));
		assertThat(new Decimal2(1.0).signum(), is(1));
	}

	// --- Decimal5 ---

	@Test
	void decimal5FromDoubleStoresValue() {
		Decimal5 d = new Decimal5(1.5);
		assertThat(d.doubleValue(), is(1.5));
	}

	@Test
	void decimal5ScaleIsFive() {
		Decimal5 d = new Decimal5(1.0);
		assertThat(d.scale(), is(5));
	}

	@Test
	void decimal5AddReturnsCorrectResult() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(2.0);
		Decimal5 result = (Decimal5) a.add(b);
		assertThat(result.doubleValue(), is(3.0));
	}

	@Test
	void decimal5SubtractReturnsCorrectResult() {
		Decimal5 a = new Decimal5(10.0);
		Decimal5 b = new Decimal5(3.5);
		Decimal5 result = (Decimal5) a.subtract(b);
		assertThat(result.doubleValue(), is(6.5));
	}

	@Test
	void decimal5MultiplyReturnsCorrectResult() {
		Decimal5 a = new Decimal5(2.5);
		Decimal5 b = new Decimal5(4.0);
		Decimal5 result = (Decimal5) a.multiply(b);
		assertThat(result.doubleValue(), is(10.0));
	}

	@Test
	void decimal5DivideReturnsCorrectResult() {
		Decimal5 a = new Decimal5(9.0);
		Decimal5 b = new Decimal5(3.0);
		Decimal5 result = (Decimal5) a.divide(b);
		assertThat(result.doubleValue(), is(3.0));
	}

	@Test
	void decimal5AbsPositive() {
		Decimal5 d = new Decimal5(-4.0);
		assertThat(((Decimal5) d.abs()).doubleValue(), is(4.0));
	}

	@Test
	void decimal5NegateChangesSign() {
		Decimal5 d = new Decimal5(3.0);
		assertThat(((Decimal5) d.negate()).doubleValue(), is(-3.0));
	}

	@Test
	void decimal5Constants() {
		assertThat(Decimal5.ZERO.doubleValue(), is(0.0));
		assertThat(Decimal5.ONE.doubleValue(), is(1.0));
		assertThat(Decimal5.SIXTY.doubleValue(), is(60.0));
		assertThat(Decimal5.ONE_HUNDRED.doubleValue(), is(100.0));
	}

	@Test
	void decimal5MinReturnsSmaller() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(3.0);
		assertThat(((Decimal5) a.min(b)).doubleValue(), is(1.0));
	}

	@Test
	void decimal5MaxReturnsLarger() {
		Decimal5 a = new Decimal5(1.0);
		Decimal5 b = new Decimal5(3.0);
		assertThat(((Decimal5) a.max(b)).doubleValue(), is(3.0));
	}

	// --- Decimal10 ---

	@Test
	void decimal10FromDoubleStoresValue() {
		Decimal10 d = new Decimal10(1.5);
		assertThat(d.doubleValue(), is(1.5));
	}

	@Test
	void decimal10ScaleIsTen() {
		Decimal10 d = new Decimal10(1.0);
		assertThat(d.scale(), is(10));
	}

	@Test
	void decimal10AddReturnsCorrectResult() {
		Decimal10 a = new Decimal10(1.0);
		Decimal10 b = new Decimal10(2.0);
		Decimal10 result = (Decimal10) a.add(b);
		assertThat(result.doubleValue(), is(3.0));
	}

	@Test
	void decimal10Constants() {
		assertThat(Decimal10.ZERO.doubleValue(), is(0.0));
		assertThat(Decimal10.ONE.doubleValue(), is(1.0));
		assertThat(Decimal10.ONE_HUNDRED.doubleValue(), is(100.0));
	}

	@Test
	void decimal10AbsPositive() {
		Decimal10 d = new Decimal10(-7.5);
		assertThat(((Decimal10) d.abs()).doubleValue(), is(7.5));
	}

	@Test
	void decimal10NegateChangesSign() {
		Decimal10 d = new Decimal10(6.0);
		assertThat(((Decimal10) d.negate()).doubleValue(), is(-6.0));
	}

	@Test
	void decimal10MinReturnsSmaller() {
		Decimal10 a = new Decimal10(2.0);
		Decimal10 b = new Decimal10(8.0);
		assertThat(((Decimal10) a.min(b)).doubleValue(), is(2.0));
	}

	@Test
	void decimal10MaxReturnsLarger() {
		Decimal10 a = new Decimal10(2.0);
		Decimal10 b = new Decimal10(8.0);
		assertThat(((Decimal10) a.max(b)).doubleValue(), is(8.0));
	}

	// --- Cross-type comparisons ---

	@Test
	void decimal2EqualsDecimal2SameValue() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(5.0);
		assertThat(a.equals(b), is(true));
	}

	@Test
	void decimal2NotEqualsDecimal2DifferentValue() {
		Decimal2 a = new Decimal2(5.0);
		Decimal2 b = new Decimal2(6.0);
		assertThat(a.equals(b), is(false));
	}

	@Test
	void decimal5LessThanOrEqual() {
		Decimal5 a = new Decimal5(3.0);
		Decimal5 b = new Decimal5(3.0);
		assertThat(a.lessThanOrEqual(b), is(true));
	}

	@Test
	void decimal5GreaterThanOrEqual() {
		Decimal5 a = new Decimal5(5.0);
		Decimal5 b = new Decimal5(5.0);
		assertThat(a.greaterThanOrEqual(b), is(true));
	}
}

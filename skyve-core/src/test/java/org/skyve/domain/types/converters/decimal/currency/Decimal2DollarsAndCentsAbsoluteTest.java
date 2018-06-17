package org.skyve.domain.types.converters.decimal.currency;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.Decimal2;

public class Decimal2DollarsAndCentsAbsoluteTest {

	private Decimal2DollarsAndCentsAbsolute formatter;

	@Before
	public void before() {
		formatter = new Decimal2DollarsAndCentsAbsolute();
	}

	/**
	 * Tests displaying of a positive value.
	 */
	@Test
	public void testPositiveToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(+2.5555)), is("2.56"));
	}

	/**
	 * Tests displaying of a negative value.
	 */
	@Test
	public void testNegativeToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(-0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-2.5555)), is("2.56"));
	}
}

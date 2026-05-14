package org.skyve.impl.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class BucketMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	public void textStartsWithDefaultConstructorHasZeroLengthAndFalse() {
		TextStartsWithBucketMetaData b = new TextStartsWithBucketMetaData();
		assertEquals(0, b.getLength());
		assertFalse(b.isCaseSensitive());
	}

	@Test
	@SuppressWarnings("static-method")
	public void textStartsWithSetLengthRoundtrip() {
		TextStartsWithBucketMetaData b = new TextStartsWithBucketMetaData();
		b.setLength(3);
		assertEquals(3, b.getLength());
	}

	@Test
	@SuppressWarnings("static-method")
	public void textStartsWithSetCaseSensitiveRoundtrip() {
		TextStartsWithBucketMetaData b = new TextStartsWithBucketMetaData();
		b.setCaseSensitive(true);
		assertTrue(b.isCaseSensitive());
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericRangeBucketDefaultConstructorHasEmptyRanges() {
		NumericRangeBucketMetaData b = new NumericRangeBucketMetaData();
		assertNotNull(b.getRanges());
		assertTrue(b.getRanges().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericRangeBucketBizQLExpressionWithSingleRange() {
		NumericRangeBucketMetaData b = new NumericRangeBucketMetaData();
		NumericRangeMetaData range = new NumericRangeMetaData();
		range.setRange(100);
		b.getRanges().add(range);
		b.convert();
		assertThat(b.bizQLExpression("amount"), is("case when bean.amount < 100 then 0 else 1 end"));
	}

	private static void assertEquals(int expected, int actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual);
	}
}

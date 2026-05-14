package org.skyve.impl.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.NumericMultipleBucket;
import org.skyve.metadata.view.model.chart.TemporalBucket.TemporalBucketType;

public class TemporalAndNumericMultipleBucketMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	public void temporalBucketDefaultTypeIsDay() {
		TemporalBucketMetaData b = new TemporalBucketMetaData();
		assertThat(b.getType(), is(TemporalBucketType.day));
	}

	@Test
	@SuppressWarnings("static-method")
	public void temporalBucketSetTypeRoundtrip() {
		TemporalBucketMetaData b = new TemporalBucketMetaData();
		b.setType(TemporalBucketType.month);
		assertThat(b.getType(), is(TemporalBucketType.month));
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericMultipleBucketDefaultMultipleIsZero() {
		NumericMultipleBucketMetaData b = new NumericMultipleBucketMetaData();
		assertEquals(0, b.getMultiple());
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericMultipleBucketSetMultipleRoundtrip() {
		NumericMultipleBucketMetaData b = new NumericMultipleBucketMetaData();
		b.setMultiple(10);
		assertEquals(10, b.getMultiple());
	}

	// ---- NumericMultipleBucket (the runtime class) ----

	@Test
	@SuppressWarnings("static-method")
	public void numericMultipleBucketBizQLExpressionContainsBinding() {
		NumericMultipleBucket b = new NumericMultipleBucket(10);
		String expr = b.bizQLExpression("salary");
		assertTrue(expr.contains("salary"));
		assertTrue(expr.contains("10"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericMultipleBucketLabelFormatsRange() {
		NumericMultipleBucket b = new NumericMultipleBucket(10);
		String label = b.label(Integer.valueOf(3));
		assertEquals("30-40", label);
	}

	@Test
	@SuppressWarnings("static-method")
	public void numericMultipleBucketLabelReturnsNullForNonNumber() {
		NumericMultipleBucket b = new NumericMultipleBucket(10);
		assertNull(b.label("not-a-number"));
	}
}

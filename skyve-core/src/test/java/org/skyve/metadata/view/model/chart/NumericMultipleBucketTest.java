package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NumericMultipleBucketTest {

	// ---- bizQLExpression ----

	@Test
	void bizQLExpressionContainsFloorExpressionWithBinding() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(10);
		String expr = bucket.bizQLExpression("amount");
		assertTrue(expr.contains("floor(bean.amount / 10.0 + 0.001)"));
	}

	@Test
	void bizQLExpressionWithDifferentMultiple() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(5);
		String expr = bucket.bizQLExpression("score");
		assertTrue(expr.contains("5.0"));
		assertTrue(expr.contains("bean.score"));
	}

	// ---- label ----

	@Test
	void labelWithNumberReturnsFormattedRange() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(10);
		// category = 2 → range "20-30"
		String result = bucket.label(Integer.valueOf(2));
		assertThat(result, is("20-30"));
	}

	@Test
	void labelWithZeroReturnsFirstRange() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(10);
		String result = bucket.label(Integer.valueOf(0));
		assertThat(result, is("0-10"));
	}

	@Test
	void labelWithNonNumberReturnsNull() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(10);
		assertNull(bucket.label("notANumber"));
	}

	@Test
	void labelWithNullReturnsNull() {
		NumericMultipleBucket bucket = new NumericMultipleBucket(10);
		assertNull(bucket.label(null));
	}
}

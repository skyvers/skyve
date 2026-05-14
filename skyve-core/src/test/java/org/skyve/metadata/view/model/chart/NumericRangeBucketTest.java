package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class NumericRangeBucketTest {

	// ---- bizQLExpression tests ----

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionWithSingleRangeContainsCaseWhen() {
		NumericRangeBucket bucket = new NumericRangeBucket(10);
		String expr = bucket.bizQLExpression("score");
		assertTrue(expr.contains("case when bean.score < 10 then 0"));
		assertTrue(expr.contains("else 1 end"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionWithTwoRanges() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10);
		String expr = bucket.bizQLExpression("amount");
		assertTrue(expr.contains("case when bean.amount < 0 then 0"));
		assertTrue(expr.contains("when bean.amount between 0 and 10 then 1"));
		assertTrue(expr.contains("else 2 end"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionWithThreeRanges() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		String expr = bucket.bizQLExpression("value");
		assertTrue(expr.contains("case when bean.value < 0 then 0"));
		assertTrue(expr.contains("when bean.value between 0 and 10 then 1"));
		assertTrue(expr.contains("when bean.value between 10 and 20 then 2"));
		assertTrue(expr.contains("else 3 end"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionIncludesBindingName() {
		NumericRangeBucket bucket = new NumericRangeBucket(100);
		String expr = bucket.bizQLExpression("myField");
		assertTrue(expr.contains("bean.myField"));
	}

	// ---- label tests ----

	@Test
	@SuppressWarnings("static-method")
	public void labelNullCategoryReturnsNull() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		assertNull(bucket.label(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelNonNumberCategoryReturnsNull() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		assertNull(bucket.label("notANumber"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelIndexZeroReturnsBelowFirstRange() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		// index 0 → "<" + ranges[0] = "<0"
		assertThat(bucket.label(Integer.valueOf(0)), is("<0"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelIndexEqualToRangesLengthReturnsAboveLastRange() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		// ranges.length = 3, index 3 → ">" + ranges[2] = ">20"
		assertThat(bucket.label(Integer.valueOf(3)), is(">20"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelIndexOneReturnsBetweenFirstTwoRanges() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		// index 1 → ranges[0] + "-" + ranges[1] = "0-10"
		assertThat(bucket.label(Integer.valueOf(1)), is("0-10"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelIndexTwoReturnsBetweenSecondAndThirdRanges() {
		NumericRangeBucket bucket = new NumericRangeBucket(0, 10, 20);
		// index 2 → ranges[1] + "-" + ranges[2] = "10-20"
		assertThat(bucket.label(Integer.valueOf(2)), is("10-20"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelWorksWithNegativeRanges() {
		NumericRangeBucket bucket = new NumericRangeBucket(-10, 0, 10);
		// index 0 → "<-10"
		assertThat(bucket.label(Integer.valueOf(0)), is("<-10"));
		// index 1 → "-10-0"
		assertThat(bucket.label(Integer.valueOf(1)), is("-10-0"));
		// index 3 → ">10"
		assertThat(bucket.label(Integer.valueOf(3)), is(">10"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelWithSingleRange() {
		NumericRangeBucket bucket = new NumericRangeBucket(50);
		// index 0 → "<50"
		assertThat(bucket.label(Integer.valueOf(0)), is("<50"));
		// index 1 → ">50"
		assertThat(bucket.label(Integer.valueOf(1)), is(">50"));
	}
}

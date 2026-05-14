package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.TemporalBucket.TemporalBucketType;

public class TemporalBucketTest {

	// ---- bizQLExpression tests (pure string building, no CORE needed) ----

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionQuarterContainsConcatAndYear() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.quarter);
		String expr = bucket.bizQLExpression("myDate");
		assertTrue(expr.contains("concat"));
		assertTrue(expr.contains("year(bean.myDate)"));
		assertTrue(expr.contains("month(bean.myDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionDayMonthYearContainsDayMonthYear() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.dayMonthYear);
		String expr = bucket.bizQLExpression("createdDate");
		assertTrue(expr.contains("day(bean.createdDate)"));
		assertTrue(expr.contains("month(bean.createdDate)"));
		assertTrue(expr.contains("year(bean.createdDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionDayContainsDayFunction() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.day);
		String expr = bucket.bizQLExpression("eventDate");
		assertThat(expr, is("day(bean.eventDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionMonthContainsMonthFunction() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.month);
		String expr = bucket.bizQLExpression("createdDate");
		assertThat(expr, is("month(bean.createdDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionYearContainsYearFunction() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.year);
		String expr = bucket.bizQLExpression("orderDate");
		assertThat(expr, is("year(bean.orderDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionMonthYearContainsConcatAndBothFunctions() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.monthYear);
		String expr = bucket.bizQLExpression("myDate");
		assertTrue(expr.contains("concat"));
		assertTrue(expr.contains("year(bean.myDate)"));
		assertTrue(expr.contains("month(bean.myDate)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionHourContainsHourFunctionWithPadZero() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.hour);
		String expr = bucket.bizQLExpression("startTime");
		assertTrue(expr.contains("hour(bean.startTime)"));
		// padZero wraps in case expression
		assertTrue(expr.contains("case when"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionHourDayContainsDayAndHourFunctions() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.hourDay);
		String expr = bucket.bizQLExpression("startTime");
		assertTrue(expr.contains("day(bean.startTime)"));
		assertTrue(expr.contains("hour(bean.startTime)"));
		assertTrue(expr.contains("concat"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionHourDayMonthContainsMonthDayHour() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.hourDayMonth);
		String expr = bucket.bizQLExpression("startTime");
		assertTrue(expr.contains("month(bean.startTime)"));
		assertTrue(expr.contains("day(bean.startTime)"));
		assertTrue(expr.contains("hour(bean.startTime)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionMinuteHourContainsHourAndMinute() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.minuteHour);
		String expr = bucket.bizQLExpression("startTime");
		assertTrue(expr.contains("hour(bean.startTime)"));
		assertTrue(expr.contains("minute(bean.startTime)"));
		assertTrue(expr.contains("concat"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void bizQLExpressionSecondMinuteHourContainsHourMinuteSecond() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.secondMinuteHour);
		String expr = bucket.bizQLExpression("startTime");
		assertTrue(expr.contains("hour(bean.startTime)"));
		assertTrue(expr.contains("minute(bean.startTime)"));
		assertTrue(expr.contains("second(bean.startTime)"));
		assertTrue(expr.contains("concat"));
	}

	// ---- label tests that do not call month() (which requires CORE) ----

	@Test
	@SuppressWarnings("static-method")
	public void labelNullCategoryReturnsNull() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.day);
		assertNull(bucket.label(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelQuarterFormatsYearAndQuarter() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.quarter);
		// quarter: "YYYY-Q" → "Qn YYYY"
		assertThat(bucket.label("2024-1"), is("Q1 2024"));
		assertThat(bucket.label("2023-4"), is("Q4 2023"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelDayFormatsOrdinalSuffix() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.day);
		assertThat(bucket.label(Integer.valueOf(1)), is("1st"));
		assertThat(bucket.label(Integer.valueOf(2)), is("2nd"));
		assertThat(bucket.label(Integer.valueOf(3)), is("3rd"));
		assertThat(bucket.label(Integer.valueOf(4)), is("4th"));
		assertThat(bucket.label(Integer.valueOf(11)), is("11th"));
		assertThat(bucket.label(Integer.valueOf(21)), is("21st"));
		assertThat(bucket.label(Integer.valueOf(22)), is("22nd"));
		assertThat(bucket.label(Integer.valueOf(23)), is("23rd"));
		assertThat(bucket.label(Integer.valueOf(31)), is("31st"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelYearReturnsYearAsIs() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.year);
		assertThat(bucket.label("2024"), is("2024"));
		assertThat(bucket.label(Integer.valueOf(2023)), is("2023"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelHourAppendsColon00() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.hour);
		assertThat(bucket.label("10"), is("10:00"));
		assertThat(bucket.label(Integer.valueOf(9)), is("9:00"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelHourDayFormatsOrdinalDayAndHour() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.hourDay);
		// hourDay: "day hour" → "Nth H:00"
		assertThat(bucket.label("1 14"), is("1st 14:00"));
		assertThat(bucket.label("3 09"), is("3rd 09:00"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelMinuteHourRetainsHourAndMinute() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.minuteHour);
		// minuteHour: "H:MM" → "H:MM"
		assertThat(bucket.label("10:30"), is("10:30"));
		assertThat(bucket.label("09:05"), is("09:05"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void labelSecondMinuteHourRetainsAllThreeComponents() {
		TemporalBucket bucket = new TemporalBucket(TemporalBucketType.secondMinuteHour);
		// secondMinuteHour: "H:MM:SS" → "H:MM:SS"
		assertThat(bucket.label("10:30:45"), is("10:30:45"));
		assertThat(bucket.label("00:00:00"), is("00:00:00"));
	}
}

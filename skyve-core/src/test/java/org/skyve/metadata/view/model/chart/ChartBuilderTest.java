package org.skyve.metadata.view.model.chart;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

/**
 * Tests for {@link ChartBuilder} builder methods that don't require CORE/persistence.
 */
public class ChartBuilderTest {

	@Test
	@SuppressWarnings("static-method")
	void categoryReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.category("createdDate");
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void categoryWithBucketReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.category("createdDate", new TemporalBucket(TemporalBucket.TemporalBucketType.day));
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.value("amount");
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueWithFunctionReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.value("amount", AggregateFunction.Sum);
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void orderByReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.orderBy(OrderBy.category, SortDirection.ascending);
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void topReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.top(10, OrderBy.value, SortDirection.descending, true);
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void jFreeChartPostProcessorClassNameReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.jFreeChartPostProcessorClassName("com.example.Processor");
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void primeFacesChartPostProcessorClassNameReturnsSelf() {
		ChartBuilder builder = new ChartBuilder();
		ChartBuilder result = builder.primeFacesChartPostProcessorClassName("com.example.PfProcessor");
		assertSame(builder, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		ChartBuilder builder = new ChartBuilder();
		assertNotNull(builder);
	}
}

package org.skyve.metadata.view.model.chart;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

/**
 * Tests for {@link ChartBuilder} builder methods that don't require CORE/persistence.
 */
class ChartBuilderTest {

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

	@Test
	@SuppressWarnings("static-method")
	void labelWithNullValueReturnsOthers() {
		assertEquals("Others", ChartBuilder.label(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelWithEmptyStringReturnsOthers() {
		assertEquals("Others", ChartBuilder.label(""));
	}

	@Test
	@SuppressWarnings("static-method")
	void labelWithValueReturnsValue() {
		assertEquals("Fruits", ChartBuilder.label("Fruits"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sumPreservesBigDecimalTypeWhenAveragingRestValues() throws Exception {
		Number result = invokeSum(List.of(beanWithValue(BigDecimal.ONE)),
									List.of(beanWithValue(BigDecimal.valueOf(2)), beanWithValue(BigDecimal.valueOf(4))),
									true);

		assertEquals(BigDecimal.valueOf(3.0), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minMaxReturnsMinimumAndMaximumNonNullValues() throws Exception {
		List<Bean> beans = List.of(beanWithValue(Integer.valueOf(3)), beanWithValue(null), beanWithValue(Integer.valueOf(7)));

		assertEquals(Integer.valueOf(3), invokeMinMax(beans, true));
		assertEquals(Integer.valueOf(7), invokeMinMax(beans, false));
	}

	private static Bean beanWithValue(Number value) {
		Map<String, Object> properties = new HashMap<>();
		properties.put("value", value);
		return new DynamicBean("admin", "Thing", properties);
	}

	private static Number invokeSum(List<Bean> best, List<Bean> beans, boolean avg) throws Exception {
		Method method = ChartBuilder.class.getDeclaredMethod("sum", List.class, List.class, boolean.class);
		method.setAccessible(true);
		return (Number) method.invoke(null, best, beans, Boolean.valueOf(avg));
	}

	private static Number invokeMinMax(List<Bean> beans, boolean min) throws Exception {
		Method method = ChartBuilder.class.getDeclaredMethod("minMax", List.class, boolean.class);
		method.setAccessible(true);
		return (Number) method.invoke(null, beans, Boolean.valueOf(min));
	}
}

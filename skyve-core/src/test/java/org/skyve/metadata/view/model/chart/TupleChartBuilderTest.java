package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.metadata.view.model.chart.colours.RainbowColourSeries;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;

public class TupleChartBuilderTest {

	@Test
	@SuppressWarnings("static-method")
	public void jFreeChartPostProcessorClassNameReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		TupleChartBuilder result = builder.jFreeChartPostProcessorClassName("com.example.Processor");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void primeFacesChartPostProcessorClassNameReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		TupleChartBuilder result = builder.primeFacesChartPostProcessorClassName("com.example.Processor");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fluentChainReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		TupleChartBuilder result = builder
				.jFreeChartPostProcessorClassName("com.example.A")
				.primeFacesChartPostProcessorClassName("com.example.B");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void withDocumentQueryReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		DocumentQuery mockQuery = Mockito.mock(DocumentQuery.class);
		TupleChartBuilder result = builder.with(mockQuery);
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void withBizQLReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		BizQL mockBizQL = Mockito.mock(BizQL.class);
		TupleChartBuilder result = builder.with(mockBizQL);
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void withSQLReturnsSelf() {
		TupleChartBuilder builder = new TupleChartBuilder();
		SQL mockSQL = Mockito.mock(SQL.class);
		TupleChartBuilder result = builder.with(mockSQL);
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void buildWithLabelReturnsChartData() {
		DocumentQuery mockQuery = Mockito.mock(DocumentQuery.class);
		List<Object[]> rows = Collections.singletonList(new Object[] { "Label1", Integer.valueOf(42) });
		Mockito.when(mockQuery.tupleResults()).thenReturn(rows);
		TupleChartBuilder builder = new TupleChartBuilder().with(mockQuery);
		ChartData data = builder.build("My Label");
		assertThat(data, notNullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void buildWithTitleAndLabelReturnsChartData() {
		DocumentQuery mockQuery = Mockito.mock(DocumentQuery.class);
		List<Object[]> rows = Collections.singletonList(new Object[] { "Label1", Integer.valueOf(10) });
		Mockito.when(mockQuery.tupleResults()).thenReturn(rows);
		TupleChartBuilder builder = new TupleChartBuilder().with(mockQuery);
		ChartData data = builder.build("Title", "Label");
		assertThat(data, notNullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void buildWithColourSeriesReturnsChartData() {
		DocumentQuery mockQuery = Mockito.mock(DocumentQuery.class);
		List<Object[]> rows = Collections.singletonList(new Object[] { "Label1", Integer.valueOf(5) });
		Mockito.when(mockQuery.tupleResults()).thenReturn(rows);
		TupleChartBuilder builder = new TupleChartBuilder().with(mockQuery);
		ChartData data = builder.build(new RainbowColourSeries(), new RainbowColourSeries(), "Label");
		assertThat(data, notNullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void buildViaSQL() {
		SQL mockSQL = Mockito.mock(SQL.class);
		List<Object[]> rows = Collections.singletonList(new Object[] { "Item", Integer.valueOf(3) });
		Mockito.when(mockSQL.tupleResults()).thenReturn(rows);
		TupleChartBuilder builder = new TupleChartBuilder().with(mockSQL);
		ChartData data = builder.build("SQL Label");
		assertThat(data, notNullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void buildViaBizQL() {
		BizQL mockBizQL = Mockito.mock(BizQL.class);
		List<Object[]> rows = Collections.singletonList(new Object[] { "BizQL", Integer.valueOf(7) });
		Mockito.when(mockBizQL.tupleResults()).thenReturn(rows);
		TupleChartBuilder builder = new TupleChartBuilder().with(mockBizQL);
		ChartData data = builder.build("BizQL Label");
		assertThat(data, notNullValue());
	}
}

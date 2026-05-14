package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.sameInstance;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
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
}

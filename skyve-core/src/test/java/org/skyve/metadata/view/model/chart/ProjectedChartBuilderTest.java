package org.skyve.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.sameInstance;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;

public class ProjectedChartBuilderTest {

	@Test
	@SuppressWarnings("static-method")
	public void categoryReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		ProjectedChartBuilder result = builder.category("cat");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		ProjectedChartBuilder result = builder.value("val");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void jFreeChartPostProcessorClassNameReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		ProjectedChartBuilder result = builder.jFreeChartPostProcessorClassName("com.example.Processor");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void primeFacesChartPostProcessorClassNameReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		ProjectedChartBuilder result = builder.primeFacesChartPostProcessorClassName("com.example.Processor");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fluentChainReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		ProjectedChartBuilder result = builder
				.category("myCategory")
				.value("myValue")
				.jFreeChartPostProcessorClassName("com.example.A")
				.primeFacesChartPostProcessorClassName("com.example.B");
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void withDocumentQueryReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		DocumentQuery mockQuery = Mockito.mock(DocumentQuery.class);
		ProjectedChartBuilder result = builder.with(mockQuery);
		assertThat(result, sameInstance(builder));
	}

	@Test
	@SuppressWarnings("static-method")
	public void withBizQLReturnsSelf() {
		ProjectedChartBuilder builder = new ProjectedChartBuilder();
		BizQL mockBizQL = Mockito.mock(BizQL.class);
		ProjectedChartBuilder result = builder.with(mockBizQL);
		assertThat(result, sameInstance(builder));
	}
}

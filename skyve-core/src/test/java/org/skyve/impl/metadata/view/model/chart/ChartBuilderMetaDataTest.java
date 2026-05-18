package org.skyve.impl.metadata.view.model.chart;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class ChartBuilderMetaDataTest {

	private ChartBuilderMetaData md;

	@BeforeEach
	void setUp() {
		md = new ChartBuilderMetaData();
	}

	// ---- default state ----

	@Test
	void defaultTitleIsNull() {
		assertNull(md.getTitle());
	}

	@Test
	void defaultLabelIsNull() {
		assertNull(md.getLabel());
	}

	@Test
	void defaultModuleNameIsNull() {
		assertNull(md.getModuleName());
	}

	@Test
	void defaultDocumentNameIsNull() {
		assertNull(md.getDocumentName());
	}

	@Test
	void defaultQueryNameIsNull() {
		assertNull(md.getQueryName());
	}

	@Test
	void defaultCategoryBindingIsNull() {
		assertNull(md.getCategoryBinding());
	}

	@Test
	void defaultCategoryBucketIsNull() {
		assertNull(md.getCategoryBucket());
	}

	@Test
	void defaultValueBindingIsNull() {
		assertNull(md.getValueBinding());
	}

	@Test
	void defaultValueFunctionIsNull() {
		assertNull(md.getValueFunction());
	}

	@Test
	void defaultTopIsNull() {
		assertNull(md.getTop());
	}

	@Test
	void defaultOrderIsNull() {
		assertNull(md.getOrder());
	}

	@Test
	void defaultJFreeChartPostProcessorClassNameIsNull() {
		assertNull(md.getJFreeChartPostProcessorClassName());
	}

	@Test
	void defaultPrimeFacesChartPostProcessorClassNameIsNull() {
		assertNull(md.getPrimeFacesChartPostProcessorClassName());
	}

	// ---- setters/getters ----

	@Test
	void setTitleRoundtrip() {
		md.setTitle("My Chart");
		assertThat(md.getTitle(), is("My Chart"));
	}

	@Test
	void setTitleNullBecomesNull() {
		md.setTitle("x");
		md.setTitle(null);
		assertNull(md.getTitle());
	}

	@Test
	void setLabelRoundtrip() {
		md.setLabel("Count");
		assertThat(md.getLabel(), is("Count"));
	}

	@Test
	void setModuleNameRoundtrip() {
		md.setModuleName("admin");
		assertThat(md.getModuleName(), is("admin"));
	}

	@Test
	void setDocumentNameRoundtrip() {
		md.setDocumentName("User");
		assertThat(md.getDocumentName(), is("User"));
	}

	@Test
	void setQueryNameRoundtrip() {
		md.setQueryName("qUsers");
		assertThat(md.getQueryName(), is("qUsers"));
	}

	@Test
	void setCategoryBindingRoundtrip() {
		md.setCategoryBinding("status");
		assertThat(md.getCategoryBinding(), is("status"));
	}

	@Test
	void setValueBindingRoundtrip() {
		md.setValueBinding("count");
		assertThat(md.getValueBinding(), is("count"));
	}

	@Test
	void setValueFunctionRoundtrip() {
		md.setValueFunction(AggregateFunction.Count);
		assertThat(md.getValueFunction(), is(AggregateFunction.Count));
	}

	@Test
	void setValueFunctionSum() {
		md.setValueFunction(AggregateFunction.Sum);
		assertThat(md.getValueFunction(), is(AggregateFunction.Sum));
	}

	@Test
	void setJFreeChartPostProcessorClassNameRoundtrip() {
		md.setJFreeChartPostProcessorClassName("com.example.Processor");
		assertThat(md.getJFreeChartPostProcessorClassName(), is("com.example.Processor"));
	}

	@Test
	void setPrimeFacesChartPostProcessorClassNameRoundtrip() {
		md.setPrimeFacesChartPostProcessorClassName("com.example.PFProcessor");
		assertThat(md.getPrimeFacesChartPostProcessorClassName(), is("com.example.PFProcessor"));
	}

	// ---- setCategoryBucket ----

	@Test
	void setCategoryBucketNoBucket() {
		NoBucketMetaData bucket = new NoBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	@Test
	void setCategoryBucketNumericMultiple() {
		NumericMultipleBucketMetaData bucket = new NumericMultipleBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	@Test
	void setCategoryBucketTemporalBucket() {
		TemporalBucketMetaData bucket = new TemporalBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	@Test
	void setCategoryBucketTextLengthBucket() {
		TextLengthBucketMetaData bucket = new TextLengthBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	@Test
	void setCategoryBucketTextStartsWithBucket() {
		TextStartsWithBucketMetaData bucket = new TextStartsWithBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	@Test
	void setCategoryBucketNumericRangeBucket() {
		NumericRangeBucketMetaData bucket = new NumericRangeBucketMetaData();
		md.setCategoryBucket(bucket);
		assertThat(md.getCategoryBucket(), is(bucket));
	}

	// ---- setTop ----

	@Test
	void setTopRoundtrip() {
		ChartBuilderTopMetaData top = new ChartBuilderTopMetaData();
		top.setTop(10);
		top.setIncludeOthers(true);
		md.setTop(top);
		assertThat(md.getTop(), is(top));
		assertThat(Integer.valueOf(md.getTop().getTop()), is(Integer.valueOf(10)));
		assertThat(Boolean.valueOf(md.getTop().isIncludeOthers()), is(Boolean.TRUE));
	}

	// ---- setOrder ----

	@Test
	void setOrderRoundtrip() {
		ChartBuilderOrderMetaData order = new ChartBuilderOrderMetaData();
		order.setBy(OrderBy.category);
		order.setSort(SortDirection.ascending);
		md.setOrder(order);
		assertThat(md.getOrder(), is(order));
		assertThat(md.getOrder().getBy(), is(OrderBy.category));
		assertThat(md.getOrder().getSort(), is(SortDirection.ascending));
	}

	// ---- getModelName ----

	@Test
	void getModelNameIsNotNull() {
		md.setModuleName("admin");
		md.setLabel("Status");
		md.setCategoryBinding("status");
		md.setValueBinding("bizId");
		String name = md.getModelName();
		assertNotNull(name);
	}

	@Test
	void getModelNameStartsWithM() {
		md.setModuleName("myModule");
		md.setLabel("Test");
		md.setCategoryBinding("field");
		md.setValueBinding("count");
		String name = md.getModelName();
		assertTrue(name.startsWith("M"), "Model name must start with M");
	}

	@Test
	void getModelNameIsCached() {
		md.setModuleName("m");
		md.setLabel("l");
		md.setCategoryBinding("c");
		md.setValueBinding("v");
		String name1 = md.getModelName();
		String name2 = md.getModelName();
		assertThat(name1, is(name2));
	}
}

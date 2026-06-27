package org.skyve.impl.metadata.view.model.chart;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.view.model.chart.OrderBy;

class ChartBuilderOrderAndTopMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	void orderMetaDataSetByRoundtrip() {
		ChartBuilderOrderMetaData order = new ChartBuilderOrderMetaData();
		order.setBy(OrderBy.category);
		assertThat(order.getBy(), is(OrderBy.category));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderMetaDataSetSortRoundtrip() {
		ChartBuilderOrderMetaData order = new ChartBuilderOrderMetaData();
		order.setSort(SortDirection.ascending);
		assertThat(order.getSort(), is(SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderMetaDataNullByDefault() {
		ChartBuilderOrderMetaData order = new ChartBuilderOrderMetaData();
		assertNull(order.getBy());
		assertNull(order.getSort());
	}

	@Test
	@SuppressWarnings("static-method")
	void topMetaDataSetTopRoundtrip() {
		ChartBuilderTopMetaData top = new ChartBuilderTopMetaData();
		top.setTop(5);
		assertEquals(5, top.getTop());
	}

	@Test
	@SuppressWarnings("static-method")
	void topMetaDataIncludeOthersFalseByDefault() {
		ChartBuilderTopMetaData top = new ChartBuilderTopMetaData();
		assertFalse(top.isIncludeOthers());
	}

	@Test
	@SuppressWarnings("static-method")
	void topMetaDataSetIncludeOthers() {
		ChartBuilderTopMetaData top = new ChartBuilderTopMetaData();
		top.setIncludeOthers(true);
		assertTrue(top.isIncludeOthers());
	}

	@Test
	@SuppressWarnings("static-method")
	void topMetaDataInheritsOrderFields() {
		ChartBuilderTopMetaData top = new ChartBuilderTopMetaData();
		top.setBy(OrderBy.value);
		top.setSort(SortDirection.descending);
		assertThat(top.getBy(), is(OrderBy.value));
		assertThat(top.getSort(), is(SortDirection.descending));
	}

	private static void assertEquals(int expected, int actual) {
		org.junit.jupiter.api.Assertions.assertEquals(expected, actual);
	}
}

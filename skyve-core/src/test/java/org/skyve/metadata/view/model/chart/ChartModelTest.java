package org.skyve.metadata.view.model.chart;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;

@SuppressWarnings("static-method")
class ChartModelTest {

	/** Minimal concrete subclass for testing. */
	private static class TestChartModel extends ChartModel<Bean> {
		@Override
		public ChartData getChartData() {
			return null;
		}
	}

	@Test
	void getBeanReturnsNullInitially() {
		TestChartModel model = new TestChartModel();
		assertNull(model.getBean());
	}

	@Test
	void setBeanRoundtrip() {
		TestChartModel model = new TestChartModel();
		Bean bean = org.mockito.Mockito.mock(Bean.class);
		model.setBean(bean);
		assertSame(bean, model.getBean());
	}

	@Test
	void postConstructDoesNotThrow() {
		TestChartModel model = new TestChartModel();
		Customer customer = org.mockito.Mockito.mock(Customer.class);
		// should not throw
		model.postConstruct(customer, false);
		model.postConstruct(null, true);
	}
}

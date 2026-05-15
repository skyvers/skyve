package org.skyve.metadata.view.model.comparison;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;

/**
 * Tests for ComparisonModel abstract class getBean/setBean/postConstruct.
 */
@SuppressWarnings("static-method")
class ComparisonModelTest {

	/** Minimal concrete ComparisonModel for testing the abstract class */
	private static class TestComparisonModel extends ComparisonModel<Bean, Bean> {
		@Override
		public ComparisonComposite getComparisonComposite(Bean toCompareTo) {
			return null;
		}
	}

	@Test
	void getBeanNullByDefault() {
		TestComparisonModel model = new TestComparisonModel();
		assertThat(model.getBean(), is(nullValue()));
	}

	@Test
	void setBeanRoundtrip() {
		TestComparisonModel model = new TestComparisonModel();
		Bean mockBean = org.mockito.Mockito.mock(Bean.class);
		model.setBean(mockBean);
		assertThat(model.getBean(), is(mockBean));
	}

	@Test
	void postConstructDoesNotThrow() {
		TestComparisonModel model = new TestComparisonModel();
		Customer mockCustomer = org.mockito.Mockito.mock(Customer.class);
		model.postConstruct(mockCustomer, true);
		assertNotNull(model);
	}
}

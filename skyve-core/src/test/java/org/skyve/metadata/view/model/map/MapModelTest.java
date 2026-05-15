package org.skyve.metadata.view.model.map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;

/**
 * Tests for MapModel getBean/setBean/postConstruct and MapFeature geometry.
 */
@SuppressWarnings("static-method")
class MapModelTest {

	/** Minimal concrete MapModel for testing the abstract class */
	private static class TestMapModel extends MapModel<Bean> {
		@Override
		public MapResult getResult(Geometry mapBounds) {
			return null;
		}
	}

	@Test
	void getBeanNullByDefault() {
		TestMapModel model = new TestMapModel();
		assertThat(model.getBean(), is(nullValue()));
	}

	@Test
	void setBeanRoundtrip() {
		TestMapModel model = new TestMapModel();
		Bean mockBean = org.mockito.Mockito.mock(Bean.class);
		model.setBean(mockBean);
		assertThat(model.getBean(), is(mockBean));
	}

	@Test
	void postConstructDoesNotThrow() {
		TestMapModel model = new TestMapModel();
		Customer mockCustomer = org.mockito.Mockito.mock(Customer.class);
		model.postConstruct(mockCustomer, false);
		assertNotNull(model);
	}
}

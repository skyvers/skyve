package org.skyve.impl.metadata.view.widget;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ChartTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Chart().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Chart().getProperties());
	}

	@Test
	void setVisibleConditionNameNegatesCondition() {
		Chart chart = new Chart();
		chart.setVisibleConditionName("showChart");
		assertThat(chart.getInvisibleConditionName(), is("notShowChart"));
	}
}

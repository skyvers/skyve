package org.skyve.impl.metadata.view.container;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TabPaneTest {

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		assertNull(new TabPane().getEnabledConditionName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new TabPane().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new TabPane().getProperties());
	}

        @Test
        void setEnabledConditionNameStoresNegatedCondition() {
                TabPane tabPane = new TabPane();
                tabPane.setEnabledConditionName("active");
                assertNotNull(tabPane.getDisabledConditionName());
        }

        @Test
        void setVisibleConditionNameStoresNegatedCondition() {
                TabPane tabPane = new TabPane();
                tabPane.setVisibleConditionName("active");
                assertNotNull(tabPane.getInvisibleConditionName());
        }
}

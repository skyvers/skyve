package org.skyve.impl.metadata.view.container;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TabPaneTest {

	@Test
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new TabPane().getXmlEnabledCondition());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new TabPane().getXmlVisibleCondition());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new TabPane().getProperties());
	}

        @Test
        void setEnabledConditionDerivesNegatedCondition() {
                TabPane tabPane = new TabPane();
                tabPane.setXmlEnabledCondition("active");
                assertEquals("notActive", tabPane.getDisabledCondition());
        }

        @Test
        void setVisibleConditionDerivesNegatedCondition() {
                TabPane tabPane = new TabPane();
                tabPane.setXmlVisibleCondition("active");
                assertEquals("notActive", tabPane.getInvisibleCondition());
        }
}

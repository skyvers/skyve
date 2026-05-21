package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class LinkTest {

        @Test
        void showsLabelByDefaultReturnsFalse() {
                assertFalse(new Link().showsLabelByDefault());
        }

        @Test
        void jaxbHelperGetVisibleConditionNameReturnsNull() {
                assertNull(new Link().getVisibleConditionName());
        }

        @Test
        void getLocalisedValueWithNullValueReturnsNull() {
                assertNull(new Link().getLocalisedValue());
        }

        @Test
        void setVisibleConditionNameStoresNegatedCondition() {
                Link link = new Link();
                link.setVisibleConditionName("active");
                assertNotNull(link.getInvisibleConditionName());
        }

        @Test
        void getPropertiesReturnsNonNull() {
                assertNotNull(new Link().getProperties());
	}
}

package org.skyve.impl.metadata.view.container.form;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FormTest {

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		assertNull(new Form().getEnabledConditionName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Form().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Form().getProperties());
	}

        @Test
        void setEnabledConditionNameStoresNegatedCondition() {
                Form form = new Form();
                form.setEnabledConditionName("myCondition");
                assertNotNull(form.getDisabledConditionName());
        }

        @Test
        void setVisibleConditionNameStoresNegatedCondition() {
                Form form = new Form();
                form.setVisibleConditionName("myVisible");
                assertNotNull(form.getInvisibleConditionName());
        }
}

package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DataGridColumnTest {

        @Test
        void getPropertiesReturnsNonNullMap() {
                // DataGridColumn is abstract; use a concrete subclass
                assertNotNull(new DataGridBoundColumn().getProperties());
        }

        @Test
        void getLocalisedTitleWithNullTitleReturnsNull() {
                // TabularColumn.getLocalisedTitle() default delegates to Util.i18n(null) which returns null
                DataGridBoundColumn col = new DataGridBoundColumn();
                assertNull(col.getLocalisedTitle());
        }
}

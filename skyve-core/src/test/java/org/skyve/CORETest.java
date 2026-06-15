package org.skyve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

class CORETest {

        @Test
        @SuppressWarnings("static-method")
        void newOrderingReturnsOrderingWithCorrectValues() {
                Ordering ordering = CORE.newOrdering("name", SortDirection.ascending);
                assertNotNull(ordering);
                assertEquals("name", ordering.getBy());
                assertEquals(SortDirection.ascending, ordering.getSort());
        }

        @Test
        @SuppressWarnings("static-method")
        void formatWithNullValueReturnsEmptyString() {
                assertEquals("", CORE.format(FormatterName.DD_MM_YYYY, null));
        }

        @Test
        @SuppressWarnings("static-method")
        void formatByNameWithNullValueReturnsEmptyString() {
                assertEquals("", CORE.format("DD_MM_YYYY", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void formatByNameWithUnknownFormatterReturnsToString() {
                assertEquals("hello", CORE.format("unknownFormatterXYZ", "hello"));
        }

        @Test
        @SuppressWarnings("static-method")
        void getDateFormatReturnsNonLenientFormatter() {
                SimpleDateFormat sdf = CORE.getDateFormat("yyyy-MM-dd");
                assertNotNull(sdf);
                assertFalse(sdf.isLenient());
        }

        @Test
        @SuppressWarnings("static-method")
        void getDecimalFormatReturnsFormatter() {
                DecimalFormat df = CORE.getDecimalFormat("#,##0.00");
                assertNotNull(df);
        }

        @Test
        @SuppressWarnings("static-method")
        void getSerializableDateFormatReturnsYYYYMMDD() {
                SimpleDateFormat sdf = CORE.getSerializableDateFormat();
                assertNotNull(sdf);
                assertEquals("yyyy-MM-dd", sdf.toPattern());
        }

        @Test
        @SuppressWarnings("static-method")
        void getSerializableTimeFormatReturnsHHmmss() {
                SimpleDateFormat sdf = CORE.getSerializableTimeFormat();
                assertNotNull(sdf);
                assertEquals("HH:mm:ss", sdf.toPattern());
        }
}

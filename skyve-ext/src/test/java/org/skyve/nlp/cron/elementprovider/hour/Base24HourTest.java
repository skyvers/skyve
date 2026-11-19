package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class Base24HourTest {

        @Test
        @SuppressWarnings("static-method")
        void testGetDayNumberElementReturnsNull() {
                Base24Hour provider = new Base24Hour();
                assertThat(provider.getDayNumberElement(), is(nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void testGetMonthElementReturnsNull() {
                Base24Hour provider = new Base24Hour();
                assertThat(provider.getMonthElement(), is(nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void testGetDayOfWeekElementReturnsNull() {
                Base24Hour provider = new Base24Hour();
                assertThat(provider.getDayOfWeekElement(), is(nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void testCanProvideMinuteReturnsFalseWithNoSegments() {
                Base24Hour provider = new Base24Hour();
                assertFalse(provider.canProvideMinute());
        }

        @Test
        @SuppressWarnings("static-method")
        void testGetMinuteElementReturnsNullWithNoSegments() {
                Base24Hour provider = new Base24Hour();
                assertThat(provider.getMinuteElement(), is(nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void testCanProvideHourReturnsFalseWithNoSegments() {
                Base24Hour provider = new Base24Hour();
                assertFalse(provider.canProvideHour());
        }

        @Test
        @SuppressWarnings("static-method")
        void testGetHourElementReturnsNullWithNoSegments() {
                Base24Hour provider = new Base24Hour();
                assertThat(provider.getHourElement(), is(nullValue()));
        }

}

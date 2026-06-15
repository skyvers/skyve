package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;

@SuppressWarnings("static-method")
class DynamicImageTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		DynamicImage widget = new DynamicImage();
		assertNull(widget.getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		DynamicImage widget = new DynamicImage();
		assertNotNull(widget.getProperties());
	}

        @Test
        void getInvisibleConditionNameReturnsNullByDefault() {
                assertNull(new DynamicImage().getInvisibleConditionName());
        }

        @Test
        void setInvisibleConditionNameStoresValue() {
                DynamicImage widget = new DynamicImage();
                widget.setInvisibleConditionName("hidden");
                assertNotNull(widget.getInvisibleConditionName());
        }

        @Test
        void setVisibleConditionNameStoresNegatedCondition() {
                DynamicImage widget = new DynamicImage();
                widget.setVisibleConditionName("active");
                assertNotNull(widget.getInvisibleConditionName());
        }

        @Test
        void settersPopulateDynamicImageConfiguration() {
                DynamicImage widget = new DynamicImage();
                ParameterImpl parameter = new ParameterImpl();
                parameter.setName("customerId");
                parameter.setValueBinding("customer.id");

                widget.setName("  provider  ");
                widget.setImageInitialPixelWidth(Integer.valueOf(64));
                widget.setImageInitialPixelHeight(Integer.valueOf(48));
                widget.setPixelWidth(Integer.valueOf(320));
                widget.setResponsiveWidth(Integer.valueOf(6));
                widget.setSm(Integer.valueOf(4));
                widget.setMd(Integer.valueOf(6));
                widget.setLg(Integer.valueOf(8));
                widget.setXl(Integer.valueOf(10));
                widget.setPercentageWidth(Integer.valueOf(75));
                widget.setMinPixelWidth(Integer.valueOf(100));
                widget.setMaxPixelWidth(Integer.valueOf(500));
                widget.setPixelHeight(Integer.valueOf(200));
                widget.setPercentageHeight(Integer.valueOf(60));
                widget.setMinPixelHeight(Integer.valueOf(80));
                widget.setMaxPixelHeight(Integer.valueOf(400));
                widget.getParameters().add(parameter);

                assertEquals("provider", widget.getName());
                assertEquals(Integer.valueOf(64), widget.getImageInitialPixelWidth());
                assertEquals(Integer.valueOf(48), widget.getImageInitialPixelHeight());
                assertEquals(Integer.valueOf(320), widget.getPixelWidth());
                assertEquals(Integer.valueOf(6), widget.getResponsiveWidth());
                assertEquals(Integer.valueOf(4), widget.getSm());
                assertEquals(Integer.valueOf(6), widget.getMd());
                assertEquals(Integer.valueOf(8), widget.getLg());
                assertEquals(Integer.valueOf(10), widget.getXl());
                assertEquals(Integer.valueOf(75), widget.getPercentageWidth());
                assertEquals(Integer.valueOf(100), widget.getMinPixelWidth());
                assertEquals(Integer.valueOf(500), widget.getMaxPixelWidth());
                assertEquals(Integer.valueOf(200), widget.getPixelHeight());
                assertEquals(Integer.valueOf(60), widget.getPercentageHeight());
                assertEquals(Integer.valueOf(80), widget.getMinPixelHeight());
                assertEquals(Integer.valueOf(400), widget.getMaxPixelHeight());
                assertEquals(1, widget.getParameters().size());
                assertEquals(parameter, widget.getParameters().get(0));
        }
}

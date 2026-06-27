package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class StaticImageTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new StaticImage().showsLabelByDefault());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new StaticImage().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new StaticImage().getProperties());
	}

	@Test
	void setVisibleConditionNameNegatesCondition() {
		StaticImage img = new StaticImage();
		img.setVisibleConditionName("visible");
		assertNotNull(img.getInvisibleConditionName());
	}

	@Test
	void settersPopulateStaticImageConfiguration() {
		StaticImage image = new StaticImage();
		image.setRelativeFile("  images/logo.png  ");
		image.setPixelWidth(Integer.valueOf(320));
		image.setResponsiveWidth(Integer.valueOf(6));
		image.setSm(Integer.valueOf(4));
		image.setMd(Integer.valueOf(6));
		image.setLg(Integer.valueOf(8));
		image.setXl(Integer.valueOf(10));
		image.setPercentageWidth(Integer.valueOf(75));
		image.setMinPixelWidth(Integer.valueOf(100));
		image.setMaxPixelWidth(Integer.valueOf(500));
		image.setPixelHeight(Integer.valueOf(200));
		image.setPercentageHeight(Integer.valueOf(60));
		image.setMinPixelHeight(Integer.valueOf(80));
		image.setMaxPixelHeight(Integer.valueOf(400));
		image.setInvisibleConditionName("hidden");

		assertEquals("images/logo.png", image.getRelativeFile());
		assertEquals(Integer.valueOf(320), image.getPixelWidth());
		assertEquals(Integer.valueOf(6), image.getResponsiveWidth());
		assertEquals(Integer.valueOf(4), image.getSm());
		assertEquals(Integer.valueOf(6), image.getMd());
		assertEquals(Integer.valueOf(8), image.getLg());
		assertEquals(Integer.valueOf(10), image.getXl());
		assertEquals(Integer.valueOf(75), image.getPercentageWidth());
		assertEquals(Integer.valueOf(100), image.getMinPixelWidth());
		assertEquals(Integer.valueOf(500), image.getMaxPixelWidth());
		assertEquals(Integer.valueOf(200), image.getPixelHeight());
		assertEquals(Integer.valueOf(60), image.getPercentageHeight());
		assertEquals(Integer.valueOf(80), image.getMinPixelHeight());
		assertEquals(Integer.valueOf(400), image.getMaxPixelHeight());
		assertEquals("hidden", image.getInvisibleConditionName());
	}
}

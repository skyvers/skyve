package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class LayoutUtilTest {

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthToPercentageWidthHalf() {
		// 6 out of 12 = 50%
		assertEquals(50, LayoutUtil.responsiveWidthToPercentageWidth(6));
	}

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthToPercentageWidthFull() {
		// 12 out of 12 = 100%
		assertEquals(100, LayoutUtil.responsiveWidthToPercentageWidth(12));
	}

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthToPercentageWidthCapsAt100() {
		// 15 out of 12 would be 125% but capped at 100%
		assertEquals(100, LayoutUtil.responsiveWidthToPercentageWidth(15));
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthToMediumResponsiveWidthSmall() {
		// 100px of 1024 * 12 = ~1.17 → 2
		int result = LayoutUtil.pixelWidthToMediumResponsiveWidth(100);
		assertTrue(result >= 1 && result <= 12);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthToMediumResponsiveWidthFull() {
		// 1024px = full width = 12
		assertEquals(12, LayoutUtil.pixelWidthToMediumResponsiveWidth(1024));
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthToMediumResponsiveWidthCapsAt12() {
		// 2048px > 1024 caps at 12
		assertEquals(12, LayoutUtil.pixelWidthToMediumResponsiveWidth(2048));
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthToLargeResponsiveWidthFull() {
		// 1440px = full width = 12
		assertEquals(12, LayoutUtil.pixelWidthToLargeResponsiveWidth(1440));
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthToLargeResponsiveWidthCapsAt12() {
		assertEquals(12, LayoutUtil.pixelWidthToLargeResponsiveWidth(2000));
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageWidthToResponsiveWidthHalf() {
		// 50% of 12 = 6
		assertEquals(6, LayoutUtil.percentageWidthToResponsiveWidth(50));
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageWidthToResponsiveWidthFull() {
		assertEquals(12, LayoutUtil.percentageWidthToResponsiveWidth(100));
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageWidthToResponsiveWidthCapsAt12() {
		assertEquals(12, LayoutUtil.percentageWidthToResponsiveWidth(150));
	}

	@Test
	@SuppressWarnings("static-method")
	void maxResponsiveWidthColumnIs12() {
		assertEquals(12, LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS);
	}
}

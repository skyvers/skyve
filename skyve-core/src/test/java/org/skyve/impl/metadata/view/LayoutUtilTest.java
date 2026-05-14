package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class LayoutUtilTest {

	@Test
	@SuppressWarnings("static-method")
	public void responsiveWidthToPercentageWidthHalf() {
		// 6 out of 12 = 50%
		assertEquals(50, LayoutUtil.responsiveWidthToPercentageWidth(6));
	}

	@Test
	@SuppressWarnings("static-method")
	public void responsiveWidthToPercentageWidthFull() {
		// 12 out of 12 = 100%
		assertEquals(100, LayoutUtil.responsiveWidthToPercentageWidth(12));
	}

	@Test
	@SuppressWarnings("static-method")
	public void responsiveWidthToPercentageWidthCapsAt100() {
		// 15 out of 12 would be 125% but capped at 100%
		assertEquals(100, LayoutUtil.responsiveWidthToPercentageWidth(15));
	}

	@Test
	@SuppressWarnings("static-method")
	public void pixelWidthToMediumResponsiveWidthSmall() {
		// 100px of 1024 * 12 = ~1.17 → 2
		int result = LayoutUtil.pixelWidthToMediumResponsiveWidth(100);
		assertTrue(result >= 1 && result <= 12);
	}

	@Test
	@SuppressWarnings("static-method")
	public void pixelWidthToMediumResponsiveWidthFull() {
		// 1024px = full width = 12
		assertEquals(12, LayoutUtil.pixelWidthToMediumResponsiveWidth(1024));
	}

	@Test
	@SuppressWarnings("static-method")
	public void pixelWidthToMediumResponsiveWidthCapsAt12() {
		// 2048px > 1024 caps at 12
		assertEquals(12, LayoutUtil.pixelWidthToMediumResponsiveWidth(2048));
	}

	@Test
	@SuppressWarnings("static-method")
	public void pixelWidthToLargeResponsiveWidthFull() {
		// 1440px = full width = 12
		assertEquals(12, LayoutUtil.pixelWidthToLargeResponsiveWidth(1440));
	}

	@Test
	@SuppressWarnings("static-method")
	public void pixelWidthToLargeResponsiveWidthCapsAt12() {
		assertEquals(12, LayoutUtil.pixelWidthToLargeResponsiveWidth(2000));
	}

	@Test
	@SuppressWarnings("static-method")
	public void percentageWidthToResponsiveWidthHalf() {
		// 50% of 12 = 6
		assertEquals(6, LayoutUtil.percentageWidthToResponsiveWidth(50));
	}

	@Test
	@SuppressWarnings("static-method")
	public void percentageWidthToResponsiveWidthFull() {
		assertEquals(12, LayoutUtil.percentageWidthToResponsiveWidth(100));
	}

	@Test
	@SuppressWarnings("static-method")
	public void percentageWidthToResponsiveWidthCapsAt12() {
		assertEquals(12, LayoutUtil.percentageWidthToResponsiveWidth(150));
	}

	@Test
	@SuppressWarnings("static-method")
	public void maxResponsiveWidthColumnIs12() {
		assertEquals(12, LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS);
	}
}

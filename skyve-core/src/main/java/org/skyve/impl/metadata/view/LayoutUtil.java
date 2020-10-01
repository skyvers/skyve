package org.skyve.impl.metadata.view;

import org.skyve.impl.util.UtilImpl;

public class LayoutUtil {
	private LayoutUtil() {}
	
	public static int MAX_RESPONSIVE_WIDTH_COLUMNS = 12;
	private static double MAX_MEDIUM_SCREEN_WIDTH_PIXELS = (UtilImpl.PRIMEFLEX ? 991.0 : 1024.0);
	private static double MAX_LARGE_SCREEN_WIDTH_PIXELS = (UtilImpl.PRIMEFLEX ? 1199.0 : 1440.0);
	
	public static int responsiveWidthToPercentageWidth(double responsiveWidth) {
		int result = (int) Math.ceil(responsiveWidth / MAX_RESPONSIVE_WIDTH_COLUMNS * 100.0);
		return (result > 100) ? 100 : result;
	}

	public static int pixelWidthToMediumResponsiveWidth(double pixelWidth) {
		int result = (int) Math.ceil(pixelWidth / MAX_MEDIUM_SCREEN_WIDTH_PIXELS * MAX_RESPONSIVE_WIDTH_COLUMNS);
		return (result > 12) ? 12 : result;
	}
	
	public static int pixelWidthToLargeResponsiveWidth(double pixelWidth) {
		int result = (int) Math.ceil(pixelWidth / MAX_LARGE_SCREEN_WIDTH_PIXELS * MAX_RESPONSIVE_WIDTH_COLUMNS);
		return (result > 12) ? 12 : result;
	}

	public static int percentageWidthToResponsiveWidth(double percentageWidth) {
		int result = (int) Math.ceil(percentageWidth / 100.0 * MAX_RESPONSIVE_WIDTH_COLUMNS);
		return (result > 12) ? 12 : result;
	}
}

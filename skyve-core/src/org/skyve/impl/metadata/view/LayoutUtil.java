package org.skyve.impl.metadata.view;

public class LayoutUtil {
	private LayoutUtil() {}
	
	public static int MAX_RESPONSIVE_WIDTH_COLUMNS = 12;
	private static double MAX_SMALL_SCREEN_WIDTH_PIXELS = 640.0;
	private static double MAX_MEDIUM_SCREEN_WIDTH_PIXELS = 1024.0;
	private static double MAX_LARGE_SCREEN_WIDTH_PIXELS = 1440.0;
	
	public static int responsiveWidthToPercentageWidth(double responsiveWidth) {
		return (int) Math.ceil(responsiveWidth / MAX_RESPONSIVE_WIDTH_COLUMNS * 100.0);
	}

	public static int pixelWidthToMediumResponsiveWidth(double pixelWidth) {
		return (int) Math.ceil(pixelWidth / MAX_MEDIUM_SCREEN_WIDTH_PIXELS * MAX_RESPONSIVE_WIDTH_COLUMNS);
	}
	
	public static int pixelWidthToLargeResponsiveWidth(double pixelWidth) {
		return (int) Math.ceil(pixelWidth / MAX_LARGE_SCREEN_WIDTH_PIXELS * MAX_RESPONSIVE_WIDTH_COLUMNS);
	}

	public static int percentageWidthToResponsiveWidth(double percentageWidth) {
		return (int) Math.ceil(percentageWidth / 100.0 * MAX_RESPONSIVE_WIDTH_COLUMNS);
	}
}

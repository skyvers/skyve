package org.skyve.util;

import org.locationtech.jts.geom.Point;

/**
 * Calculates geodesic measurements for longitude/latitude coordinates.
 *
 * <p>Thread-safe: this utility holds no mutable state.
 */
public class GeodesicUtil {
	/**
	 * Mean Earth radius in kilometres, using the IUGG mean radius.
	 */
	public static final double EARTH_RADIUS_KILOMETRES = 6371.0088d;

	private GeodesicUtil() {
		// prevent instantiation
	}
	
	/**
	 * Returns the great-circle distance between two points in kilometres.
	 *
	 * <p>Precondition: both points must use longitude/latitude coordinates in decimal
	 * degrees. JTS stores longitude in {@link Point#getX()} and latitude in
	 * {@link Point#getY()}.
	 *
	 * <p>Complexity: O(1) time and O(1) space.
	 *
	 * @param a the first point; must not be {@code null}
	 * @param b the second point; must not be {@code null}
	 * @return the haversine distance in kilometres
	 */
	public static double haversineDistanceKilometres(Point a, Point b) {
		double latitude1 = Math.toRadians(a.getY());
		double latitude2 = Math.toRadians(b.getY());
		double deltaLatitude = Math.toRadians(b.getY() - a.getY());
		double deltaLongitude = Math.toRadians(b.getX() - a.getX());

		double sinLatitude = Math.sin(deltaLatitude / 2.0d);
		double sinLongitude = Math.sin(deltaLongitude / 2.0d);
		double haversine = (sinLatitude * sinLatitude) +
								(Math.cos(latitude1) * Math.cos(latitude2) * sinLongitude * sinLongitude);
		return EARTH_RADIUS_KILOMETRES * 2.0d * Math.atan2(Math.sqrt(haversine), Math.sqrt(1.0d - haversine));
	}
}

package modules.admin.SecurityLog.models;

import java.util.Collections;
import java.util.List;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.skyve.metadata.view.model.map.MapFeature;
import org.skyve.metadata.view.model.map.MapItem;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;
import org.skyve.util.IPGeolocation;

import modules.admin.SecurityLog.SecurityLogExtension;

/**
	 * Builds a single-item map view showing the current security log's GeoIP
	 * point location and a small visual buffer.
 */
public class GeoIPMap extends MapModel<SecurityLogExtension> {
	/**
	 * Returns map features for the bound security log bean.
	 *
	 * @param mapBounds the requested map bounds
	 * @return a map result containing either one map item or no items when the
	 *         bean has no location
	 * @throws Exception if map model generation fails
	 */
	@Override
	public MapResult getResult(Geometry mapBounds) throws Exception {
		return mapModel(getBean().getGeoIP());
	}
	
	/**
	 * Creates a map result from a geolocation record.
	 *
	 * @param geoIP the geolocation to transform
	 * @return a map result containing a marker and light buffer when a location
	 *         exists, otherwise an empty result
	 */
	public static MapResult mapModel(IPGeolocation geoIP) {
		if (IPGeolocation.EMPTY != geoIP) {
			Point location = geoIP.location();
			if (location != null) {
				// Add 1 map item with marker and buffer
				MapItem item = new MapItem();
				List<MapFeature> features = item.getFeatures();
				
				// Add a buffer coz the extents in MapResult doesn't work 
				MapFeature feature = new MapFeature();
				feature.setGeometry(location.buffer(10d));
				feature.setZoomable(false);
				feature.setFillColour("#FFFF00"); // yellow
				feature.setFillOpacity(0.05f);
				feature.setStrokeColour("#0000FF"); //blue
				features.add(feature);
				
				feature = new MapFeature();
				feature.setGeometry(location);
				feature.setZoomable(false);
				features.add(feature);

				return new MapResult(Collections.singletonList(item), null);
			}
		}
		return new MapResult(Collections.emptyList(), null);
	}
}

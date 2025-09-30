package modules.admin.UserLoginRecord.models;

import org.locationtech.jts.geom.Geometry;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;

import modules.admin.UserLoginRecord.UserLoginRecordExtension;

/**
 * Show IP location on a map.
 */
public class GeoIPMap extends MapModel<UserLoginRecordExtension> {
	@Override
	public MapResult getResult(Geometry mapBounds) throws Exception {
		return modules.admin.SecurityLog.models.GeoIPMap.mapModel(getBean().getGeoIP());
	}
}

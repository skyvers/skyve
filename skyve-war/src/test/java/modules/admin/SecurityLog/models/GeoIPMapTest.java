package modules.admin.SecurityLog.models;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.metadata.view.model.map.MapResult;
import org.skyve.util.IPGeolocation;

import util.AbstractSkyveTest;

@SuppressWarnings("static-method")
public class GeoIPMapTest extends AbstractSkyveTest {

	@Test
	void mapModelWithEmptyGeoIPReturnsEmptyList() {
		MapResult result = GeoIPMap.mapModel(IPGeolocation.EMPTY);
		assertNotNull(result);
		assertTrue(result.getItems().isEmpty());
	}

	@Test
	void mapModelWithNullLocationReturnsEmptyList() {
		IPGeolocation geoIP = new IPGeolocation("Sydney", "Australia", "AU", null);
		MapResult result = GeoIPMap.mapModel(geoIP);
		assertNotNull(result);
		assertTrue(result.getItems().isEmpty());
	}

	@Test
	void mapModelWithValidLocationReturnsSingleItem() {
		GeometryFactory factory = new GeometryFactory();
		Point point = factory.createPoint(new Coordinate(151.2, -33.9));
		IPGeolocation geoIP = new IPGeolocation("Sydney", "Australia", "AU", point);
		MapResult result = GeoIPMap.mapModel(geoIP);
		assertNotNull(result);
		assertTrue(result.getItems().size() == 1);
	}
}

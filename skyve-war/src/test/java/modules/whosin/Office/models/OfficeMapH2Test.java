package modules.whosin.Office.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.metadata.view.model.map.MapItem;
import org.skyve.metadata.view.model.map.MapResult;

import modules.whosin.domain.Office;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class OfficeMapH2Test extends AbstractH2Test {
	private static final GeometryFactory GEOMETRY_FACTORY = new GeometryFactory();

	@Test
	void nonPersistedOfficeWithoutBoundaryReturnsNoItems() throws Exception {
		OfficeMap model = new OfficeMap();
		model.setBean(new Office());

		MapResult result = model.getResult(square(0.0, 0.0, 2.0));

		assertTrue(result.getItems().isEmpty());
	}

	@Test
	void nonPersistedOfficeWithIntersectingBoundaryReturnsOfficeFeature() throws Exception {
		Office office = new Office();
		office.setBizId("office-1");
		office.setStreetAddress("1 Test Street");
		office.setSuburb("Adelaide");
		office.setBoundary(square(0.0, 0.0, 1.0));
		OfficeMap model = new OfficeMap();
		model.setBean(office);

		MapResult result = model.getResult(square(-0.5, -0.5, 2.0));

		assertEquals(1, result.getItems().size());
		MapItem item = result.getItems().get(0);
		assertThat(item.getBizId(), is("office-1"));
		assertThat(item.getModuleName(), is(Office.MODULE_NAME));
		assertThat(item.getDocumentName(), is(Office.DOCUMENT_NAME));
		assertEquals(1, item.getFeatures().size());
		assertThat(item.getFeatures().get(0).getFillColour(), is("#FFFF00"));
	}

	@Test
	void nonPersistedOfficeWithOutsideBoundaryReturnsNoItems() throws Exception {
		Office office = new Office();
		office.setBoundary(square(10.0, 10.0, 1.0));
		OfficeMap model = new OfficeMap();
		model.setBean(office);

		MapResult result = model.getResult(square(0.0, 0.0, 2.0));

		assertTrue(result.getItems().isEmpty());
	}

	private static Geometry square(double x, double y, double width) {
		return GEOMETRY_FACTORY.createPolygon(new Coordinate[] {
				new Coordinate(x, y),
				new Coordinate(x + width, y),
				new Coordinate(x + width, y + width),
				new Coordinate(x, y + width),
				new Coordinate(x, y)
		});
	}
}

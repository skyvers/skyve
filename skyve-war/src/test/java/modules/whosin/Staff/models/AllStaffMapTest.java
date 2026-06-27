package modules.whosin.Staff.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.metadata.view.model.map.MapFeature;
import org.skyve.metadata.view.model.map.MapItem;
import org.skyve.metadata.view.model.map.MapResult;

import modules.admin.Contact.ContactExtension;
import modules.whosin.domain.Staff;

@SuppressWarnings("static-method")
class AllStaffMapTest {
	private static final GeometryFactory GEOMETRY_FACTORY = new GeometryFactory();

	@Test
	void getResultReturnsMapItemForStaffInsideBounds() throws Exception {
		Staff staff = staff("staff-1", "Person Example", Staff.Status.inTheOffice, point(1.0, 1.0));
		TestableAllStaffMap model = new TestableAllStaffMap(List.of(staff));

		MapResult result = model.getResult(square(0.0, 0.0, 2.0));

		assertEquals(1, result.getItems().size());
		MapItem item = result.getItems().get(0);
		assertThat(item.getBizId(), is("staff-1"));
		assertThat(item.getModuleName(), is(Staff.MODULE_NAME));
		assertThat(item.getDocumentName(), is(Staff.DOCUMENT_NAME));
		assertThat(item.getInfoMarkup(), is("Person Example<br/>In the Office"));
		assertEquals(1, item.getFeatures().size());
		MapFeature feature = item.getFeatures().get(0);
		assertThat(feature.getGeometry(), is(staff.getLocation()));
		assertThat(feature.getIconRelativeFilePath(), is("icons/document/user16.png"));
		assertThat(feature.getIconAnchorX(), is(Integer.valueOf(8)));
		assertThat(feature.getIconAnchorY(), is(Integer.valueOf(8)));
	}

	@Test
	void getResultSkipsStaffOutsideBoundsAndOmitsNullStatus() throws Exception {
		Staff outside = staff("outside", "Outside Person", Staff.Status.outOfTheOffice, point(10.0, 10.0));
		Staff inside = staff("inside", "Inside Person", null, point(1.0, 1.0));
		TestableAllStaffMap model = new TestableAllStaffMap(List.of(outside, inside));

		MapResult result = model.getResult(square(0.0, 0.0, 2.0));

		assertEquals(1, result.getItems().size());
		assertThat(result.getItems().get(0).getBizId(), is("inside"));
		assertThat(result.getItems().get(0).getInfoMarkup(), is("Inside Person"));
	}

	@Test
	void getResultReturnsNoItemsWhenNoStaffIntersectBounds() throws Exception {
		TestableAllStaffMap model = new TestableAllStaffMap(List.of(staff("outside", "Outside Person", null, point(10.0, 10.0))));

		MapResult result = model.getResult(square(0.0, 0.0, 2.0));

		assertTrue(result.getItems().isEmpty());
	}

	private static Staff staff(String bizId, String contactName, Staff.Status status, Geometry location) {
		Staff staff = new Staff();
		staff.setBizId(bizId);
		staff.setStatus(status);
		staff.setLocation(location);
		ContactExtension contact = new ContactExtension();
		contact.setName(contactName);
		staff.setContact(contact);
		return staff;
	}

	private static Geometry point(double x, double y) {
		return GEOMETRY_FACTORY.createPoint(new Coordinate(x, y));
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

	private static class TestableAllStaffMap extends AllStaffMap {
		private final List<Staff> staff;

		private TestableAllStaffMap(List<Staff> staff) {
			this.staff = staff;
		}

		@Override
		protected List<Staff> staffForMap() {
			return staff;
		}
	}
}

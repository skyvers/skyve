package org.skyve.impl.domain.types.jaxb;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;

class GeometryMapperTest {

	@Test
	@SuppressWarnings("static-method")
	void unmarshalNullReturnsNull() throws Exception {
		GeometryMapper mapper = new GeometryMapper();
		assertNull(mapper.unmarshal(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void unmarshalValidWKT() throws Exception {
		GeometryMapper mapper = new GeometryMapper();
		Geometry geom = mapper.unmarshal("POINT (1 2)");
		assertThat(geom.getGeometryType(), is("Point"));
	}

	@Test
	@SuppressWarnings("static-method")
	void marshalNullReturnsNull() throws Exception {
		GeometryMapper mapper = new GeometryMapper();
		assertNull(mapper.marshal(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void marshalRoundtrip() throws Exception {
		GeometryMapper mapper = new GeometryMapper();
		Geometry geom = mapper.unmarshal("POINT (1 2)");
		String wkt = mapper.marshal(geom);
		assertThat(wkt, is("POINT (1 2)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void unmarshalInvalidWKTThrows() {
		GeometryMapper mapper = new GeometryMapper();
		org.junit.jupiter.api.Assertions.assertThrows(Exception.class, () -> mapper.unmarshal("NOT_VALID_WKT!!"));
	}
}

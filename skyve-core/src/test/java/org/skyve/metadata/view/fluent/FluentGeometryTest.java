package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;

@SuppressWarnings("static-method")
class FluentGeometryTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentGeometry().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesInstance() {
		Geometry g = new Geometry();
		assertEquals(g, new FluentGeometry(g).get());
	}

	@Test
	void typeSetsValue() {
		assertThat(new FluentGeometry().type(GeometryInputType.point).get().getType(), is(GeometryInputType.point));
	}

	@Test
	void pixelWidthSetsValue() {
		assertEquals(300, new FluentGeometry().pixelWidth(300).get().getPixelWidth().intValue());
	}

	@Test
	void fromCopiesType() {
		Geometry src = new Geometry();
		src.setType(GeometryInputType.polygon);
		assertThat(new FluentGeometry().from(src).get().getType(), is(GeometryInputType.polygon));
	}

	@Test
	void fromWithNullTypeDoesNotThrow() {
		Geometry src = new Geometry();
		assertThat(new FluentGeometry().from(src).get(), is(notNullValue()));
	}
}

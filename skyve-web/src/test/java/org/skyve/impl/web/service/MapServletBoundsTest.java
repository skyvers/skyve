package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.io.ParseException;

import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class MapServletBoundsTest {
	@Test
	void mapBoundsDefaultsToWorldBoundsWhenNoCornersProvided() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Geometry result = invokeMapBounds(request);

		assertInstanceOf(Polygon.class, result);
		assertEquals(-180.0, result.getEnvelopeInternal().getMinX());
		assertEquals(180.0, result.getEnvelopeInternal().getMaxX());
		assertEquals(-90.0, result.getEnvelopeInternal().getMinY());
		assertEquals(90.0, result.getEnvelopeInternal().getMaxY());
	}

	@Test
	void mapBoundsReturnsPolygonWhenViewportDoesNotCrossAntiMeridian() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("_ne")).thenReturn("POINT(10 20)");
		when(request.getParameter("_sw")).thenReturn("POINT(-5 -10)");

		Geometry result = invokeMapBounds(request);

		assertInstanceOf(Polygon.class, result);
		assertEquals(-5.0, result.getEnvelopeInternal().getMinX());
		assertEquals(10.0, result.getEnvelopeInternal().getMaxX());
	}

	@Test
	void mapBoundsReturnsMultiPolygonWhenViewportCrossesAntiMeridian() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("_ne")).thenReturn("POINT(-170 20)");
		when(request.getParameter("_sw")).thenReturn("POINT(170 -10)");

		Geometry result = invokeMapBounds(request);

		MultiPolygon multiPolygon = assertInstanceOf(MultiPolygon.class, result);
		assertEquals(2, multiPolygon.getNumGeometries());
	}

	@Test
	void mapBoundsRejectsInvalidWktInput() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("_ne")).thenReturn("NOT_A_POINT");

		ParseException thrown = assertThrows(ParseException.class, () -> invokeMapBounds(request));
		assertEquals(ParseException.class, thrown.getClass());
	}

	private static Geometry invokeMapBounds(HttpServletRequest request) throws Exception {
		Method method = MapServlet.class.getDeclaredMethod("mapBounds", HttpServletRequest.class);
		method.setAccessible(true);
		try {
			return (Geometry) method.invoke(null, request);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}
}

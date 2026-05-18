package org.skyve.metadata.view.model.map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class MapFeatureTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorHasExpectedDefaults() {
		MapFeature f = new MapFeature();
		assertNull(f.getGeometry());
		assertTrue(f.isZoomable());
		assertFalse(f.isEditable());
		assertNull(f.getStrokeColour());
		assertNull(f.getFillColour());
		assertEquals(1.0f, f.getFillOpacity(), 0.001f);
		assertNull(f.getIconRelativeFilePath());
		assertNull(f.getIconAnchorX());
		assertNull(f.getIconAnchorY());
	}

	@Test
	@SuppressWarnings("static-method")
	void fullConstructorSetsAllFields() {
		MapFeature f = new MapFeature(null, false, true, "stroke1", "fill1", 0.5f, "icon.png", Integer.valueOf(10), Integer.valueOf(20));
		assertNull(f.getGeometry());
		assertFalse(f.isZoomable());
		assertTrue(f.isEditable());
		assertEquals("stroke1", f.getStrokeColour());
		assertEquals("fill1", f.getFillColour());
		assertEquals(0.5f, f.getFillOpacity(), 0.001f);
		assertEquals("icon.png", f.getIconRelativeFilePath());
		assertEquals(Integer.valueOf(10), f.getIconAnchorX());
		assertEquals(Integer.valueOf(20), f.getIconAnchorY());
	}

	@Test
	@SuppressWarnings("static-method")
	void setFillOpacityClampsBelowZero() {
		MapFeature f = new MapFeature();
		f.setFillOpacity(-0.5f);
		assertEquals(0.0f, f.getFillOpacity(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void setFillOpacityClampsAboveOne() {
		MapFeature f = new MapFeature();
		f.setFillOpacity(1.5f);
		assertEquals(1.0f, f.getFillOpacity(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void setFillOpacityMidRangeIsKept() {
		MapFeature f = new MapFeature();
		f.setFillOpacity(0.75f);
		assertEquals(0.75f, f.getFillOpacity(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void setZoomableRoundtrip() {
		MapFeature f = new MapFeature();
		f.setZoomable(false);
		assertFalse(f.isZoomable());
		f.setZoomable(true);
		assertTrue(f.isZoomable());
	}

	@Test
	@SuppressWarnings("static-method")
	void setEditableRoundtrip() {
		MapFeature f = new MapFeature();
		f.setEditable(true);
		assertTrue(f.isEditable());
	}

	@Test
	@SuppressWarnings("static-method")
	void setStrokeColourRoundtrip() {
		MapFeature f = new MapFeature();
		f.setStrokeColour("#ff0000");
		assertEquals("#ff0000", f.getStrokeColour());
	}

	@Test
	@SuppressWarnings("static-method")
	void setFillColourRoundtrip() {
		MapFeature f = new MapFeature();
		f.setFillColour("#00ff00");
		assertEquals("#00ff00", f.getFillColour());
	}

	@Test
	@SuppressWarnings("static-method")
	void setIconRelativeFilePathRoundtrip() {
		MapFeature f = new MapFeature();
		f.setIconRelativeFilePath("images/marker.png");
		assertEquals("images/marker.png", f.getIconRelativeFilePath());
	}

	@Test
	@SuppressWarnings("static-method")
	void setIconAnchorXRoundtrip() {
		MapFeature f = new MapFeature();
		f.setIconAnchorX(Integer.valueOf(15));
		assertEquals(Integer.valueOf(15), f.getIconAnchorX());
	}

	@Test
	@SuppressWarnings("static-method")
	void setIconAnchorYRoundtrip() {
		MapFeature f = new MapFeature();
		f.setIconAnchorY(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), f.getIconAnchorY());
	}

	@Test
	@SuppressWarnings("static-method")
	void setGeometryRoundtrip() throws Exception {
		MapFeature f = new MapFeature();
		org.locationtech.jts.geom.Geometry geom = new org.skyve.domain.types.converters.geometry.GeometryConverter().fromDisplayValue("POINT (1 2)");
		f.setGeometry(geom);
		assertEquals(geom, f.getGeometry());
	}
}

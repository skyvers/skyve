package org.skyve.metadata.view.model.map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Envelope;

class MapResultTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorHasNullFields() {
		MapResult result = new MapResult();
		assertNull(result.getItems());
		assertNull(result.getMapExtents());
	}

	@Test
	@SuppressWarnings("static-method")
	void twoArgConstructorSetsFields() {
		List<MapItem> items = new ArrayList<>();
		Envelope envelope = new Envelope(0, 1, 0, 1);
		MapResult result = new MapResult(items, envelope);
		assertSame(items, result.getItems());
		assertSame(envelope, result.getMapExtents());
	}

	@Test
	@SuppressWarnings("static-method")
	void setItemsRoundtrip() {
		MapResult result = new MapResult();
		List<MapItem> items = new ArrayList<>();
		result.setItems(items);
		assertNotNull(result.getItems());
	}

	@Test
	@SuppressWarnings("static-method")
	void setMapExtentsRoundtrip() {
		MapResult result = new MapResult();
		Envelope envelope = new Envelope(10, 20, 30, 40);
		result.setMapExtents(envelope);
		assertSame(envelope, result.getMapExtents());
	}
}

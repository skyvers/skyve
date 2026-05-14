package org.skyve.impl.metadata.view.widget.bound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.Action.ActionShow;

@SuppressWarnings("static-method")
class ZoomInTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new ZoomIn());
	}

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new ZoomIn().showsLabelByDefault());
	}

	@Test
	void displayNameRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setDisplayName("Open");
		assertEquals("Open", z.getDisplayName());
	}

	@Test
	void displayNameBlankBecomesNull() {
		ZoomIn z = new ZoomIn();
		z.setDisplayName("  ");
		assertNull(z.getDisplayName());
	}

	@Test
	void relativeIconFileNameRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setRelativeIconFileName("icon.png");
		assertEquals("icon.png", z.getRelativeIconFileName());
	}

	@Test
	void iconStyleClassRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setIconStyleClass("fa-search");
		assertEquals("fa-search", z.getIconStyleClass());
	}

	@Test
	void toolTipRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setToolTip("Click to zoom");
		assertEquals("Click to zoom", z.getToolTip());
	}

	@Test
	void toolTipNullByDefault() {
		assertNull(new ZoomIn().getToolTip());
	}

	@Test
	void pixelWidthRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setPixelWidth(Integer.valueOf(200));
		assertEquals(Integer.valueOf(200), z.getPixelWidth());
	}

	@Test
	void pixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setPixelHeight(Integer.valueOf(400));
		assertEquals(Integer.valueOf(400), z.getPixelHeight());
	}

	@Test
	void minPixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setMinPixelHeight(Integer.valueOf(100));
		assertEquals(Integer.valueOf(100), z.getMinPixelHeight());
	}

	@Test
	void maxPixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setMaxPixelHeight(Integer.valueOf(600));
		assertEquals(Integer.valueOf(600), z.getMaxPixelHeight());
	}

	@Test
	void showRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setShow(ActionShow.icon);
		assertEquals(ActionShow.icon, z.getShow());
	}

	@Test
	void showNullByDefault() {
		assertNull(new ZoomIn().getShow());
	}
}

package org.skyve.impl.metadata.view.widget.bound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings({"static-method", "boxing"})
class ProgressBarTest {

	@Test
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new ProgressBar().showsLabelByDefault());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new ProgressBar().getProperties());
	}

	@Test
	void pixelWidthRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setPixelWidth(200);
		assertEquals(200, pb.getPixelWidth());
	}

	@Test
	void minPixelWidthRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setMinPixelWidth(50);
		assertEquals(50, pb.getMinPixelWidth());
	}

	@Test
	void maxPixelWidthRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setMaxPixelWidth(500);
		assertEquals(500, pb.getMaxPixelWidth());
	}

	@Test
	void pixelHeightRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setPixelHeight(20);
		assertEquals(20, pb.getPixelHeight());
	}

	@Test
	void minPixelHeightRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setMinPixelHeight(10);
		assertEquals(10, pb.getMinPixelHeight());
	}

	@Test
	void maxPixelHeightRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setMaxPixelHeight(40);
		assertEquals(40, pb.getMaxPixelHeight());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setInvisibleConditionName("hidden");
		assertEquals("hidden", pb.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegated() {
		ProgressBar pb = new ProgressBar();
		pb.setVisibleConditionName("visible");
		assertEquals("notVisible", pb.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameBlankBecomesNullInvisible() {
		ProgressBar pb = new ProgressBar();
		pb.setVisibleConditionName("  ");
		assertNull(pb.getInvisibleConditionName());
	}

	@Test
	void bindingRoundTrip() {
		ProgressBar pb = new ProgressBar();
		pb.setBinding("progress");
		assertEquals("progress", pb.getBinding());
		assertEquals("progress", pb.getSource());
	}

	@Test
	void visibleConditionNameJaxbGetterReturnsNull() {
		assertNull(new ProgressBar().getVisibleConditionName());
	}
}

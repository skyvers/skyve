package org.skyve.impl.metadata.view.widget.bound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.Action.ActionShow;

@SuppressWarnings({"static-method", "boxing"})
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
		z.setPixelWidth(200);
		assertEquals(200, z.getPixelWidth());
	}

	@Test
	void pixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setPixelHeight(400);
		assertEquals(400, z.getPixelHeight());
	}

	@Test
	void minPixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setMinPixelHeight(100);
		assertEquals(100, z.getMinPixelHeight());
	}

	@Test
	void maxPixelHeightRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setMaxPixelHeight(600);
		assertEquals(600, z.getMaxPixelHeight());
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

	@Test
	void bindingRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setBinding("contact");
		assertEquals("contact", z.getBinding());
		assertEquals("contact", z.getSource());
	}

	@Test
	void bindingBlankBecomesNull() {
		ZoomIn z = new ZoomIn();
		z.setBinding("  ");
		assertNull(z.getBinding());
		assertNull(z.getSource());
	}

	@Test
	void disabledConditionNameRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setDisabledConditionName("readonly");
		assertEquals("readonly", z.getDisabledConditionName());
	}

	@Test
	void enabledConditionNameNegated() {
		ZoomIn z = new ZoomIn();
		z.setEnabledConditionName("editable");
		assertEquals("notEditable", z.getDisabledConditionName());
	}

	@Test
	void enabledConditionNameNotPrefixedNegated() {
		ZoomIn z = new ZoomIn();
		z.setEnabledConditionName("notReady");
		assertEquals("ready", z.getDisabledConditionName());
	}

	@Test
	void enabledConditionNameBlankBecomesNullDisabled() {
		ZoomIn z = new ZoomIn();
		z.setEnabledConditionName("  ");
		assertNull(z.getDisabledConditionName());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setInvisibleConditionName("hidden");
		assertEquals("hidden", z.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegated() {
		ZoomIn z = new ZoomIn();
		z.setVisibleConditionName("visible");
		assertEquals("notVisible", z.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameBlankBecomesNullInvisible() {
		ZoomIn z = new ZoomIn();
		z.setVisibleConditionName("  ");
		assertNull(z.getInvisibleConditionName());
	}

	@Test
	void localisedDisplayNameNullWhenDisplayNameNull() {
		ZoomIn z = new ZoomIn();
		assertNotNull(z.getLocalisedDisplayName());
	}

	@Test
	void localisedDisplayNameWithDisplayNameSet() {
		ZoomIn z = new ZoomIn();
		z.setDisplayName("Zoom In");
		assertNotNull(z.getLocalisedDisplayName());
	}

	@Test
	void localisedToolTipReturnsNonNull() {
		ZoomIn z = new ZoomIn();
		z.setToolTip("Open related record");
		assertNotNull(z.getLocalisedToolTip());
	}

	@Test
	void enabledConditionNameJaxbGetterReturnsNull() {
		assertNull(new ZoomIn().getEnabledConditionName());
	}

	@Test
	void visibleConditionNameJaxbGetterReturnsNull() {
		assertNull(new ZoomIn().getVisibleConditionName());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new ZoomIn().getProperties());
	}
}

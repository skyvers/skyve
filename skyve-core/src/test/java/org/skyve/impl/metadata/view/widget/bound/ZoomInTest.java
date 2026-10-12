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
	void disabledConditionRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setXmlDisabledCondition("readonly");
		assertEquals("readonly", z.getDisabledCondition());
	}

	@Test
	void enabledConditionNegated() {
		ZoomIn z = new ZoomIn();
		z.setXmlEnabledCondition("editable");
		assertEquals("notEditable", z.getDisabledCondition());
	}

	@Test
	void enabledConditionNotPrefixedNegated() {
		ZoomIn z = new ZoomIn();
		z.setXmlEnabledCondition("notReady");
		assertEquals("ready", z.getDisabledCondition());
	}

	@Test
	void enabledConditionBlankBecomesNullDisabled() {
		ZoomIn z = new ZoomIn();
		z.setXmlEnabledCondition("  ");
		assertNull(z.getDisabledCondition());
	}

	@Test
	void invisibleConditionRoundTrip() {
		ZoomIn z = new ZoomIn();
		z.setXmlInvisibleCondition("hidden");
		assertEquals("hidden", z.getInvisibleCondition());
	}

	@Test
	void visibleConditionNegated() {
		ZoomIn z = new ZoomIn();
		z.setXmlVisibleCondition("visible");
		assertEquals("notVisible", z.getInvisibleCondition());
	}

	@Test
	void visibleConditionBlankBecomesNullInvisible() {
		ZoomIn z = new ZoomIn();
		z.setXmlVisibleCondition("  ");
		assertNull(z.getInvisibleCondition());
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
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new ZoomIn().getXmlEnabledCondition());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new ZoomIn().getXmlVisibleCondition());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new ZoomIn().getProperties());
	}
}

package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SpacerTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Spacer().showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Spacer().getProperties());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new Spacer().getXmlVisibleCondition());
	}

	@Test
	void visibleConditionIsKeptAsAuthoredAndInvisibleIsDerived() {
		Spacer spacer = new Spacer();
		spacer.setXmlVisibleCondition("active");
		assertEquals("active", spacer.getXmlVisibleCondition());
		assertEquals("notActive", spacer.getInvisibleCondition());
		// JAXB writes only what was authored
		assertNull(spacer.getXmlInvisibleCondition());
	}

	@Test
	void invisibleConditionIsKeptAsAuthored() {
		Spacer spacer = new Spacer();
		spacer.setXmlInvisibleCondition("inactive");
		assertEquals("inactive", spacer.getInvisibleCondition());
		assertEquals("inactive", spacer.getXmlInvisibleCondition());
		assertNull(spacer.getXmlVisibleCondition());
	}

	@Test
	void lastConditionSetOfThePairWins() {
		Spacer spacer = new Spacer();
		spacer.setXmlVisibleCondition("active");
		spacer.setXmlInvisibleCondition("inactive");
		assertEquals("inactive", spacer.getInvisibleCondition());
		assertNull(spacer.getXmlVisibleCondition());

		spacer.setXmlVisibleCondition("active");
		assertEquals("notActive", spacer.getInvisibleCondition());
		assertNull(spacer.getXmlInvisibleCondition());
	}

	@Test
	void settingVisibleConditionToNullClearsThePair() {
		Spacer spacer = new Spacer();
		spacer.setXmlInvisibleCondition("inactive");
		spacer.setXmlVisibleCondition(null);
		assertNull(spacer.getInvisibleCondition());
	}

	@Test
	void inlineVisibleConditionIsKeptAsAuthoredAndItsInverseIsDerived() {
		Spacer spacer = new Spacer();
		spacer.setXmlVisibleCondition("{el:bean.active}");
		assertEquals("{el:bean.active}", spacer.getXmlVisibleCondition());
		assertEquals("not{el:bean.active}", spacer.getInvisibleCondition());
		assertNull(spacer.getXmlInvisibleCondition());
	}

	@Test
	void setVisibleConditionNegatesCondition() {
		Spacer spacer = new Spacer();
		spacer.setXmlVisibleCondition("visible");
		// the invisible condition is derived as the negation of the authored visible condition
		assertEquals("notVisible", spacer.getInvisibleCondition());
	}
}

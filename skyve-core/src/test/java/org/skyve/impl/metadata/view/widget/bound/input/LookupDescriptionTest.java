package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class LookupDescriptionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new LookupDescription());
	}

	@Test
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new LookupDescription().showsLabelByDefault());
	}

	@Test
	void descriptionBindingRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDescriptionBinding("description");
		assertEquals("description", ld.getDescriptionBinding());
	}

	@Test
	void queryRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setQuery("qContacts");
		assertEquals("qContacts", ld.getQuery());
	}

	@Test
	void queryNullByDefault() {
		assertNull(new LookupDescription().getQuery());
	}

	@Test
	void disableEditConditionNameRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDisableEditConditionName("noEdit");
		assertEquals("noEdit", ld.getDisableEditConditionName());
	}

	@Test
	void enableEditConditionNameNegatesAndStoresAsDisableEdit() {
		LookupDescription ld = new LookupDescription();
		ld.setEnableEditConditionName("canEdit");
		assertEquals("notCanEdit", ld.getDisableEditConditionName());
	}

	@Test
	void disableAddConditionNameRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDisableAddConditionName("noAdd");
		assertEquals("noAdd", ld.getDisableAddConditionName());
	}

	@Test
	void enableAddConditionNameNegatesAndStoresAsDisableAdd() {
		LookupDescription ld = new LookupDescription();
		ld.setEnableAddConditionName("canAdd");
		assertEquals("notCanAdd", ld.getDisableAddConditionName());
	}

	@Test
	void disableClearConditionNameRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDisableClearConditionName("noClear");
		assertEquals("noClear", ld.getDisableClearConditionName());
	}

	@Test
	void enableClearConditionNameNegatesAndStoresAsDisableClear() {
		LookupDescription ld = new LookupDescription();
		ld.setEnableClearConditionName("canClear");
		assertEquals("notCanClear", ld.getDisableClearConditionName());
	}

	@Test
	void disablePickConditionNameRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDisablePickConditionName("noPick");
		assertEquals("noPick", ld.getDisablePickConditionName());
	}

	@Test
	void enablePickConditionNameNegatesAndStoresAsDisablePick() {
		LookupDescription ld = new LookupDescription();
		ld.setEnablePickConditionName("canPick");
		assertEquals("notCanPick", ld.getDisablePickConditionName());
	}

	@Test
	void editableRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setEditable(Boolean.FALSE);
		assertEquals(Boolean.FALSE, ld.getEditable());
	}

	@Test
	void pixelWidthRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setPixelWidth(Integer.valueOf(300));
		assertEquals(Integer.valueOf(300), ld.getPixelWidth());
	}

	@Test
	void dropDownColumnsListNonNull() {
		assertNotNull(new LookupDescription().getDropDownColumns());
	}

	@Test
	void filterParametersListNonNull() {
		assertNotNull(new LookupDescription().getFilterParameters());
	}

	@Test
	void parametersListNonNull() {
		assertNotNull(new LookupDescription().getParameters());
	}

	@Test
	void pickedActionsListNonNull() {
		assertNotNull(new LookupDescription().getPickedActions());
	}

	@Test
	void clearedActionsListNonNull() {
		assertNotNull(new LookupDescription().getClearedActions());
	}

	@Test
	void editedActionsListNonNull() {
		assertNotNull(new LookupDescription().getEditedActions());
	}

	@Test
	void addedActionsListNonNull() {
		assertNotNull(new LookupDescription().getAddedActions());
	}

	@Test
	void propertiesMapNonNull() {
		assertNotNull(new LookupDescription().getProperties());
	}
}

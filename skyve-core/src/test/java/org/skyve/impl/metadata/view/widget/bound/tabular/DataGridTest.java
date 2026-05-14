package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DataGridTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new DataGrid());
	}

	@Test
	void inlineRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setInline(Boolean.TRUE);
		assertEquals(Boolean.TRUE, dg.getInline());
	}

	@Test
	void inlineNullByDefault() {
		assertNull(new DataGrid().getInline());
	}

	@Test
	void editableRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setEditable(Boolean.TRUE);
		assertEquals(Boolean.TRUE, dg.getEditable());
	}

	@Test
	void wordWrapRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setWordWrap(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dg.getWordWrap());
	}

	@Test
	void columnsListNonNull() {
		assertNotNull(new DataGrid().getColumns());
	}

	@Test
	void disabledConditionNameRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setDisabledConditionName("notEditable");
		assertEquals("notEditable", dg.getDisabledConditionName());
	}

	@Test
	void enabledConditionNameNegatesAndStoresAsDisabled() {
		DataGrid dg = new DataGrid();
		dg.setEnabledConditionName("editable");
		assertEquals("notEditable", dg.getDisabledConditionName());
	}

	@Test
	void disableAddConditionNameRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setDisableAddConditionName("noAdd");
		assertEquals("noAdd", dg.getDisableAddConditionName());
	}

	@Test
	void enableAddConditionNameNegatesAndStoresAsDisableAdd() {
		DataGrid dg = new DataGrid();
		dg.setEnableAddConditionName("canAdd");
		assertEquals("notCanAdd", dg.getDisableAddConditionName());
	}

	@Test
	void disableZoomConditionNameRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setDisableZoomConditionName("noZoom");
		assertEquals("noZoom", dg.getDisableZoomConditionName());
	}

	@Test
	void enableZoomConditionNameNegatesAndStoresAsDisableZoom() {
		DataGrid dg = new DataGrid();
		dg.setEnableZoomConditionName("canZoom");
		assertEquals("notCanZoom", dg.getDisableZoomConditionName());
	}

	@Test
	void disableEditConditionNameRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setDisableEditConditionName("noEdit");
		assertEquals("noEdit", dg.getDisableEditConditionName());
	}

	@Test
	void enableEditConditionNameNegatesAndStoresAsDisableEdit() {
		DataGrid dg = new DataGrid();
		dg.setEnableEditConditionName("canEdit");
		assertEquals("notCanEdit", dg.getDisableEditConditionName());
	}

	@Test
	void disableRemoveConditionNameRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setDisableRemoveConditionName("noRemove");
		assertEquals("noRemove", dg.getDisableRemoveConditionName());
	}

	@Test
	void enableRemoveConditionNameNegatesAndStoresAsDisableRemove() {
		DataGrid dg = new DataGrid();
		dg.setEnableRemoveConditionName("canRemove");
		assertEquals("notCanRemove", dg.getDisableRemoveConditionName());
	}

	@Test
	void showAddRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setShowAdd(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dg.getShowAdd());
	}

	@Test
	void showZoomRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setShowZoom(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dg.getShowZoom());
	}

	@Test
	void showEditRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setShowEdit(Boolean.TRUE);
		assertEquals(Boolean.TRUE, dg.getShowEdit());
	}

	@Test
	void showRemoveRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setShowRemove(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dg.getShowRemove());
	}

	@Test
	void showDeselectRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setShowDeselect(Boolean.TRUE);
		assertEquals(Boolean.TRUE, dg.getShowDeselect());
	}

	@Test
	void selectedIdBindingRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setSelectedIdBinding("selectedId");
		assertEquals("selectedId", dg.getSelectedIdBinding());
	}

	@Test
	void addedActionsListNonNull() {
		assertNotNull(new DataGrid().getAddedActions());
	}

	@Test
	void editedActionsListNonNull() {
		assertNotNull(new DataGrid().getEditedActions());
	}

	@Test
	void removedActionsListNonNull() {
		assertNotNull(new DataGrid().getRemovedActions());
	}

	@Test
	void selectedActionsListNonNull() {
		assertNotNull(new DataGrid().getSelectedActions());
	}

	@Test
	void propertiesMapNonNull() {
		assertNotNull(new DataGrid().getProperties());
	}
}

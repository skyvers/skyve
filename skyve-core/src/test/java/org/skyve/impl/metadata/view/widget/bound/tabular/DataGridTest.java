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
	void disabledConditionRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setXmlDisabledCondition("notEditable");
		assertEquals("notEditable", dg.getDisabledCondition());
	}

	@Test
	void enabledConditionNegatesToDisabledCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlEnabledCondition("editable");
		assertEquals("notEditable", dg.getDisabledCondition());
	}

	@Test
	void disableAddConditionRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setXmlDisableAddCondition("noAdd");
		assertEquals("noAdd", dg.getDisableAddCondition());
	}

	@Test
	void enableAddConditionNegatesToDisableAddCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlEnableAddCondition("canAdd");
		assertEquals("notCanAdd", dg.getDisableAddCondition());
	}

	@Test
	void disableZoomConditionRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setXmlDisableZoomCondition("noZoom");
		assertEquals("noZoom", dg.getDisableZoomCondition());
	}

	@Test
	void enableZoomConditionNegatesToDisableZoomCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlEnableZoomCondition("canZoom");
		assertEquals("notCanZoom", dg.getDisableZoomCondition());
	}

	@Test
	void disableEditConditionRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setXmlDisableEditCondition("noEdit");
		assertEquals("noEdit", dg.getDisableEditCondition());
	}

	@Test
	void enableEditConditionNegatesToDisableEditCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlEnableEditCondition("canEdit");
		assertEquals("notCanEdit", dg.getDisableEditCondition());
	}

	@Test
	void disableRemoveConditionRoundTrip() {
		DataGrid dg = new DataGrid();
		dg.setXmlDisableRemoveCondition("noRemove");
		assertEquals("noRemove", dg.getDisableRemoveCondition());
	}

	@Test
	void enableRemoveConditionNegatesToDisableRemoveCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlEnableRemoveCondition("canRemove");
		assertEquals("notCanRemove", dg.getDisableRemoveCondition());
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

	// ---- AbstractDataWidget coverage via DataGrid ----

	@Test
	void setTitleAndGet() {
		DataGrid dg = new DataGrid();
		dg.setTitle("Grid Title");
		assertEquals("Grid Title", dg.getTitle());
	}

	@Test
	void blankTitleBecomesNull() {
		DataGrid dg = new DataGrid();
		dg.setTitle("  ");
		assertNull(dg.getTitle());
	}

	@Test
	void getLocalisedTitleNullWhenTitleNull() {
		assertNull(new DataGrid().getLocalisedTitle());
	}

	@Test
	void setWidgetIdAndGet() {
		DataGrid dg = new DataGrid();
		dg.setWidgetId("gridWidget1");
		assertEquals("gridWidget1", dg.getWidgetId());
	}

	@Test
	void setInvisibleConditionAndGet() {
		DataGrid dg = new DataGrid();
		dg.setXmlInvisibleCondition("hiddenWhen");
		assertEquals("hiddenWhen", dg.getInvisibleCondition());
	}

	@Test
	void setVisibleConditionNegatesCondition() {
		DataGrid dg = new DataGrid();
		dg.setXmlVisibleCondition("showWhen");
		assertEquals("notShowWhen", dg.getInvisibleCondition());
	}

	@Test
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new DataGrid().getXmlEnabledCondition());
	}

	@Test
	void xmlEnableAddConditionIsNullByDefault() {
		assertNull(new DataGrid().getXmlEnableAddCondition());
	}

	@Test
	void xmlEnableZoomConditionIsNullByDefault() {
		assertNull(new DataGrid().getXmlEnableZoomCondition());
	}

	@Test
	void xmlEnableEditConditionIsNullByDefault() {
		assertNull(new DataGrid().getXmlEnableEditCondition());
	}

	@Test
	void xmlEnableRemoveConditionIsNullByDefault() {
		assertNull(new DataGrid().getXmlEnableRemoveCondition());
	}
}

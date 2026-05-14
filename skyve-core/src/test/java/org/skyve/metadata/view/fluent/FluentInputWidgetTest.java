package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FluentInputWidgetTest {

	// ---- FluentCheckBox -----------------------------------------------------

	@Test
	void fluentCheckBoxDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentCheckBox().get());
	}

	@Test
	void fluentCheckBoxTriStateTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentCheckBox().triState(true).get().getTriState()));
	}

	@Test
	void fluentCheckBoxTriStateFalseSetsFalse() {
		assertFalse(Boolean.TRUE.equals(new FluentCheckBox().triState(false).get().getTriState()));
	}

	@Test
	void fluentCheckBoxPixelWidthSetsValue() {
		assertEquals(40, new FluentCheckBox().pixelWidth(40).get().getPixelWidth().intValue());
	}

	@Test
	void fluentCheckBoxPixelHeightSetsValue() {
		assertEquals(40, new FluentCheckBox().pixelHeight(40).get().getPixelHeight().intValue());
	}

	// ---- FluentCombo --------------------------------------------------------

	@Test
	void fluentComboDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentCombo().get());
	}

	@Test
	void fluentComboPixelWidthSetsValue() {
		assertEquals(300, new FluentCombo().pixelWidth(300).get().getPixelWidth().intValue());
	}

	// ---- FluentDataGrid -----------------------------------------------------

	@Test
	void fluentDataGridDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentDataGrid().get());
	}

	@Test
	void fluentDataGridDisabledConditionSetsValue() {
		assertEquals("notReady", new FluentDataGrid().disabledConditionName("notReady").get().getDisabledConditionName());
	}

	@Test
	void fluentDataGridDisableAddConditionSetsValue() {
		assertEquals("noAdd", new FluentDataGrid().disableAddConditionName("noAdd").get().getDisableAddConditionName());
	}

	@Test
	void fluentDataGridShowAddTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentDataGrid().showAdd(true).get().getShowAdd()));
	}

	@Test
	void fluentDataGridShowRemoveFalseSetsFalse() {
		assertFalse(Boolean.TRUE.equals(new FluentDataGrid().showRemove(false).get().getShowRemove()));
	}

	@Test
	void fluentDataGridSelectedIdBindingSetsValue() {
		assertEquals("{id}", new FluentDataGrid().selectedIdBinding("{id}").get().getSelectedIdBinding());
	}

	@Test
	void fluentDataGridInlineTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentDataGrid().inline(true).get().getInline()));
	}

	@Test
	void fluentDataGridEditableTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentDataGrid().editable(true).get().getEditable()));
	}

	@Test
	void fluentDataGridWordWrapTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentDataGrid().wordWrap(true).get().getWordWrap()));
	}

	// ---- FluentListGrid -----------------------------------------------------

	@Test
	void fluentListGridDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentListGrid().get());
	}

	@Test
	void fluentListGridTitleSetsValue() {
		assertEquals("Users", new FluentListGrid().title("Users").get().getTitle());
	}

	@Test
	void fluentListGridQueryNameSetsValue() {
		assertEquals("qUsers", new FluentListGrid().queryName("qUsers").get().getQueryName());
	}

	@Test
	void fluentListGridModelNameSetsValue() {
		assertEquals("UserModel", new FluentListGrid().modelName("UserModel").get().getModelName());
	}

	@Test
	void fluentListGridShowAddTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentListGrid().showAdd(true).get().getShowAdd()));
	}

	@Test
	void fluentListGridShowExportTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentListGrid().showExport(true).get().getShowExport()));
	}

	@Test
	void fluentListGridDisabledConditionSetsValue() {
		assertEquals("locked", new FluentListGrid().disabledConditionName("locked").get().getDisabledConditionName());
	}

	// ---- FluentBlurb --------------------------------------------------------

	@Test
	void fluentBlurbDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentBlurb().get());
	}

	// ---- FluentHTML ---------------------------------------------------------

	@Test
	void fluentHTMLDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentHTML().get());
	}

	// ---- FluentSidebar -------------------------------------------------------

	@Test
	void fluentSidebarDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentSidebar().get());
	}
}

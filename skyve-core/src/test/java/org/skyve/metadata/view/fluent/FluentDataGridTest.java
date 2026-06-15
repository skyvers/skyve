package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;

@SuppressWarnings("static-method")
class FluentDataGridTest {

	@Test
	void defaultConstructorCreatesGrid() {
		assertThat(new FluentDataGrid().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorUsesGrid() {
		DataGrid dg = new DataGrid();
		assertEquals(dg, new FluentDataGrid(dg).get());
	}

	@Test
	void disabledConditionNameSetsValue() {
		assertThat(new FluentDataGrid().disabledConditionName("cond").get().getDisabledConditionName(), is("cond"));
	}

	@Test
	void disableAddConditionNameSetsValue() {
		assertThat(new FluentDataGrid().disableAddConditionName("noAdd").get().getDisableAddConditionName(), is("noAdd"));
	}

	@Test
	void disableZoomConditionNameSetsValue() {
		assertThat(new FluentDataGrid().disableZoomConditionName("noZoom").get().getDisableZoomConditionName(), is("noZoom"));
	}

	@Test
	void disableEditConditionNameSetsValue() {
		assertThat(new FluentDataGrid().disableEditConditionName("noEdit").get().getDisableEditConditionName(), is("noEdit"));
	}

	@Test
	void disableRemoveConditionNameSetsValue() {
		assertThat(new FluentDataGrid().disableRemoveConditionName("noRemove").get().getDisableRemoveConditionName(), is("noRemove"));
	}

	@Test
	void showAddSetsTrue() {
		assertThat(new FluentDataGrid().showAdd(true).get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void showAddSetsFalse() {
		assertThat(new FluentDataGrid().showAdd(false).get().getShowAdd(), is(Boolean.FALSE));
	}

	@Test
	void showEditSetsTrue() {
		assertThat(new FluentDataGrid().showEdit(true).get().getShowEdit(), is(Boolean.TRUE));
	}

	@Test
	void showZoomSetsTrue() {
		assertThat(new FluentDataGrid().showZoom(true).get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void showRemoveSetsTrue() {
		assertThat(new FluentDataGrid().showRemove(true).get().getShowRemove(), is(Boolean.TRUE));
	}

	@Test
	void showDeselectSetsTrue() {
		assertThat(new FluentDataGrid().showDeselect(true).get().getShowDeselect(), is(Boolean.TRUE));
	}

	@Test
	void selectedIdBindingSetsValue() {
		assertThat(new FluentDataGrid().selectedIdBinding("sel").get().getSelectedIdBinding(), is("sel"));
	}

	@Test
	void inlineSetsTrue() {
		assertThat(new FluentDataGrid().inline(true).get().getInline(), is(Boolean.TRUE));
	}

	@Test
	void editableSetsTrue() {
		assertThat(new FluentDataGrid().editable(true).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void wordWrapSetsTrue() {
		assertThat(new FluentDataGrid().wordWrap(true).get().getWordWrap(), is(Boolean.TRUE));
	}

	@Test
	void addBoundColumnAddsToColumns() {
		FluentDataGrid dg = new FluentDataGrid().addBoundColumn(new FluentDataGridBoundColumn());
		assertEquals(1, dg.get().getColumns().size());
	}

	@Test
	void addBoundColumnAtIndexInserts() {
		FluentDataGrid dg = new FluentDataGrid()
				.addBoundColumn(new FluentDataGridBoundColumn())
				.addBoundColumn(0, new FluentDataGridBoundColumn());
		assertEquals(2, dg.get().getColumns().size());
	}

	@Test
	void getBoundColumnReturnsColumn() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		FluentDataGrid dg = new FluentDataGrid().addBoundColumn(col);
		assertThat(dg.getBoundColumn(0), is(notNullValue()));
	}

	@Test
	void addContainerColumnAddsToColumns() {
		FluentDataGrid dg = new FluentDataGrid().addContainerColumn(new FluentDataGridContainerColumn());
		assertEquals(1, dg.get().getColumns().size());
	}

	@Test
	void addContainerColumnAtIndexInserts() {
		FluentDataGrid dg = new FluentDataGrid()
				.addContainerColumn(new FluentDataGridContainerColumn())
				.addContainerColumn(0, new FluentDataGridContainerColumn());
		assertEquals(2, dg.get().getColumns().size());
	}

	@Test
	void getContainerColumnReturnsColumn() {
		FluentDataGrid dg = new FluentDataGrid().addContainerColumn(new FluentDataGridContainerColumn());
		assertThat(dg.getContainerColumn(0), is(notNullValue()));
	}

	@Test
	void removeColumnRemovesColumn() {
		FluentDataGrid dg = new FluentDataGrid().addBoundColumn(new FluentDataGridBoundColumn()).removeColumn(0);
		assertEquals(0, dg.get().getColumns().size());
	}

	@Test
	void clearColumnsRemovesAll() {
		FluentDataGrid dg = new FluentDataGrid()
				.addBoundColumn(new FluentDataGridBoundColumn())
				.addBoundColumn(new FluentDataGridBoundColumn())
				.clearColumns();
		assertEquals(0, dg.get().getColumns().size());
	}

	@Test
	void addAddedActionAddsAction() {
		FluentDataGrid dg = new FluentDataGrid().addAddedAction(new FluentRerenderEventAction());
		assertEquals(1, dg.get().getAddedActions().size());
	}

	@Test
	void clearAddedActionsRemovesAll() {
		FluentDataGrid dg = new FluentDataGrid()
				.addAddedAction(new FluentRerenderEventAction())
				.clearAddedActions();
		assertEquals(0, dg.get().getAddedActions().size());
	}

	@Test
	void addEditedActionAddsAction() {
		FluentDataGrid dg = new FluentDataGrid().addEditedAction(new FluentRerenderEventAction());
		assertEquals(1, dg.get().getEditedActions().size());
	}

	@Test
	void clearEditedActionsRemovesAll() {
		FluentDataGrid dg = new FluentDataGrid()
				.addEditedAction(new FluentRerenderEventAction())
				.clearEditedActions();
		assertEquals(0, dg.get().getEditedActions().size());
	}

	@Test
	void addRemovedActionAddsAction() {
		FluentDataGrid dg = new FluentDataGrid().addRemovedAction(new FluentRerenderEventAction());
		assertEquals(1, dg.get().getRemovedActions().size());
	}

	@Test
	void clearRemovedActionsRemovesAll() {
		FluentDataGrid dg = new FluentDataGrid()
				.addRemovedAction(new FluentRerenderEventAction())
				.clearRemovedActions();
		assertEquals(0, dg.get().getRemovedActions().size());
	}

	@Test
	void addSelectedActionAddsAction() {
		FluentDataGrid dg = new FluentDataGrid().addSelectedAction(new FluentRerenderEventAction());
		assertEquals(1, dg.get().getSelectedActions().size());
	}

	@Test
	void clearSelectedActionsRemovesAll() {
		FluentDataGrid dg = new FluentDataGrid()
				.addSelectedAction(new FluentRerenderEventAction())
				.clearSelectedActions();
		assertEquals(0, dg.get().getSelectedActions().size());
	}

	@Test
	void fromCopiesDisabledConditionName() {
		DataGrid src = new DataGrid();
		src.setDisabledConditionName("cond");
		assertThat(new FluentDataGrid().from(src).get().getDisabledConditionName(), is("cond"));
	}

	@Test
	void fromCopiesShowAdd() {
		DataGrid src = new DataGrid();
		src.setShowAdd(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowEdit() {
		DataGrid src = new DataGrid();
		src.setShowEdit(Boolean.FALSE);
		assertThat(new FluentDataGrid().from(src).get().getShowEdit(), is(Boolean.FALSE));
	}

	@Test
	void fromCopiesInline() {
		DataGrid src = new DataGrid();
		src.setInline(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getInline(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesEditable() {
		DataGrid src = new DataGrid();
		src.setEditable(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesSelectedIdBinding() {
		DataGrid src = new DataGrid();
		src.setSelectedIdBinding("binding");
		assertThat(new FluentDataGrid().from(src).get().getSelectedIdBinding(), is("binding"));
	}

	@Test
	void fromCopiesBoundColumns() {
		DataGrid src = new DataGrid();
		src.getColumns().add(new FluentDataGridBoundColumn().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getColumns().size());
	}

	@Test
	void fromCopiesContainerColumns() {
		DataGrid src = new DataGrid();
		src.getColumns().add(new FluentDataGridContainerColumn().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getColumns().size());
	}

	@Test
	void fromCopiesAddedActions() {
		DataGrid src = new DataGrid();
		src.getAddedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getAddedActions().size());
	}

	@Test
	void fromCopiesEditedActions() {
		DataGrid src = new DataGrid();
		src.getEditedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getEditedActions().size());
	}

	@Test
	void fromCopiesRemovedActions() {
		DataGrid src = new DataGrid();
		src.getRemovedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getRemovedActions().size());
	}

	@Test
	void fromCopiesSelectedActions() {
		DataGrid src = new DataGrid();
		src.getSelectedActions().add(new FluentRerenderEventAction().get());
		assertEquals(1, new FluentDataGrid().from(src).get().getSelectedActions().size());
	}

	@Test
	void fromCopiesShowZoom() {
		DataGrid src = new DataGrid();
		src.setShowZoom(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowRemove() {
		DataGrid src = new DataGrid();
		src.setShowRemove(Boolean.FALSE);
		assertThat(new FluentDataGrid().from(src).get().getShowRemove(), is(Boolean.FALSE));
	}

	@Test
	void fromCopiesShowDeselect() {
		DataGrid src = new DataGrid();
		src.setShowDeselect(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getShowDeselect(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesWordWrap() {
		DataGrid src = new DataGrid();
		src.setWordWrap(Boolean.TRUE);
		assertThat(new FluentDataGrid().from(src).get().getWordWrap(), is(Boolean.TRUE));
	}
}

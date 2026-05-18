package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;

/**
 * Tests for {@link FluentListGrid} setters and parameter management.
 */
@SuppressWarnings("static-method")
class FluentListGridTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentListGrid().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ListGrid grid = new ListGrid();
		assertThat(new FluentListGrid(grid).get(), is(grid));
	}

	// ---- string setters ----

	@Test
	void titleSetsValue() {
		assertThat(new FluentListGrid().title("Contacts").get().getTitle(), is("Contacts"));
	}

	@Test
	void queryNameSetsValue() {
		assertThat(new FluentListGrid().queryName("qContacts").get().getQueryName(), is("qContacts"));
	}

	@Test
	void modelNameSetsValue() {
		assertThat(new FluentListGrid().modelName("ContactsModel").get().getModelName(), is("ContactsModel"));
	}

	@Test
	void postRefreshConditionNameSetsValue() {
		assertThat(new FluentListGrid().postRefreshConditionName("afterRefresh").get().getPostRefreshConditionName(),
				is("afterRefresh"));
	}

	@Test
	void invisibleConditionNameSetsValue() {
		assertThat(new FluentListGrid().invisibleConditionName("notAdmin").get().getInvisibleConditionName(),
				is("notAdmin"));
	}

	@Test
	void disabledConditionNameSetsValue() {
		assertThat(new FluentListGrid().disabledConditionName("readOnly").get().getDisabledConditionName(),
				is("readOnly"));
	}

	@Test
	void disableAddConditionNameSetsValue() {
		assertThat(new FluentListGrid().disableAddConditionName("noAdd").get().getDisableAddConditionName(),
				is("noAdd"));
	}

	@Test
	void disableZoomConditionNameSetsValue() {
		assertThat(new FluentListGrid().disableZoomConditionName("noZoom").get().getDisableZoomConditionName(),
				is("noZoom"));
	}

	@Test
	void disableEditConditionNameSetsValue() {
		assertThat(new FluentListGrid().disableEditConditionName("noEdit").get().getDisableEditConditionName(),
				is("noEdit"));
	}

	@Test
	void disableRemoveConditionNameSetsValue() {
		assertThat(new FluentListGrid().disableRemoveConditionName("noRemove").get().getDisableRemoveConditionName(),
				is("noRemove"));
	}

	@Test
	void selectedIdBindingSetsValue() {
		assertThat(new FluentListGrid().selectedIdBinding("selectedId").get().getSelectedIdBinding(),
				is("selectedId"));
	}

	// ---- boolean show-flag setters ----

	@Test
	void showAddSetsTrue() {
		assertThat(new FluentListGrid().showAdd(true).get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void showAddSetsFalse() {
		assertThat(new FluentListGrid().showAdd(false).get().getShowAdd(), is(Boolean.FALSE));
	}

	@Test
	void showEditSetsTrue() {
		assertThat(new FluentListGrid().showEdit(true).get().getShowEdit(), is(Boolean.TRUE));
	}

	@Test
	void showZoomSetsTrue() {
		assertThat(new FluentListGrid().showZoom(true).get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void showRemoveSetsTrue() {
		assertThat(new FluentListGrid().showRemove(true).get().getShowRemove(), is(Boolean.TRUE));
	}

	@Test
	void showDeselectSetsTrue() {
		assertThat(new FluentListGrid().showDeselect(true).get().getShowDeselect(), is(Boolean.TRUE));
	}

	@Test
	void showExportSetsTrue() {
		assertThat(new FluentListGrid().showExport(true).get().getShowExport(), is(Boolean.TRUE));
	}

	@Test
	void showChartSetsTrue() {
		assertThat(new FluentListGrid().showChart(true).get().getShowChart(), is(Boolean.TRUE));
	}

	@Test
	void showFilterSetsTrue() {
		assertThat(new FluentListGrid().showFilter(true).get().getShowFilter(), is(Boolean.TRUE));
	}

	@Test
	void showSummarySetsTrue() {
		assertThat(new FluentListGrid().showSummary(true).get().getShowSummary(), is(Boolean.TRUE));
	}

	@Test
	void showSnapSetsTrue() {
		assertThat(new FluentListGrid().showSnap(true).get().getShowSnap(), is(Boolean.TRUE));
	}

	@Test
	void showTagSetsTrue() {
		assertThat(new FluentListGrid().showTag(true).get().getShowTag(), is(Boolean.TRUE));
	}

	@Test
	void showFlagSetsTrue() {
		assertThat(new FluentListGrid().showFlag(true).get().getShowFlag(), is(Boolean.TRUE));
	}

	@Test
	void autoPopulateSetsTrue() {
		assertThat(new FluentListGrid().autoPopulate(true).get().getAutoPopulate(), is(Boolean.TRUE));
	}

	@Test
	void continueConversationSetsTrue() {
		assertTrue(new FluentListGrid().continueConversation(true).get().getContinueConversation());
	}

	// ---- size setters ----

	@Test
	void pixelWidthSetsValue() {
		assertThat(new FluentListGrid().pixelWidth(600).get().getPixelWidth(), is(Integer.valueOf(600)));
	}

	@Test
	void pixelHeightSetsValue() {
		assertThat(new FluentListGrid().pixelHeight(400).get().getPixelHeight(), is(Integer.valueOf(400)));
	}

	@Test
	void minPixelWidthSetsValue() {
		assertThat(new FluentListGrid().minPixelWidth(200).get().getMinPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	void maxPixelWidthSetsValue() {
		assertThat(new FluentListGrid().maxPixelWidth(800).get().getMaxPixelWidth(), is(Integer.valueOf(800)));
	}

	@Test
	void minPixelHeightSetsValue() {
		assertThat(new FluentListGrid().minPixelHeight(100).get().getMinPixelHeight(), is(Integer.valueOf(100)));
	}

	@Test
	void maxPixelHeightSetsValue() {
		assertThat(new FluentListGrid().maxPixelHeight(600).get().getMaxPixelHeight(), is(Integer.valueOf(600)));
	}

	@Test
	void percentageWidthSetsValue() {
		assertThat(new FluentListGrid().percentageWidth(80).get().getPercentageWidth(), is(Integer.valueOf(80)));
	}

	@Test
	void responsiveWidthSetsValue() {
		assertThat(new FluentListGrid().responsiveWidth(6).get().getResponsiveWidth(), is(Integer.valueOf(6)));
	}

	@Test
	void smSetsValue() {
		assertThat(new FluentListGrid().sm(4).get().getSm(), is(Integer.valueOf(4)));
	}

	@Test
	void mdSetsValue() {
		assertThat(new FluentListGrid().md(6).get().getMd(), is(Integer.valueOf(6)));
	}

	@Test
	void lgSetsValue() {
		assertThat(new FluentListGrid().lg(8).get().getLg(), is(Integer.valueOf(8)));
	}

	@Test
	void xlSetsValue() {
		assertThat(new FluentListGrid().xl(10).get().getXl(), is(Integer.valueOf(10)));
	}

	@Test
	void percentageHeightSetsValue() {
		assertThat(new FluentListGrid().percentageHeight(50).get().getPercentageHeight(), is(Integer.valueOf(50)));
	}

	// ---- parameter management ----

	@Test
	void addParameterAddsParam() {
		FluentListGrid grid = new FluentListGrid().addParameter(new FluentParameter());
		assertEquals(1, grid.get().getParameters().size());
	}

	@Test
	void addFilterParameterAddsFilterParam() {
		FluentListGrid grid = new FluentListGrid().addFilterParameter(new FluentFilterParameter());
		assertEquals(1, grid.get().getFilterParameters().size());
	}

	// ---- event action management ----

	@Test
	void addEditedActionAddsAction() {
		FluentListGrid grid = new FluentListGrid().addEditedAction(new FluentRerenderEventAction());
		assertEquals(1, grid.get().getEditedActions().size());
	}

	@Test
	void addRemovedActionAddsAction() {
		FluentListGrid grid = new FluentListGrid().addRemovedAction(new FluentRerenderEventAction());
		assertEquals(1, grid.get().getRemovedActions().size());
	}

	@Test
	void addSelectedActionAddsAction() {
		FluentListGrid grid = new FluentListGrid().addSelectedAction(new FluentRerenderEventAction());
		assertEquals(1, grid.get().getSelectedActions().size());
	}

	// ---- from() round-trip ----

	@Test
	void fromCopiesTitle() {
		ListGrid source = new ListGrid();
		source.setTitle("My Grid");
		assertThat(new FluentListGrid().from(source).get().getTitle(), is("My Grid"));
	}

	@Test
	void fromCopiesQueryName() {
		ListGrid source = new ListGrid();
		source.setQueryName("qAll");
		assertThat(new FluentListGrid().from(source).get().getQueryName(), is("qAll"));
	}

	@Test
	void fromCopiesShowAddTrue() {
		ListGrid source = new ListGrid();
		source.setShowAdd(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowEditTrue() {
		ListGrid source = new ListGrid();
		source.setShowEdit(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowEdit(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowZoomTrue() {
		ListGrid source = new ListGrid();
		source.setShowZoom(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowRemoveFalse() {
		ListGrid source = new ListGrid();
		source.setShowRemove(Boolean.FALSE);
		assertThat(new FluentListGrid().from(source).get().getShowRemove(), is(Boolean.FALSE));
	}

	@Test
	void fromCopiesShowDeselectTrue() {
		ListGrid source = new ListGrid();
		source.setShowDeselect(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowDeselect(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowExportTrue() {
		ListGrid source = new ListGrid();
		source.setShowExport(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowExport(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowChartTrue() {
		ListGrid source = new ListGrid();
		source.setShowChart(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowChart(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowFilterTrue() {
		ListGrid source = new ListGrid();
		source.setShowFilter(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowFilter(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowSummaryTrue() {
		ListGrid source = new ListGrid();
		source.setShowSummary(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowSummary(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowSnapTrue() {
		ListGrid source = new ListGrid();
		source.setShowSnap(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowSnap(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowTagTrue() {
		ListGrid source = new ListGrid();
		source.setShowTag(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowTag(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesShowFlagTrue() {
		ListGrid source = new ListGrid();
		source.setShowFlag(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getShowFlag(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesAutoPopulateTrue() {
		ListGrid source = new ListGrid();
		source.setAutoPopulate(Boolean.TRUE);
		assertThat(new FluentListGrid().from(source).get().getAutoPopulate(), is(Boolean.TRUE));
	}
}

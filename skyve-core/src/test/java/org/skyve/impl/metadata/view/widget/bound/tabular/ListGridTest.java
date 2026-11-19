package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for ListGrid getter/setter methods.
 */
@SuppressWarnings("static-method")
class ListGridTest {

	// ---- AbstractListWidget coverage (via ListGrid) ----

	@Test
	void setTitleAndGet() {
		ListGrid lg = new ListGrid();
		lg.setTitle("My List");
		assertThat(lg.getTitle(), is("My List"));
	}

	@Test
	void blankTitleBecomesNull() {
		ListGrid lg = new ListGrid();
		lg.setTitle("  ");
		assertNull(lg.getTitle());
	}

	@Test
	void getLocalisedTitleNullWhenTitleNull() {
		assertNull(new ListGrid().getLocalisedTitle());
	}

	@Test
	void setQueryNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setQueryName("myQuery");
		assertThat(lg.getQueryName(), is("myQuery"));
	}

	@Test
	void blankQueryNameBecomesNull() {
		ListGrid lg = new ListGrid();
		lg.setQueryName("  ");
		assertNull(lg.getQueryName());
	}

	@Test
	void setModelNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setModelName("myModel");
		assertThat(lg.getModelName(), is("myModel"));
	}

	@Test
	void setInvisibleConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlInvisibleCondition("hiddenWhen");
		assertThat(lg.getInvisibleCondition(), is("hiddenWhen"));
	}

	@Test
	void setVisibleConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlVisibleCondition("showWhen");
		assertThat(lg.getInvisibleCondition(), is("notShowWhen"));
	}

	// ---- ListGrid specific ----

	@Test
	void continueConversationFalseByDefault() {
		assertFalse(new ListGrid().getContinueConversation());
	}

	@Test
	void setContinueConversationTrue() {
		ListGrid lg = new ListGrid();
		lg.setContinueConversation(true);
		assertTrue(lg.getContinueConversation());
	}

	@Test
	void setDisabledConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlDisabledCondition("isDisabled");
		assertThat(lg.getDisabledCondition(), is("isDisabled"));
	}

	@Test
	void setEnabledConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlEnabledCondition("isEnabled");
		assertThat(lg.getDisabledCondition(), is("notIsEnabled"));
	}

	@Test
	void setDisableAddConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlDisableAddCondition("noAdd");
		assertThat(lg.getDisableAddCondition(), is("noAdd"));
	}

	@Test
	void setEnableAddConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlEnableAddCondition("canAdd");
		assertThat(lg.getDisableAddCondition(), is("notCanAdd"));
	}

	@Test
	void setDisableZoomConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlDisableZoomCondition("noZoom");
		assertThat(lg.getDisableZoomCondition(), is("noZoom"));
	}

	@Test
	void setEnableZoomConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlEnableZoomCondition("canZoom");
		assertThat(lg.getDisableZoomCondition(), is("notCanZoom"));
	}

	@Test
	void setDisableEditConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlDisableEditCondition("noEdit");
		assertThat(lg.getDisableEditCondition(), is("noEdit"));
	}

	@Test
	void setEnableEditConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlEnableEditCondition("canEdit");
		assertThat(lg.getDisableEditCondition(), is("notCanEdit"));
	}

	@Test
	void setDisableRemoveConditionAndGet() {
		ListGrid lg = new ListGrid();
		lg.setXmlDisableRemoveCondition("noRemove");
		assertThat(lg.getDisableRemoveCondition(), is("noRemove"));
	}

	@Test
	void setEnableRemoveConditionNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setXmlEnableRemoveCondition("canRemove");
		assertThat(lg.getDisableRemoveCondition(), is("notCanRemove"));
	}

	@Test
	void setShowAddRoundtrip() {
		ListGrid lg = new ListGrid();
		lg.setShowAdd(Boolean.FALSE);
		assertThat(lg.getShowAdd(), is(Boolean.FALSE));
	}

	@Test
	void setAutoPopulateRoundtrip() {
		ListGrid lg = new ListGrid();
		lg.setAutoPopulate(Boolean.TRUE);
		assertThat(lg.getAutoPopulate(), is(Boolean.TRUE));
	}

	@Test
	void getPropertiesNotNull() {
		assertNotNull(new ListGrid().getProperties());
	}

	@Test
	void getEditedActionsNotNull() {
		assertNotNull(new ListGrid().getEditedActions());
	}

	@Test
	void getRemovedActionsNotNull() {
		assertNotNull(new ListGrid().getRemovedActions());
	}

	@Test
	void getSelectedActionsNotNull() {
		assertNotNull(new ListGrid().getSelectedActions());
	}

	@Test
	void getSourceReturnsModelNameWhenQueryNameIsNull() {
		ListGrid grid = new ListGrid();
		grid.setModelName("summaryModel");
		// queryName is null, so getSource() returns modelName
		assertThat(grid.getSource(), is("summaryModel"));
	}
}

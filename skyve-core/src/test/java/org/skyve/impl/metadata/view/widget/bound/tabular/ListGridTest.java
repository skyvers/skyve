package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
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
	void setInvisibleConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setInvisibleConditionName("hiddenWhen");
		assertThat(lg.getInvisibleConditionName(), is("hiddenWhen"));
	}

	@Test
	void setVisibleConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setVisibleConditionName("showWhen");
		assertThat(lg.getInvisibleConditionName(), startsWith("not"));
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
	void setDisabledConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setDisabledConditionName("isDisabled");
		assertThat(lg.getDisabledConditionName(), is("isDisabled"));
	}

	@Test
	void setEnabledConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setEnabledConditionName("isEnabled");
		assertThat(lg.getDisabledConditionName(), startsWith("not"));
	}

	@Test
	void setDisableAddConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setDisableAddConditionName("noAdd");
		assertThat(lg.getDisableAddConditionName(), is("noAdd"));
	}

	@Test
	void setEnableAddConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setEnableAddConditionName("canAdd");
		assertThat(lg.getDisableAddConditionName(), startsWith("not"));
	}

	@Test
	void setDisableZoomConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setDisableZoomConditionName("noZoom");
		assertThat(lg.getDisableZoomConditionName(), is("noZoom"));
	}

	@Test
	void setEnableZoomConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setEnableZoomConditionName("canZoom");
		assertThat(lg.getDisableZoomConditionName(), startsWith("not"));
	}

	@Test
	void setDisableEditConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setDisableEditConditionName("noEdit");
		assertThat(lg.getDisableEditConditionName(), is("noEdit"));
	}

	@Test
	void setEnableEditConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setEnableEditConditionName("canEdit");
		assertThat(lg.getDisableEditConditionName(), startsWith("not"));
	}

	@Test
	void setDisableRemoveConditionNameAndGet() {
		ListGrid lg = new ListGrid();
		lg.setDisableRemoveConditionName("noRemove");
		assertThat(lg.getDisableRemoveConditionName(), is("noRemove"));
	}

	@Test
	void setEnableRemoveConditionNameNegatesCondition() {
		ListGrid lg = new ListGrid();
		lg.setEnableRemoveConditionName("canRemove");
		assertThat(lg.getDisableRemoveConditionName(), startsWith("not"));
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
}

package org.skyve.impl.metadata.repository.view;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
class ViewRepositoryMetaDataTest {

	// ViewMetaData

	@Test
	void viewNameRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		assertThat(v.getName(), is("edit"));
	}

	@Test
	void viewTitleRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setTitle("Edit Contact");
		assertThat(v.getTitle(), is("Edit Contact"));
	}

	@Test
	void viewIcon32x32RoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setIcon32x32RelativeFileName("images/icon.png");
		assertThat(v.getIcon32x32RelativeFileName(), is("images/icon.png"));
	}

	@Test
	void viewIconStyleClassRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setIconStyleClass("fa-user");
		assertThat(v.getIconStyleClass(), is("fa-user"));
	}

	@Test
	void viewHelpRelativeFileNameRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpRelativeFileName("help/contact.html");
		assertThat(v.getHelpRelativeFileName(), is("help/contact.html"));
	}

	@Test
	void viewHelpURLRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpURL("https://example.com/help");
		assertThat(v.getHelpURL(), is("https://example.com/help"));
	}

	@Test
	void viewRefreshTimeRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshTimeInSeconds(30);
		assertThat(v.getRefreshTimeInSeconds(), is(30));
	}

	@Test
	void viewRefreshConditionRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshConditionName("needsRefresh");
		assertThat(v.getRefreshConditionName(), is("needsRefresh"));
	}

	@Test
	void viewRefreshActionRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshActionName("RefreshAction");
		assertThat(v.getRefreshActionName(), is("RefreshAction"));
	}

	@Test
	void viewDocumentationRoundTrips() {
		ViewMetaData v = new ViewMetaData();
		v.setDocumentation("View docs");
		assertThat(v.getDocumentation(), is("View docs"));
	}

	@Test
	void viewParametersNotNull() {
		ViewMetaData v = new ViewMetaData();
		assertThat(v.getParameters(), notNullValue());
	}

	@Test
	void viewPropertiesNotNull() {
		ViewMetaData v = new ViewMetaData();
		assertThat(v.getProperties(), notNullValue());
	}

	@Test
	void viewBlankNameBecomesNull() {
		ViewMetaData v = new ViewMetaData();
		v.setName("  ");
		assertThat(v.getName(), nullValue());
	}

	// CustomAction (extends ClassAction -> ValidatableAction -> PositionableAction -> ActionMetaData)

	@Test
	void customActionNameRoundTrips() {
		CustomAction a = new CustomAction();
		a.setName("MyAction");
		assertThat(a.getName(), is("MyAction"));
	}

	@Test
	void customActionDisplayNameRoundTrips() {
		CustomAction a = new CustomAction();
		a.setDisplayName("Save Record");
		assertThat(a.getDisplayName(), is("Save Record"));
	}

	@Test
	void customActionClassNameRoundTrips() {
		CustomAction a = new CustomAction();
		a.setClassName("com.example.MyAction");
		assertThat(a.getClassName(), is("com.example.MyAction"));
	}

	@Test
	void customActionToolTipRoundTrips() {
		CustomAction a = new CustomAction();
		a.setToolTip("Click to save");
		assertThat(a.getToolTip(), is("Click to save"));
	}

	@Test
	void customActionConfirmationTextRoundTrips() {
		CustomAction a = new CustomAction();
		a.setConfirmationText("Are you sure?");
		assertThat(a.getConfirmationText(), is("Are you sure?"));
	}

	@Test
	void customActionRelativeIconFileNameRoundTrips() {
		CustomAction a = new CustomAction();
		a.setRelativeIconFileName("icons/save.png");
		assertThat(a.getRelativeIconFileName(), is("icons/save.png"));
	}

	@Test
	void customActionIconStyleClassRoundTrips() {
		CustomAction a = new CustomAction();
		a.setIconStyleClass("fa-save");
		assertThat(a.getIconStyleClass(), is("fa-save"));
	}

	@Test
	void customActionInvisibleConditionRoundTrips() {
		CustomAction a = new CustomAction();
		a.setInvisibleConditionName("notVisible");
		assertThat(a.getInvisibleConditionName(), is("notVisible"));
	}

	@Test
	void customActionDisabledConditionRoundTrips() {
		CustomAction a = new CustomAction();
		a.setDisabledConditionName("isDisabled");
		assertThat(a.getDisabledConditionName(), is("isDisabled"));
	}

	@Test
	void customActionClientValidationRoundTrips() {
		CustomAction a = new CustomAction();
		a.setClientValidation(Boolean.FALSE);
		assertThat(a.getClientValidation(), is(Boolean.FALSE));
	}

	@Test
	void customActionPropertiesNotNull() {
		CustomAction a = new CustomAction();
		assertThat(a.getProperties(), notNullValue());
	}

	@Test
	void customActionHasNoImplicitName() {
		CustomAction a = new CustomAction();
		assertThat(a.getImplicitName(), nullValue());
	}

	// OKAction

	@Test
	void okActionHasImplicitName() {
		OKAction a = new OKAction();
		assertThat(a.getImplicitName(), is(ImplicitActionName.OK));
	}

	@Test
	void okActionClientValidationRoundTrips() {
		OKAction a = new OKAction();
		a.setClientValidation(Boolean.TRUE);
		assertThat(a.getClientValidation(), is(Boolean.TRUE));
	}
}

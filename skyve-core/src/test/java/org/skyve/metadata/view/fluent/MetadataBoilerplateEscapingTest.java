package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.widget.bound.Parameter;

class MetadataBoilerplateEscapingTest {

	@Test
	@SuppressWarnings("static-method")
	void metadataPojoFlagsRoundTripNullableBooleans() {
		ViewMetaData view = new ViewMetaData();
		view.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, view.getEscapeTitle());

		Tab tab = new Tab();
		tab.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, tab.getEscapeTitle());

		VBox vbox = new VBox();
		vbox.setEscapeBorderTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, vbox.getEscapeBorderTitle());

		HBox hbox = new HBox();
		hbox.setEscapeBorderTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, hbox.getEscapeBorderTitle());

		Form form = new Form();
		form.setEscapeBorderTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, form.getEscapeBorderTitle());

		FormItem item = new FormItem();
		item.setEscapeLabel(Boolean.FALSE);
		item.setEscapeRequiredMessage(Boolean.FALSE);
		item.setEscapeHelp(Boolean.FALSE);
		assertEquals(Boolean.FALSE, item.getEscapeLabel());
		assertEquals(Boolean.FALSE, item.getEscapeRequiredMessage());
		assertEquals(Boolean.FALSE, item.getEscapeHelp());

		DataGrid dataGrid = new DataGrid();
		dataGrid.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dataGrid.getEscapeTitle());

		ListGrid listGrid = new ListGrid();
		listGrid.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, listGrid.getEscapeTitle());

		DataGridBoundColumn boundColumn = new DataGridBoundColumn();
		boundColumn.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, boundColumn.getEscapeTitle());

		ZoomIn zoomIn = new ZoomIn();
		zoomIn.setEscapeDisplayName(Boolean.FALSE);
		zoomIn.setEscapeToolTip(Boolean.FALSE);
		assertEquals(Boolean.FALSE, zoomIn.getEscapeDisplayName());
		assertEquals(Boolean.FALSE, zoomIn.getEscapeToolTip());

		DialogButton dialogButton = new DialogButton();
		dialogButton.setEscapeDisplayName(Boolean.FALSE);
		assertEquals(Boolean.FALSE, dialogButton.getEscapeDisplayName());

		ListMembership listMembership = new ListMembership();
		listMembership.setEscapeCandidatesHeading(Boolean.FALSE);
		listMembership.setEscapeMembersHeading(Boolean.FALSE);
		assertEquals(Boolean.FALSE, listMembership.getEscapeCandidatesHeading());
		assertEquals(Boolean.FALSE, listMembership.getEscapeMembersHeading());

		Link link = new Link();
		link.setEscapeValue(Boolean.FALSE);
		assertEquals(Boolean.FALSE, link.getEscapeValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void repositoryConversionCopiesViewAndActionFlags() {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		view.setTitle("Edit");
		view.setEscapeTitle(Boolean.FALSE);
		ViewImpl runtimeView = view.convert("test.edit.xml");
		assertEquals(Boolean.FALSE, runtimeView.getEscapeTitle());

		SaveAction action = new SaveAction();
		action.setEscapeDisplayName(Boolean.FALSE);
		action.setEscapeToolTip(Boolean.TRUE);
		action.setEscapeConfirm(Boolean.FALSE);
		ActionImpl runtimeAction = action.toMetaDataAction();
		assertEquals(Boolean.FALSE, runtimeAction.getEscapeDisplayName());
		assertEquals(Boolean.TRUE, runtimeAction.getEscapeToolTip());
		assertEquals(Boolean.FALSE, runtimeAction.getEscapeConfirm());

		SaveAction roundTrip = (SaveAction) runtimeAction.toRepositoryAction();
		assertEquals(Boolean.FALSE, roundTrip.getEscapeDisplayName());
		assertEquals(Boolean.TRUE, roundTrip.getEscapeToolTip());
		assertEquals(Boolean.FALSE, roundTrip.getEscapeConfirm());
	}

	@Test
	@SuppressWarnings("static-method")
	void publicActionInterfaceDefaultsEscapeFlagsToNull() {
		Action action = new MinimalAction();
		assertEquals(null, action.getEscapeDisplayName());
		assertEquals(null, action.getEscapeToolTip());
		assertEquals(null, action.getEscapeConfirm());
	}

	@Test
	@SuppressWarnings("static-method")
	void fluentBuildersSetAndPreserveEscapeFlags() {
		assertFalse(new FluentView().escapeTitle(false).get().getEscapeTitle().booleanValue());
		assertFalse(new FluentTab().escapeTitle(false).get().getEscapeTitle().booleanValue());
		assertFalse(new FluentVBox().escapeBorderTitle(false).get().getEscapeBorderTitle().booleanValue());
		assertFalse(new FluentHBox().escapeBorderTitle(false).get().getEscapeBorderTitle().booleanValue());
		assertFalse(new FluentForm().escapeBorderTitle(false).get().getEscapeBorderTitle().booleanValue());
		assertFalse(new FluentFormItem()
						.escapeLabel(false)
						.escapeHelp(false)
						.escapeRequiredMessage(false)
						.get()
						.getEscapeRequiredMessage()
						.booleanValue());
		assertFalse(new FluentDataGrid().escapeTitle(false).get().getEscapeTitle().booleanValue());
		assertFalse(new FluentListGrid().escapeTitle(false).get().getEscapeTitle().booleanValue());
		assertFalse(new FluentDataGridBoundColumn().escapeTitle(false).get().getEscapeTitle().booleanValue());
		assertFalse(new BuilderShim().escapeDisplayName(false).get().getEscapeDisplayName().booleanValue());
		assertFalse(new FluentZoomIn().escapeDisplayName(false).escapeToolTip(false).get().getEscapeToolTip().booleanValue());
		assertFalse(new FluentDialogButton().escapeDisplayName(false).get().getEscapeDisplayName().booleanValue());
		assertFalse(new FluentListMembership()
						.escapeCandidatesHeading(false)
						.escapeMembersHeading(false)
						.get()
						.getEscapeMembersHeading()
						.booleanValue());
		assertFalse(new FluentLink().escapeValue(false).get().getEscapeValue().booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void fluentFromPreservesEscapeFlags() {
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		view.setTitle("Edit");
		view.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, new FluentView().from((View) view).get().getEscapeTitle());

		FormItem item = new FormItem();
		item.setEscapeLabel(Boolean.FALSE);
		item.setEscapeHelp(Boolean.TRUE);
		item.setEscapeRequiredMessage(Boolean.FALSE);
		item.setWidget(new Link());
		FormItem copiedItem = new FluentFormItem().from(item).get();
		assertEquals(Boolean.FALSE, copiedItem.getEscapeLabel());
		assertEquals(Boolean.TRUE, copiedItem.getEscapeHelp());
		assertEquals(Boolean.FALSE, copiedItem.getEscapeRequiredMessage());

		DataGridBoundColumn boundColumn = new DataGridBoundColumn();
		boundColumn.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, new FluentDataGridBoundColumn().from(boundColumn).get().getEscapeTitle());

		DataGridContainerColumn containerColumn = new DataGridContainerColumn();
		containerColumn.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, new FluentDataGridContainerColumn().from(containerColumn).get().getEscapeTitle());

		ListRepeater repeater = new ListRepeater();
		repeater.setEscapeTitle(Boolean.FALSE);
		assertEquals(Boolean.FALSE, new FluentListRepeater().from(repeater).get().getEscapeTitle());

		Link link = new Link();
		link.setEscapeValue(Boolean.FALSE);
		assertEquals(Boolean.FALSE, new FluentLink().from(link).get().getEscapeValue());
	}

	@Test
	void generatedViewSchemaContainsEscapeAttributes() throws IOException {
		String schema;
		try (InputStream stream = getClass().getResourceAsStream("/schemas/view.xsd")) {
			assertNotNull(stream);
			schema = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
		}

		assertSchemaContainsEscapeAttributes(schema);
	}

	@Test
	@SuppressWarnings("static-method")
	void generatedWarViewSchemaContainsEscapeAttributes() throws IOException {
		String schema = Files.readString(Path.of("../skyve-war/src/main/java/schemas/view.xsd"), StandardCharsets.UTF_8);
		assertSchemaContainsEscapeAttributes(schema);
	}

	private static void assertSchemaContainsEscapeAttributes(String schema) {
		for (String attribute : new String[] {"escapeTitle", "escapeBorderTitle", "escapeLabel",
													"escapeRequiredMessage", "escapeHelp", "escapeDisplayName",
													"escapeToolTip", "escapeConfirm", "escapeMembersHeading",
													"escapeCandidatesHeading", "escapeValue"}) {
			assertTrue(schema.contains("name=\"" + attribute + "\""), attribute);
		}
	}

	private static final class BuilderShim extends FluentAction<BuilderShim> {
		private final SaveAction action = new SaveAction();

		@Override
		public SaveAction get() {
			return action;
		}
	}

	private static final class MinimalAction implements Action {
		private static final long serialVersionUID = 1L;

		@Override
		public String getName() {
			return "minimal";
		}

		@Override
		public String getDisabledConditionName() {
			return null;
		}

		@Override
		public void setDisabledConditionName(String disabledConditionName) {
			// default-method test only
		}

		@Override
		public void setEnabledConditionName(String disabledConditionName) {
			// default-method test only
		}

		@Override
		public String getInvisibleConditionName() {
			return null;
		}

		@Override
		public void setInvisibleConditionName(String invisibleConditionName) {
			// default-method test only
		}

		@Override
		public void setVisibleConditionName(String visibleConditionName) {
			// default-method test only
		}

		@Override
		public List<Parameter> getParameters() {
			return List.of();
		}

		@Override
		public Map<String, String> getProperties() {
			return Map.of();
		}

		@Override
		public ImplicitActionName getImplicitName() {
			return null;
		}

		@Override
		public String getResourceName() {
			return null;
		}

		@Override
		public Boolean getClientValidation() {
			return null;
		}

		@Override
		public String getDisplayName() {
			return null;
		}

		@Override
		public String getRelativeIconFileName() {
			return null;
		}

		@Override
		public String getIconStyleClass() {
			return null;
		}

		@Override
		public String getConfirmationText() {
			return null;
		}

		@Override
		public String getToolTip() {
			return null;
		}

		@Override
		public Boolean getInActionPanel() {
			return null;
		}

		@Override
		public Action.ActionShow getShow() {
			return Action.ActionShow.both;
		}

		@Override
		public ServerSideAction<?> getServerSideAction(Customer customer, Document document) {
			return null;
		}
	}
}

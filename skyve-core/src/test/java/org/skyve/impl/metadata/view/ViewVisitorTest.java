package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;

import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.controller.ImplicitActionName;

/**
 * Tests for {@link ViewVisitor} and {@link NoOpViewVisitor} traversal logic.
 * Each test populates a {@link ViewImpl} with specific widget/container types
 * and asserts that visiting does not throw.
 */
@SuppressWarnings("static-method")
class ViewVisitorTest {

	/** Concrete subclass of NoOpViewVisitor for testing (constructor is protected). */
	private static final class TestNoOpViewVisitor extends NoOpViewVisitor {
		TestNoOpViewVisitor(CustomerImpl customer, ModuleImpl module, DocumentImpl document, ViewImpl view) {
			super(customer, module, document, view, "desktop");
		}
	}

	private static CustomerImpl mockCustomer() {
		return Mockito.mock(CustomerImpl.class);
	}

	private static ModuleImpl mockModule() {
		return Mockito.mock(ModuleImpl.class);
	}

	/** DocumentImpl is final — pass null (none of these tests dereference the document parameter). */
	private static DocumentImpl nullDocument() {
		return null;
	}

	// ---- Container traversal tests ----

	@Test
	void visitEmptyView() {
		ViewImpl view = new ViewImpl();
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithSidebar() {
		ViewImpl view = new ViewImpl();
		Sidebar sidebar = new Sidebar();
		sidebar.getContained().add(new Button());
		view.setSidebar(sidebar);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithVBox() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.getContained().add(new Button());
		vbox.getContained().add(new Label());
		vbox.getContained().add(new Blurb());
		vbox.getContained().add(new Spacer());
		view.getContained().add(vbox);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithHBox() {
		ViewImpl view = new ViewImpl();
		HBox hbox = new HBox();
		hbox.getContained().add(new Blurb());
		view.getContained().add(hbox);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithTabPaneAndTab() {
		ViewImpl view = new ViewImpl();
		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Test Tab");
		tab.getContained().add(new Button());
		tab.getContained().add(new ZoomIn());
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- Direct container widget tests ----

	@Test
	void visitViewWithDirectWidgets() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new Button());
		view.getContained().add(new Blurb());
		view.getContained().add(new Label());
		view.getContained().add(new Spacer());
		view.getContained().add(new StaticImage());
		view.getContained().add(new DynamicImage());
		view.getContained().add(new Chart());
		view.getContained().add(new MapDisplay());
		view.getContained().add(new Link());
		view.getContained().add(new ZoomIn());
		view.getContained().add(new DialogButton());
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithMembershipAndComparisonWidgets() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new CheckMembership());
		view.getContained().add(new ListMembership());
		view.getContained().add(new Comparison());
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- Form traversal tests ----

	@Test
	void visitFormWithFormColumnAndFormRow() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormColumn column = new FormColumn();
		form.getColumns().add(column);
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		item.setWidget(new Button());
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitFormWithInputWidgets() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		// TextField
		FormItem tfItem = new FormItem();
		tfItem.setWidget(new TextField());
		row.getItems().add(tfItem);
		// CheckBox
		FormItem cbItem = new FormItem();
		cbItem.setWidget(new CheckBox());
		row.getItems().add(cbItem);
		// Combo
		FormItem coItem = new FormItem();
		coItem.setWidget(new Combo());
		row.getItems().add(coItem);
		// Password
		FormItem pwItem = new FormItem();
		pwItem.setWidget(new Password());
		row.getItems().add(pwItem);
		// Radio
		FormItem rdItem = new FormItem();
		rdItem.setWidget(new Radio());
		row.getItems().add(rdItem);
		// RichText
		FormItem rtItem = new FormItem();
		rtItem.setWidget(new RichText());
		row.getItems().add(rtItem);
		// TextArea
		FormItem taItem = new FormItem();
		taItem.setWidget(new TextArea());
		row.getItems().add(taItem);
		// HTML
		FormItem htmlItem = new FormItem();
		htmlItem.setWidget(new HTML());
		row.getItems().add(htmlItem);
		// ContentImage
		FormItem ciItem = new FormItem();
		ciItem.setWidget(new ContentImage());
		row.getItems().add(ciItem);
		// ContentLink
		FormItem clItem = new FormItem();
		clItem.setWidget(new ContentLink());
		row.getItems().add(clItem);
		// ContentSignature
		FormItem csItem = new FormItem();
		csItem.setWidget(new ContentSignature());
		row.getItems().add(csItem);
		// ColourPicker
		FormItem cpItem = new FormItem();
		cpItem.setWidget(new ColourPicker());
		row.getItems().add(cpItem);
		// Slider
		FormItem slItem = new FormItem();
		slItem.setWidget(new Slider());
		row.getItems().add(slItem);
		// Spinner
		FormItem spItem = new FormItem();
		spItem.setWidget(new Spinner());
		row.getItems().add(spItem);
		// ProgressBar (in FormItem only)
		FormItem pbItem = new FormItem();
		pbItem.setWidget(new ProgressBar());
		row.getItems().add(pbItem);
		form.getRows().add(row);
		view.getContained().add(form);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitFormWithLookupAndGeometryWidgets() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		// LookupDescription
		FormItem ldItem = new FormItem();
		LookupDescription lookup = new LookupDescription();
		ldItem.setWidget(lookup);
		row.getItems().add(ldItem);
		// Geometry
		FormItem geomItem = new FormItem();
		geomItem.setWidget(new Geometry());
		row.getItems().add(geomItem);
		// GeometryMap
		FormItem geoMapItem = new FormItem();
		geoMapItem.setWidget(new GeometryMap());
		row.getItems().add(geoMapItem);
		form.getRows().add(row);
		view.getContained().add(form);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- Tabular widget tests ----

	@Test
	void visitListGridListRepeaterAndTreeGrid() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new ListGrid());
		view.getContained().add(new ListRepeater());
		view.getContained().add(new TreeGrid());
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitDataGridWithExplicitWidgetColumn() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("fieldName");
		WidgetReference ref = new WidgetReference();
		ref.setWidget(new TextField());
		col.setInputWidget(ref);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitDataGridWithContainerColumn() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.getWidgets().add(new Label());
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitDataRepeaterWithExplicitWidgetColumn() {
		ViewImpl view = new ViewImpl();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("lines");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("fieldName");
		WidgetReference ref = new WidgetReference();
		ref.setWidget(new TextField());
		col.setInputWidget(ref);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- DataGrid with action handlers ----

	@Test
	void visitDataGridWithActionHandlers() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		ServerSideActionEventAction addAction = new ServerSideActionEventAction();
		addAction.setActionName("onAdd");
		grid.getAddedActions().add(addAction);
		ServerSideActionEventAction editAction = new ServerSideActionEventAction();
		editAction.setActionName("onEdit");
		grid.getEditedActions().add(editAction);
		ServerSideActionEventAction removeAction = new ServerSideActionEventAction();
		removeAction.setActionName("onRemove");
		grid.getRemovedActions().add(removeAction);
		ServerSideActionEventAction selectedAction = new ServerSideActionEventAction();
		selectedAction.setActionName("onSelect");
		grid.getSelectedActions().add(selectedAction);
		view.getContained().add(grid);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- Event handler coverage ----

	@Test
	void visitWidgetWithAllEventActionTypes() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField textField = new TextField();
		// add all event action types to changedActions
		ServerSideActionEventAction serverAction = new ServerSideActionEventAction();
		serverAction.setActionName("myAction");
		textField.getChangedActions().add(serverAction);
		RerenderEventAction rerenderAction = new RerenderEventAction();
		textField.getChangedActions().add(rerenderAction);
		SetDisabledEventAction disabledAction = new SetDisabledEventAction();
		textField.getChangedActions().add(disabledAction);
		SetInvisibleEventAction invisibleAction = new SetInvisibleEventAction();
		textField.getChangedActions().add(invisibleAction);
		ToggleDisabledEventAction toggleDisabledAction = new ToggleDisabledEventAction();
		textField.getChangedActions().add(toggleDisabledAction);
		ToggleVisibilityEventAction toggleVisibilityAction = new ToggleVisibilityEventAction();
		textField.getChangedActions().add(toggleVisibilityAction);
		item.setWidget(textField);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitWidgetWithFocusEventHandlers() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField textField = new TextField();
		ServerSideActionEventAction focusAction = new ServerSideActionEventAction();
		focusAction.setActionName("onFocus");
		textField.getFocusActions().add(focusAction);
		ServerSideActionEventAction blurAction = new ServerSideActionEventAction();
		blurAction.setActionName("onBlur");
		textField.getBlurActions().add(blurAction);
		item.setWidget(textField);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitListMembershipWithChangedAction() {
		ViewImpl view = new ViewImpl();
		ListMembership membership = new ListMembership();
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		action.setActionName("onChange");
		membership.getChangedActions().add(action);
		view.getContained().add(membership);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	// ---- Action visitor coverage ----

	@Test
	void visitViewWithCustomAction() {
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		ActionImpl action = new ActionImpl();
		action.setName("MyCustomAction");
		view.putAction(action);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithDefaultsActionOnListView() {
		ViewImpl view = new ViewImpl();
		view.setName("list");
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.putAction(action);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithDefaultsActionOnEditView() {
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.putAction(action);
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}

	@Test
	void visitViewWithAllImplicitActions() {
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		int counter = 0;
		for (ImplicitActionName name : ImplicitActionName.values()) {
			if (ImplicitActionName.DEFAULTS.equals(name)) {
				continue; // tested separately
			}
			ActionImpl action = new ActionImpl();
			action.setName("action" + (counter++));
			action.setImplicitName(name);
			view.putAction(action);
		}
		TestNoOpViewVisitor visitor = new TestNoOpViewVisitor(mockCustomer(), mockModule(), nullDocument(), view);
		assertDoesNotThrow(visitor::visit);
	}
}

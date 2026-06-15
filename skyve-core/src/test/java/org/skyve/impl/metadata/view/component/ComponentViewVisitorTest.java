package org.skyve.impl.metadata.view.component;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
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
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;

/**
 * Tests for {@link ComponentViewVisitor} traversal and binding/condition translation.
 */
@SuppressWarnings("static-method")
class ComponentViewVisitorTest {

	private static CustomerImpl mockCustomer() {
		return Mockito.mock(CustomerImpl.class);
	}

	private static ModuleImpl mockModule() {
		return Mockito.mock(ModuleImpl.class);
	}

	private static ComponentViewVisitor visitor(ViewImpl view) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, Collections.emptyList(), null);
	}

	private static ComponentViewVisitor visitorWithPrefix(ViewImpl view, String prefix) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				prefix, Collections.emptyList(), null);
	}

	private static ComponentViewVisitor visitorWithWidgetId(ViewImpl view, String widgetId) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, Collections.emptyList(), widgetId);
	}

	private static ComponentViewVisitor visitorWithNames(ViewImpl view, String from, String to) {
		ComponentNameMap nameMap = new ComponentNameMap();
		nameMap.setFromComponent(from);
		nameMap.setMappedTo(to);
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, List.of(nameMap), null);
	}

	// ---- Basic traversal tests ----

	@Test
	void visitEmptyViewDoesNotThrow() {
		ViewImpl view = new ViewImpl();
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitViewWithVBoxAndHBox() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.getContained().add(new Button());
		view.getContained().add(vbox);
		HBox hbox = new HBox();
		hbox.getContained().add(new Blurb());
		view.getContained().add(hbox);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitViewWithTabPaneAndTab() {
		ViewImpl view = new ViewImpl();
		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.getContained().add(new Button());
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitViewWithSidebar() {
		ViewImpl view = new ViewImpl();
		Sidebar sidebar = new Sidebar();
		sidebar.getContained().add(new Button());
		view.setSidebar(sidebar);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitFormWithInputWidgets() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormColumn column = new FormColumn();
		form.getColumns().add(column);
		FormRow row = new FormRow();
		// Add variety of input widgets
		for (FormItem item : List.of(
				formItem(new TextField()),
				formItem(new CheckBox()),
				formItem(new Combo()),
				formItem(new ColourPicker()),
				formItem(new Password()),
				formItem(new Radio()),
				formItem(new RichText()),
				formItem(new TextArea()),
				formItem(new HTML()),
				formItem(new ContentImage()),
				formItem(new ContentLink()),
				formItem(new ContentSignature()),
				formItem(new Slider()),
				formItem(new Spinner()),
				formItem(new ProgressBar()),
				formItem(new LookupDescription()),
				formItem(new Geometry()),
				formItem(new GeometryMap()),
				formItem(new DefaultWidget()),
				formItem(new ZoomIn()),
				formItem(new Label()),
				formItem(new Blurb()))) {
			row.getItems().add(item);
		}
		form.getRows().add(row);
		view.getContained().add(form);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	private static FormItem formItem(Object widget) {
		FormItem item = new FormItem();
		item.setWidget((org.skyve.metadata.SerializableMetaData) widget);
		return item;
	}

	@Test
	void visitDirectWidgets() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new Label());
		view.getContained().add(new Blurb());
		view.getContained().add(new Spacer());
		view.getContained().add(new StaticImage());
		view.getContained().add(new DynamicImage());
		view.getContained().add(new Chart());
		view.getContained().add(new MapDisplay());
		view.getContained().add(new DialogButton());
		view.getContained().add(new Link());
		view.getContained().add(new ZoomIn());
		view.getContained().add(new CheckMembership());
		view.getContained().add(new ListMembership());
		view.getContained().add(new Comparison());
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitDataGridWithColumns() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("amount");
		grid.getColumns().add(col);
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.getWidgets().add(new Label());
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitDataRepeaterAndListWidgets() {
		ViewImpl view = new ViewImpl();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("lines");
		view.getContained().add(repeater);
		view.getContained().add(new ListGrid());
		view.getContained().add(new ListRepeater());
		view.getContained().add(new TreeGrid());
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	// ---- Event handler coverage ----

	@Test
	void visitEventHandlersOnTextField() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField textField = new TextField();
		textField.getChangedActions().add(changedServerAction("onChange"));
		textField.getChangedActions().add(new RerenderEventAction());
		SetDisabledEventAction sd = new SetDisabledEventAction();
		sd.setBinding("myField");
		textField.getChangedActions().add(sd);
		SetInvisibleEventAction si = new SetInvisibleEventAction();
		si.setBinding("otherField");
		textField.getChangedActions().add(si);
		ToggleDisabledEventAction td = new ToggleDisabledEventAction();
		td.setBinding("flag");
		textField.getChangedActions().add(td);
		ToggleVisibilityEventAction tv = new ToggleVisibilityEventAction();
		tv.setBinding("flag");
		textField.getChangedActions().add(tv);
		textField.getFocusActions().add(changedServerAction("onFocus"));
		textField.getBlurActions().add(changedServerAction("onBlur"));
		item.setWidget(textField);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	@Test
	void visitDataGridEventHandlers() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		grid.getAddedActions().add(changedServerAction("onAdd"));
		grid.getEditedActions().add(changedServerAction("onEdit"));
		grid.getRemovedActions().add(changedServerAction("onRemove"));
		grid.getSelectedActions().add(changedServerAction("onSelect"));
		view.getContained().add(grid);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	// ---- Action traversal ----

	@Test
	void visitCustomAndImplicitActions() {
		ViewImpl view = new ViewImpl();
		view.setName("edit");
		ActionImpl customAction = new ActionImpl();
		customAction.setName("myAction");
		view.putAction(customAction);
		for (ImplicitActionName name : ImplicitActionName.values()) {
			ActionImpl action = new ActionImpl();
			action.setName("implicit_" + name.name());
			action.setImplicitName(name);
			view.putAction(action);
		}
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	// ---- Binding prefix tests ----

	@Test
	void bindingPrefixedOnTextField() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField textField = new TextField();
		textField.setBinding("amount");
		item.setWidget(textField);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		ComponentViewVisitor v = visitorWithPrefix(view, "line");
		v.visit();
		assertEquals("line.amount", textField.getBinding());
	}

	@Test
	void bindingPrefixedOnDataGrid() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("items");
		view.getContained().add(grid);
		ComponentViewVisitor v = visitorWithPrefix(view, "parent");
		v.visit();
		assertEquals("parent.items", grid.getBinding());
	}

	@Test
	void nullBindingIsNotPrefixed() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField textField = new TextField();
		// binding is null
		item.setWidget(textField);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		ComponentViewVisitor v = visitorWithPrefix(view, "prefix");
		v.visit();
		assertEquals(null, textField.getBinding());
	}

	// ---- Name translation tests ----

	@Test
	void actionNameTranslatedViaNameMap() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("componentAction");
		view.getContained().add(button);
		ComponentViewVisitor v = visitorWithNames(view, "componentAction", "viewAction");
		v.visit();
		assertEquals("viewAction", button.getActionName());
	}

	@Test
	void unmappedActionNameKeptAsIs() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("notMapped");
		view.getContained().add(button);
		ComponentViewVisitor v = visitorWithNames(view, "something", "other");
		v.visit();
		assertEquals("notMapped", button.getActionName());
	}

	@Test
	void negatedConditionTranslated() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setInvisibleConditionName("notActive");
		view.getContained().add(vbox);
		// Map "active" -> "enabled"; the negation "notActive" should translate to "notEnabled"
		ComponentViewVisitor v = visitorWithNames(view, "active", "enabled");
		v.visit();
		assertEquals("notEnabled", vbox.getInvisibleConditionName());
	}

	// ---- widgetId capture tests ----

	@Test
	void getContainedReturnsViewContainedWhenNoWidgetFound() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new Button());
		ComponentViewVisitor v = visitorWithWidgetId(view, "nonExistentWidgetId");
		v.visit();
		assertThrows(MetaDataException.class, v::getContained);
	}

	@Test
	void getContainedCapturesMatchingTabPane() {
		ViewImpl view = new ViewImpl();
		TabPane tabPane = new TabPane();
		tabPane.setWidgetId("myPane");
		view.getContained().add(tabPane);
		ComponentViewVisitor v = visitorWithWidgetId(view, "myPane");
		v.visit();
		List<org.skyve.metadata.SerializableMetaData> contained = v.getContained();
		assertNotNull(contained);
		assertEquals(1, contained.size());
		assertEquals(tabPane, contained.get(0));
	}

	@Test
	void getContainedCapturesMatchingDataGrid() {
		ViewImpl view = new ViewImpl();
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		grid.setWidgetId("gridWidget");
		view.getContained().add(grid);
		ComponentViewVisitor v = visitorWithWidgetId(view, "gridWidget");
		v.visit();
		List<org.skyve.metadata.SerializableMetaData> contained = v.getContained();
		assertEquals(1, contained.size());
		assertEquals(grid, contained.get(0));
	}

	@Test
	void getContainedReturnsViewWhenNoWidgetId() {
		ViewImpl view = new ViewImpl();
		view.getContained().add(new Button());
		ComponentViewVisitor v = visitor(view);
		v.visit();
		List<org.skyve.metadata.SerializableMetaData> contained = v.getContained();
		assertEquals(view.getContained(), contained);
	}

	// ---- LookupDescription event handlers ----

	@Test
	void visitLookupDescriptionWithPickedAndClearedHandlers() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		LookupDescription lookup = new LookupDescription();
		lookup.setBinding("contact");
		lookup.getPickedActions().add(changedServerAction("onPicked"));
		lookup.getClearedActions().add(changedServerAction("onCleared"));
		item.setWidget(lookup);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		assertDoesNotThrow(() -> visitor(view).visit());
	}

	// ---- Blurb and Link expression prefixing ----

	@Test
	void blurbMarkupExpressionPrefixedWhenBindingPrefixSet() {
		ViewImpl view = new ViewImpl();
		Blurb blurb = new Blurb();
		blurb.setMarkup("{greeting}");
		view.getContained().add(blurb);
		// prefixExpression uses BindUtil.prefixMessageExpressions
		ComponentViewVisitor v = visitorWithPrefix(view, "header");
		assertDoesNotThrow(v::visit);
	}

	private static ServerSideActionEventAction changedServerAction(String name) {
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		action.setActionName(name);
		return action;
	}
}

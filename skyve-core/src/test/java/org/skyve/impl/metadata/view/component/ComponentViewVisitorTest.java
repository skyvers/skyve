package org.skyve.impl.metadata.view.component;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
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
		return mock(CustomerImpl.class);
	}

	private static ModuleImpl mockModule() {
		return mock(ModuleImpl.class);
	}

	private static ComponentViewVisitor visitor(ViewImpl view) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, Collections.emptyList(), Collections.emptyList(), null);
	}

	private static ComponentViewVisitor visitorWithPrefix(ViewImpl view, String prefix) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				prefix, Collections.emptyList(), Collections.emptyList(), null);
	}

	private static ComponentViewVisitor visitorWithWidgetId(ViewImpl view, String widgetId) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, Collections.emptyList(), Collections.emptyList(), widgetId);
	}

	private static ComponentTranslation mapping(String from, String to) {
		ComponentTranslation result = new ComponentTranslation();
		result.setFromComponent(from);
		result.setMappedTo(to);
		return result;
	}

	private static ComponentViewVisitor visitorWithActions(ViewImpl view, String from, String to) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				null, List.of(mapping(from, to)), Collections.emptyList(), null);
	}

	private static ComponentViewVisitor visitorWithConditions(ViewImpl view, String from, String to) {
		return visitorWithConditions(view, null, from, to);
	}

	private static ComponentViewVisitor visitorWithConditions(ViewImpl view, String prefix, String from, String to) {
		return new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop",
				prefix, Collections.emptyList(), List.of(mapping(from, to)), null);
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

	// ---- Action and condition mapping tests ----

	@Test
	void actionNameTranslatedViaActionMappings() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("componentAction");
		view.getContained().add(button);
		ComponentViewVisitor v = visitorWithActions(view, "componentAction", "viewAction");
		v.visit();
		assertEquals("viewAction", button.getActionName());
	}

	@Test
	void unmappedActionNameKeptAsIs() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("notMapped");
		view.getContained().add(button);
		ComponentViewVisitor v = visitorWithActions(view, "something", "other");
		v.visit();
		assertEquals("notMapped", button.getActionName());
	}

	@Test
	void negatedConditionTranslated() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("notActive");
		view.getContained().add(vbox);
		// Map "active" -> "enabled"; the negation "notActive" should translate to "notEnabled"
		ComponentViewVisitor v = visitorWithConditions(view, "active", "enabled");
		v.visit();
		assertEquals("notEnabled", vbox.getInvisibleCondition());
	}

	@Test
	void inlineConditionExpressionTakesTheBindingPrefix() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("{el:bean.active}");
		view.getContained().add(vbox);
		visitorWithPrefix(view, "contact").visit();
		assertEquals("{el:bean.contact.active}", vbox.getInvisibleCondition());
	}

	@Test
	void inverseOfInlineConditionExpressionTakesTheBindingPrefixAndKeepsItsSense() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlVisibleCondition("{el:bean.active}");
		view.getContained().add(vbox);
		visitorWithPrefix(view, "contact").visit();
		assertEquals("not{el:bean.contact.active}", vbox.getInvisibleCondition());
	}

	@Test
	void inlineConditionAuthoredAsVisibleStaysAuthoredAsVisible() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlVisibleCondition("{el:bean.active}");
		view.getContained().add(vbox);
		visitorWithPrefix(view, "contact").visit();
		// the inverse of the expression is derived, never stored
		assertEquals("{el:bean.contact.active}", vbox.getXmlVisibleCondition());
		assertNull(vbox.getXmlInvisibleCondition());
	}

	@Test
	void conditionNameAuthoredAsVisibleIsMappedAndStaysAuthoredAsVisible() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlVisibleCondition("active");
		view.getContained().add(vbox);
		visitorWithConditions(view, "active", "enabled").visit();
		assertEquals("enabled", vbox.getXmlVisibleCondition());
		assertNull(vbox.getXmlInvisibleCondition());
		assertEquals("notEnabled", vbox.getInvisibleCondition());
	}

	@Test
	void conditionAuthoredAsInvisibleStaysAuthoredAsInvisible() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("active");
		view.getContained().add(vbox);
		visitorWithConditions(view, "active", "enabled").visit();
		assertEquals("enabled", vbox.getXmlInvisibleCondition());
		assertNull(vbox.getXmlVisibleCondition());
	}

	@Test
	void enabledConditionOnAnInputKeepsItsAuthoredSense() {
		ViewImpl view = new ViewImpl();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		TextField text = new TextField();
		text.setBinding("name");
		text.setXmlEnabledCondition("{el:bean.editable}");
		item.setWidget(text);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
		visitorWithPrefix(view, "contact").visit();
		assertEquals("{el:bean.contact.editable}", text.getXmlEnabledCondition());
		assertNull(text.getXmlDisabledCondition());
		assertEquals("not{el:bean.contact.editable}", text.getDisabledCondition());
	}

	@Test
	void gridAndLookupFlagsKeepTheirAuthoredSense() {
		ViewImpl view = new ViewImpl();
		DataGrid dataGrid = new DataGrid();
		dataGrid.setBinding("lines");
		dataGrid.setXmlEnableAddCondition("canAdd");
		dataGrid.setXmlDisableEditCondition("noEdit");
		dataGrid.setXmlEnableRemoveCondition("{el:bean.open}");
		dataGrid.setXmlDisableZoomCondition("{el:bean.closed}");
		view.getContained().add(dataGrid);
		ListGrid listGrid = new ListGrid();
		listGrid.setXmlEnableAddCondition("canAdd");
		listGrid.setXmlEnableEditCondition("canEdit");
		listGrid.setXmlEnableRemoveCondition("canRemove");
		listGrid.setXmlEnableZoomCondition("canZoom");
		view.getContained().add(listGrid);
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		LookupDescription lookup = new LookupDescription();
		lookup.setBinding("owner");
		lookup.setXmlEnablePickCondition("canPick");
		lookup.setXmlEnableClearCondition("canClear");
		lookup.setXmlEnableAddCondition("canAdd");
		lookup.setXmlEnableEditCondition("canEdit");
		item.setWidget(lookup);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);

		visitorWithPrefix(view, "contact").visit();

		assertEquals("canAdd", dataGrid.getXmlEnableAddCondition());
		assertNull(dataGrid.getXmlDisableAddCondition());
		assertEquals("noEdit", dataGrid.getXmlDisableEditCondition());
		assertNull(dataGrid.getXmlEnableEditCondition());
		assertEquals("{el:bean.contact.open}", dataGrid.getXmlEnableRemoveCondition());
		assertNull(dataGrid.getXmlDisableRemoveCondition());
		assertEquals("{el:bean.contact.closed}", dataGrid.getXmlDisableZoomCondition());

		assertEquals("canAdd", listGrid.getXmlEnableAddCondition());
		assertEquals("canEdit", listGrid.getXmlEnableEditCondition());
		assertEquals("canRemove", listGrid.getXmlEnableRemoveCondition());
		assertEquals("canZoom", listGrid.getXmlEnableZoomCondition());
		assertEquals("notCanZoom", listGrid.getDisableZoomCondition());

		assertEquals("canPick", lookup.getXmlEnablePickCondition());
		assertEquals("canClear", lookup.getXmlEnableClearCondition());
		assertEquals("canAdd", lookup.getXmlEnableAddCondition());
		assertEquals("canEdit", lookup.getXmlEnableEditCondition());
		assertNull(lookup.getXmlDisablePickCondition());
	}

	@Test
	void inlineConditionExpressionWithoutBindingPrefixKeptAsIs() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("{el:bean.active}");
		view.getContained().add(vbox);
		visitor(view).visit();
		assertEquals("{el:bean.active}", vbox.getInvisibleCondition());
	}

	@Test
	void actionMappingsAreNotAppliedToConditions() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("active");
		view.getContained().add(vbox);
		visitorWithActions(view, "active", "enabled").visit();
		assertEquals("active", vbox.getXmlInvisibleCondition());
	}

	@Test
	void conditionMappingsAreNotAppliedToActionNames() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("active");
		TextField text = textFieldIn(view);
		text.getChangedActions().add(changedServerAction("active"));
		view.getContained().add(button);
		visitorWithConditions(view, "active", "enabled").visit();
		assertEquals("active", button.getActionName());
		assertEquals("active", ((ServerSideActionEventAction) text.getChangedActions().get(0)).getActionName());
	}

	@Test
	void actionNameStartingWithNotIsNotTreatedAsANegatedCondition() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		button.setActionName("notifyAll");
		view.getContained().add(button);
		// were the action a condition, notifyAll would be the inverse of ifyAll
		visitorWithActions(view, "ifyAll", "other").visit();
		assertEquals("notifyAll", button.getActionName());
	}

	@Test
	void nullActionNameStaysNull() {
		ViewImpl view = new ViewImpl();
		Button button = new Button();
		view.getContained().add(button);
		visitorWithActions(view, "componentAction", "viewAction").visit();
		assertNull(button.getActionName());
	}

	@Test
	void serverSideEventActionNamesAreMappedOnInputsAndGrids() {
		ViewImpl view = new ViewImpl();
		TextField text = textFieldIn(view);
		text.getChangedActions().add(changedServerAction("componentAction"));
		text.getChangedActions().add(changedServerAction("unmapped"));
		DataGrid grid = new DataGrid();
		grid.setBinding("lines");
		grid.getAddedActions().add(changedServerAction("componentAction"));
		view.getContained().add(grid);
		visitorWithActions(view, "componentAction", "viewAction").visit();
		assertEquals("viewAction", ((ServerSideActionEventAction) text.getChangedActions().get(0)).getActionName());
		assertEquals("unmapped", ((ServerSideActionEventAction) text.getChangedActions().get(1)).getActionName());
		assertEquals("viewAction", ((ServerSideActionEventAction) grid.getAddedActions().get(0)).getActionName());
	}

	/**
	 * Each case is the attribute authored in the component view, its value, the component binding,
	 * the condition mapping (from, to), then the invisible and visible attributes expected in the
	 * containing view and the derived invisible condition renderers read.
	 */
	@TestFactory
	Stream<DynamicTest> conditionMappingsTranslateEitherAttributeOfAPair() {
		String a = "{el:bean.a}";
		String b = "{el:bean.b}";
		String[][] cases = {
			// condition names
			{"invisible", "active", null, "active", "enabled", "enabled", null, "enabled"},
			{"visible", "active", null, "active", "enabled", null, "enabled", "notEnabled"},
			{"invisible", "notActive", null, "active", "enabled", "notEnabled", null, "notEnabled"},
			{"visible", "notActive", null, "active", "enabled", null, "notEnabled", "enabled"},
			{"invisible", "notActive", null, "notActive", "hidden", "hidden", null, "hidden"},
			{"invisible", "other", null, "active", "enabled", "other", null, "other"},
			{"invisible", "notOther", null, "active", "enabled", "notOther", null, "notOther"},
			// a binding never changes a condition name or what a condition is mapped to
			{"invisible", "active", "contact", "active", "enabled", "enabled", null, "enabled"},
			{"invisible", "other", "contact", "active", "enabled", "other", null, "other"},
			// inline expressions mapped to condition names
			{"invisible", a, null, a, "enabled", "enabled", null, "enabled"},
			{"visible", a, null, a, "enabled", null, "enabled", "notEnabled"},
			{"invisible", a, "contact", a, "enabled", "enabled", null, "enabled"},
			{"visible", a, "contact", a, "notEnabled", null, "notEnabled", "enabled"},
			// inline expressions mapped to inline expressions
			{"invisible", a, null, a, b, b, null, b},
			{"visible", a, null, a, b, null, b, "not" + b},
			{"invisible", a, "contact", a, b, b, null, b},
			{"visible", a, "contact", a, b, null, b, "not" + b},
			// condition names mapped to inline expressions
			{"invisible", "active", null, "active", b, b, null, b},
			{"visible", "active", null, "active", b, null, b, "not" + b},
			// ... where the inverse of an expression would have to be stored, the other attribute is used
			{"invisible", "notActive", null, "active", b, null, b, "not" + b},
			{"visible", "notActive", null, "active", b, b, null, b},
			// an inline expression without a mapping takes the binding prefix, whatever else is mapped
			{"invisible", "{el:bean.c}", "contact", a, "enabled", "{el:bean.contact.c}", null, "{el:bean.contact.c}"},
			{"visible", "{el:bean.c}", "contact", a, b, null, "{el:bean.contact.c}", "not{el:bean.contact.c}"},
			{"invisible", "{el:bean.c}", null, a, "enabled", "{el:bean.c}", null, "{el:bean.c}"},
			// a mapping matches the exact text of the expression
			{"invisible", "{el:bean.a }", null, a, "enabled", "{el:bean.a }", null, "{el:bean.a }"},
			{"invisible", "{el: bean.a}", "contact", a, "enabled", "{el:bean.contact.a}", null, "{el:bean.contact.a}"},
			// boolean literals
			{"invisible", "true", "contact", "active", "enabled", "true", null, "true"},
			{"visible", "true", "contact", "active", "enabled", null, "true", "false"},
			{"invisible", "active", null, "active", "true", "true", null, "true"},
			{"visible", "active", null, "active", "false", null, "false", "true"},
			// nothing authored
			{"invisible", null, "contact", "active", "enabled", null, null, null},
		};
		return Stream.of(cases).map(c -> dynamicTest(c[0] + "=" + c[1] + " in " + c[2] + " with " + c[3] + " -> " + c[4], () -> {
			ViewImpl view = new ViewImpl();
			VBox vbox = new VBox();
			if ("visible".equals(c[0])) {
				vbox.setXmlVisibleCondition(c[1]);
			}
			else {
				vbox.setXmlInvisibleCondition(c[1]);
			}
			view.getContained().add(vbox);
			visitorWithConditions(view, c[2], c[3], c[4]).visit();
			assertEquals(c[5], vbox.getXmlInvisibleCondition(), "invisible attribute");
			assertEquals(c[6], vbox.getXmlVisibleCondition(), "visible attribute");
			assertEquals(c[7], vbox.getInvisibleCondition(), "derived invisible condition");
		}));
	}

	@Test
	void conditionMappingsApplyToTheDisabledPairOfAnInput() {
		ViewImpl view = new ViewImpl();
		TextField text = textFieldIn(view);
		text.setXmlEnabledCondition("{el:bean.editable}");
		text.setXmlVisibleCondition("shown");
		visitorWithConditions(view, "contact", "{el:bean.editable}", "canEdit").visit();
		assertEquals("canEdit", text.getXmlEnabledCondition());
		assertNull(text.getXmlDisabledCondition());
		assertEquals("notCanEdit", text.getDisabledCondition());
		assertEquals("shown", text.getXmlVisibleCondition());
	}

	@Test
	void conditionMappingsApplyToGridAndLookupFlags() {
		ViewImpl view = new ViewImpl();
		String open = "{el:bean.open}";
		DataGrid dataGrid = new DataGrid();
		dataGrid.setBinding("lines");
		dataGrid.setXmlEnableAddCondition(open);
		dataGrid.setXmlDisableEditCondition(open);
		dataGrid.setXmlEnableRemoveCondition("notOpen");
		dataGrid.setXmlDisableZoomCondition("notOpen");
		view.getContained().add(dataGrid);
		ListGrid listGrid = new ListGrid();
		listGrid.setXmlEnableAddCondition(open);
		listGrid.setXmlDisableEditCondition(open);
		listGrid.setXmlEnableRemoveCondition(open);
		listGrid.setXmlDisableZoomCondition(open);
		view.getContained().add(listGrid);
		LookupDescription lookup = new LookupDescription();
		lookup.setBinding("owner");
		lookup.setXmlEnablePickCondition(open);
		lookup.setXmlDisableClearCondition(open);
		lookup.setXmlEnableAddCondition(open);
		lookup.setXmlDisableEditCondition(open);
		addToForm(view, lookup);

		new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop", "contact",
				Collections.emptyList(),
				List.of(mapping(open, "{el:bean.unlocked}"), mapping("open", "{el:bean.unlocked}")),
				null).visit();

		String unlocked = "{el:bean.unlocked}";
		assertEquals(unlocked, dataGrid.getXmlEnableAddCondition());
		assertEquals(unlocked, dataGrid.getXmlDisableEditCondition());
		// notOpen maps to the inverse of an expression, which is stored through the other attribute
		assertEquals(unlocked, dataGrid.getXmlDisableRemoveCondition());
		assertNull(dataGrid.getXmlEnableRemoveCondition());
		assertEquals(unlocked, dataGrid.getXmlEnableZoomCondition());
		assertNull(dataGrid.getXmlDisableZoomCondition());

		assertEquals(unlocked, listGrid.getXmlEnableAddCondition());
		assertEquals(unlocked, listGrid.getXmlDisableEditCondition());
		assertEquals(unlocked, listGrid.getXmlEnableRemoveCondition());
		assertEquals(unlocked, listGrid.getXmlDisableZoomCondition());

		assertEquals(unlocked, lookup.getXmlEnablePickCondition());
		assertEquals(unlocked, lookup.getXmlDisableClearCondition());
		assertEquals(unlocked, lookup.getXmlEnableAddCondition());
		assertEquals(unlocked, lookup.getXmlDisableEditCondition());
		assertEquals("not" + unlocked, lookup.getDisablePickCondition());
	}

	@TestFactory
	Stream<DynamicTest> conditionMappingsApplyToAnUnpairedCondition() {
		String a = "{el:bean.a}";
		// authored postRefresh, component binding, mapping from, mapping to, expected
		String[][] cases = {
			{"active", null, "active", "enabled", "enabled"},
			{"notActive", null, "active", "enabled", "notEnabled"},
			{"other", "contact", "active", "enabled", "other"},
			{a, "contact", a, "enabled", "enabled"},
			{a, "contact", a, "{el:bean.b}", "{el:bean.b}"},
			{a, "contact", "active", "enabled", "{el:bean.contact.a}"},
			{a, null, "active", "enabled", a},
			// there is no opposite attribute to store this through, so the inverse is kept
			{"notActive", null, "active", a, "not" + a},
			{null, "contact", "active", "enabled", null},
		};
		return Stream.of(cases).map(c -> dynamicTest(c[0] + " in " + c[1] + " with " + c[2] + " -> " + c[3], () -> {
			ViewImpl view = new ViewImpl();
			ListGrid listGrid = new ListGrid();
			listGrid.setPostRefreshCondition(c[0]);
			view.getContained().add(listGrid);
			ListRepeater repeater = new ListRepeater();
			repeater.setPostRefreshCondition(c[0]);
			view.getContained().add(repeater);
			visitorWithConditions(view, c[1], c[2], c[3]).visit();
			assertEquals(c[4], listGrid.getPostRefreshCondition());
			assertEquals(c[4], repeater.getPostRefreshCondition());
		}));
	}

	@Test
	void conditionMappingsApplyToSetDisabledAndSetInvisibleEventActions() {
		ViewImpl view = new ViewImpl();
		TextField text = textFieldIn(view);
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("other");
		setDisabled.setXmlEnabledCondition("{el:bean.a}");
		text.getChangedActions().add(setDisabled);
		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("other");
		setInvisible.setXmlInvisibleCondition("notActive");
		text.getChangedActions().add(setInvisible);
		new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop", "contact",
				Collections.emptyList(),
				List.of(mapping("{el:bean.a}", "canEdit"), mapping("active", "{el:bean.b}")),
				null).visit();
		assertEquals("contact.other", setDisabled.getBinding());
		assertEquals("canEdit", setDisabled.getXmlEnabledCondition());
		assertNull(setDisabled.getXmlDisabledCondition());
		assertEquals("{el:bean.b}", setInvisible.getXmlVisibleCondition());
		assertNull(setInvisible.getXmlInvisibleCondition());
	}

	@Test
	void translationsWithABlankSideAreIgnored() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("active");
		view.getContained().add(vbox);
		Button button = new Button();
		button.setActionName("componentAction");
		view.getContained().add(button);
		List<ComponentTranslation> incomplete = List.of(mapping(" ", "other"), mapping("active", ""), mapping("componentAction", null), mapping(null, null));
		new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop", "contact", incomplete, incomplete, null).visit();
		assertEquals("active", vbox.getXmlInvisibleCondition());
		assertEquals("componentAction", button.getActionName());
	}

	@Test
	void translationsAreTrimmed() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("active");
		view.getContained().add(vbox);
		visitorWithConditions(view, "  active ", " enabled\t").visit();
		assertEquals("enabled", vbox.getXmlInvisibleCondition());
	}

	@Test
	void laterMappingOfTheSameConditionWins() {
		ViewImpl view = new ViewImpl();
		VBox vbox = new VBox();
		vbox.setXmlInvisibleCondition("active");
		view.getContained().add(vbox);
		new ComponentViewVisitor(mockCustomer(), mockModule(), null, view, "desktop", null,
				Collections.emptyList(),
				List.of(mapping("active", "first"), mapping("active", "second")),
				null).visit();
		assertEquals("second", vbox.getXmlInvisibleCondition());
	}

	private static TextField textFieldIn(ViewImpl view) {
		TextField result = new TextField();
		result.setBinding("name");
		addToForm(view, result);
		return result;
	}

	private static void addToForm(ViewImpl view, org.skyve.metadata.MetaData widget) {
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		item.setWidget(widget);
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);
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

package org.skyve.impl.sail.execution.sc;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularColumn;
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.impl.sail.execution.sc.Locator.Kind;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Responsible for traversing a SmartClient (SC) view definition and
 * generating locators for all supported user interactions.
 * <p>
 * It processes containers, forms, widgets, and actions within the view and registers a
 * {@link Locator} per SAIL identifier in the {@link SmartClientAutomationContext}.
 * The locators are SAIL locators (see {@link Locator}): they name the widget by its binding,
 * action name, tab title or grid ordinal, and the harness resolves them against the live view in
 * the browser. They do not re-derive SmartClient's generated component IDs, so they are not
 * affected by create views, the per-page ID counter, window nesting or the order of containers.
 *
 * @author simeonsolomou
 */
public class SmartClientSAILViewVisitor {
	private static final String ELEMENT = "/element";
	private static final String DATE_ELEMENT = "/item[name=dateTextField]/element";
	private static final String TIME_ELEMENT = "/item[name=timeTextField]/element";
	private static final String GEOMETRY_ELEMENT = "/canvas/member[Class=DynamicForm||classIndex=0]/item[Class=TextItem||name=value]/element";
	private static final String LOOKUP_CANVAS = "/canvas";
	private static final String RICH_TEXT_AREA = "/canvas/editArea";
	private static final String ROW_CELL = "/body/row[%d]/col[%d]";

	private CustomerImpl customer;
	private ModuleImpl module;
	private DocumentImpl document;
	private ViewImpl view;
	private String currentUxUi;
	private String viewName;

	private SmartClientAutomationContext automationContext;

	// The zero-based ordinal of the next list grid, tree grid or list repeater (BizListGrid) in the view
	private int listGridOrdinal = 0;

	// The titles of the tabs being visited, outermost first
	private Deque<String> tabPath = new ArrayDeque<>();

	/**
	 * Creates a SmartClient view visitor that records interaction locators into the supplied context.
	 *
	 * @param user the current user
	 * @param module the module metadata being visited
	 * @param document the document metadata being visited
	 * @param view the view metadata being traversed
	 * @param uxui the active UX/UI name
	 * @param automationContext the automation context receiving generated locators
	 */
	public SmartClientSAILViewVisitor(
			@Nonnull User user,
			@Nonnull Module module,
			@Nonnull Document document,
			@Nonnull View view,
			@Nonnull String uxui,
			@Nonnull SmartClientAutomationContext automationContext) {
		this.customer = (CustomerImpl) user.getCustomer();
		this.module = (ModuleImpl) module;
		this.document = (DocumentImpl) document;
		this.view = (ViewImpl) view;
		this.currentUxUi = uxui;
		this.automationContext = automationContext;
		this.viewName = viewName(module.getName(), document.getName());
	}

	/**
	 * The name the SAIL harness expects the browser's view to have.
	 *
	 * @param moduleName	the module name
	 * @param documentName	the document name
	 * @return {@code module.Document}
	 */
	public static @Nonnull String viewName(@Nonnull String moduleName, @Nonnull String documentName) {
		return moduleName + '.' + documentName;
	}

	/**
	 * Traverses the configured view and records supported automation locators in the current context.
	 */
	public final void visit() {
		visitContainer(view);
	}

	/**
	 * Visits a container and dispatches traversal to contained metadata.
	 *
	 * @param container the container metadata to traverse
	 */
	private void visitContainer(@Nonnull Container container) {
		if (container instanceof Tab tab) {
			String title = tabTitle(tab);
			tabPath.addLast(title);
			try {
				visitTab(title);
				for (MetaData widget : container.getContained()) {
					visitWidget(widget);
				}
			}
			finally {
				tabPath.removeLast();
			}
		}
		else {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		}

		if (container == view) {
			visitActions();
		}
	}

	/**
	 * Dispatches a metadata widget to the SmartClient locator visitor that matches its concrete widget type.
	 *
	 * @param widget the metadata widget to visit
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visitWidget(@Nullable MetaData widget) {
		if (widget instanceof Container container) {
			visitContainer(container);
		}
		else if (widget instanceof Form form) {
			for (FormRow row : form.getRows()) {
				for (FormItem item : row.getItems()) {
					MetaData itemWidget = item.getWidget();
					if (itemWidget instanceof DefaultWidget defaultWidget) {
						visitDefaultWidget(defaultWidget, null);
					}
					else {
						visitWidget(itemWidget);
					}
				}
			}
		}
		else if (widget instanceof TabPane tabPane) {
			for (Tab tab : tabPane.getTabs()) {
				visitContainer(tab);
			}
		}
		else if (widget instanceof Button button) {
			visitButton(button);
		}
		else if (widget instanceof ZoomIn zoomIn) {
			visitZoomIn(zoomIn);
		}
		else if (widget instanceof ListGrid grid) { // includes TreeGrid
			visitListGrid(grid.getQueryName(), grid.getModelName());
		}
		else if (widget instanceof ListRepeater repeater) {
			visitListGrid(repeater.getQueryName(), repeater.getModelName());
		}
		else if (widget instanceof DataGrid grid) {
			visitDataGrid(grid, Boolean.TRUE.equals(grid.getInline()));
		}
		else if (widget instanceof DataRepeater repeater) {
			visitDataGrid(repeater, false);
		}
		else if (widget instanceof Component component) {
			visitComponent(component);
		}
		else if (widget instanceof Bound bound) {
			visitInput(bound, bound.getBinding(), null);
		}
	}

	/**
	 * Resolves a default widget to its concrete input widget and visits the resolved widget.
	 *
	 * @param widget the default widget metadata
	 * @param grid the inline data grid whose column this is, or {@code null} in a form
	 */
	private void visitDefaultWidget(@Nonnull DefaultWidget widget, @Nullable AbstractDataWidget grid) {
		String binding = widget.getBinding();
		if (binding == null) { // validated as required when the view is loaded
			throw new MetaDataException("DefaultWidget in view " + view.getName() + " of document " + module.getName() + '.' + document.getName() + " has no binding");
		}
		visitDefaultBinding(binding, document, grid);
	}

	/**
	 * Visits the default input widget of a binding.
	 *
	 * @param binding the binding, relative to {@code owner}
	 * @param owner the document the binding is relative to
	 * @param grid the inline data grid whose column this is, or {@code null} in a form
	 */
	private void visitDefaultBinding(@Nonnull String binding, @Nonnull Document owner, @Nullable AbstractDataWidget grid) {
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, owner, binding);
		Attribute attribute = target.getAttribute();
		if (attribute != null) {
			Bound defaultWidget = attribute.getDefaultInputWidget();
			String definedBinding = defaultWidget.getBinding();

			try {
				defaultWidget.setBinding(binding);
				if (grid == null) {
					visitWidget(defaultWidget);
				}
				else {
					visitInput(defaultWidget, binding, grid);
				}
			} finally {
				defaultWidget.setBinding(definedBinding);
			}
		}
	}

	/**
	 * Registers a locator for an input widget, in a form or as an inline data grid cell.
	 * Widgets that cannot take data entry (such as HTML) are registered without an input type so that
	 * {@code testValue} can still read them.
	 *
	 * @param widget the input widget
	 * @param binding the binding to register it under: the widget's own, or a data grid column's
	 * @param grid the inline data grid whose column this is, or {@code null} in a form
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visitInput(@Nonnull Bound widget, @Nullable String binding, @Nullable AbstractDataWidget grid) {
		if (binding == null) {
			return;
		}

		InputType type = null;
		String suffix = null;
		if (widget instanceof CheckBox) {
			type = InputType.CHECKBOX;
		}
		else if (widget instanceof Combo) {
			type = InputType.COMBO;
		}
		else if (widget instanceof Radio) {
			type = InputType.RADIO;
		}
		else if (widget instanceof LookupDescription) {
			type = InputType.LOOKUP_DESCRIPTION;
			// in an inline data grid row, the cell's editor is the combo box itself
			suffix = (grid == null) ? LOOKUP_CANVAS : null;
		}
		else if (widget instanceof RichText) {
			type = InputType.RICH_TEXT;
			suffix = RICH_TEXT_AREA;
		}
		else if (widget instanceof Slider) {
			type = InputType.SLIDER;
		}
		else if (widget instanceof Geometry) {
			type = InputType.TEXT;
			suffix = GEOMETRY_ELEMENT;
		}
		else if ((widget instanceof ColourPicker) ||
					(widget instanceof Password) ||
					(widget instanceof Spinner) ||
					(widget instanceof TextArea)) {
			type = InputType.TEXT;
			suffix = ELEMENT;
		}
		else if (widget instanceof TextField) {
			type = InputType.TEXT;
			suffix = textSuffix(binding, grid);
		}
		else if (widget instanceof HTML) {
			type = InputType.HTML;
		}
		else if (widget instanceof ListMembership) {
			if (grid == null) {
				automationContext.put(binding, new Locator(Locator.sail(viewName, Kind.membership, sanitise(binding), null, null, null, null),
															InputType.MEMBERSHIP));
			}
			return;
		}
		else if (! ((widget instanceof ContentUpload) || (widget instanceof ContentSignature))) {
			// Not an input SAIL can address (check membership, comparison...)
			return;
		}
		// content widgets are registered without an input type: <upload> and testValue use them, dataEnter does not

		if (grid == null) {
			automationContext.put(binding, Locator.item(viewName, sanitise(binding), suffix, type));
		}
		else {
			String gridBinding = grid.getBinding();
			if (gridBinding != null) {
				automationContext.put(gridBinding + '.' + binding,
										Locator.cell(viewName, sanitise(gridBinding), sanitise(binding), suffix, type));
			}
		}
	}

	/**
	 * Determines the AutoTest suffix to a text field's input, which depends on the attribute type:
	 * date and time items have an inner text field.
	 *
	 * @param binding the binding
	 * @param grid the inline data grid whose column this is, or {@code null} in a form
	 * @return the suffix
	 */
	private @Nonnull String textSuffix(@Nonnull String binding, @Nullable AbstractDataWidget grid) {
		Document owner = (grid == null) ? document : gridDocument(grid);
		Attribute attribute = (owner == null) ? null : BindUtil.getMetaDataForBinding(customer, module, owner, binding).getAttribute();
		if (attribute != null) {
			AttributeType attributeType = attribute.getAttributeType();
			if ((attributeType == AttributeType.date) ||
					(attributeType == AttributeType.dateTime) ||
					(attributeType == AttributeType.timestamp)) {
				return DATE_ELEMENT;
			}
			if (attributeType == AttributeType.time) {
				return TIME_ELEMENT;
			}
		}
		return ELEMENT;
	}

	/**
	 * Resolves the document of a data grid's rows.
	 *
	 * @param grid the data grid
	 * @return the row document, or {@code null} if the binding is not a relation
	 */
	private @Nullable Document gridDocument(@Nonnull AbstractDataWidget grid) {
		String binding = grid.getBinding();
		if (binding == null) {
			return null;
		}
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
		if (target.getAttribute() instanceof Relation relation) {
			return module.getDocument(customer, relation.getDocumentName());
		}
		return null;
	}

	/**
	 * Registers a locator for a form button action.
	 *
	 * @param button the button metadata
	 */
	private final void visitButton(@Nonnull Button button) {
		String actionName = button.getActionName();
		automationContext.put(actionName, Locator.widget(viewName, Kind.button, buttonName(actionName), null, null, null));
	}

	/**
	 * Returns whether buttons of this kind of implicit action are named after the action (its class), not the implicit action.
	 *
	 * @param implicitName the implicit action, or {@code null}
	 * @return {@code true} for upload, download, import and export actions
	 */
	private static boolean classNamed(@Nullable ImplicitActionName implicitName) {
		return ImplicitActionName.Upload.equals(implicitName) ||
				ImplicitActionName.Download.equals(implicitName) ||
				ImplicitActionName.BizImport.equals(implicitName) ||
				ImplicitActionName.BizExport.equals(implicitName);
	}

	/**
	 * The action name SmartClient gives an action's buttons: an implicit action's name, or a custom action's class
	 * (such as {@code Save} for {@code <action name="Refresh" className="Save"/>}).
	 *
	 * @param actionName the action's name
	 * @return the buttons' action name
	 */
	private @Nonnull String buttonName(@Nonnull String actionName) {
		Action action = view.getAction(actionName);
		if (action == null) {
			return actionName;
		}
		ImplicitActionName implicitName = action.getImplicitName();
		if (classNamed(implicitName)) {
			return actionName;
		}
		if ((implicitName != null) && (! ImplicitActionName.DEFAULTS.equals(implicitName))) {
			return implicitName.toString();
		}
		// a custom action's buttons carry its class (resource) name, which a named action can give differently
		String resourceName = action.getResourceName();
		return (resourceName == null) ? actionName : resourceName;
	}

	/**
	 * Registers a locator for a zoom-in widget under {@code <binding>.zoomIn}.
	 *
	 * @param zoomIn the zoom-in metadata
	 */
	private final void visitZoomIn(@Nonnull ZoomIn zoomIn) {
		String binding = zoomIn.getBinding();
		if (binding == null) { // validated as required when the view is loaded
			throw new MetaDataException("ZoomIn in view " + view.getName() + " of document " + module.getName() + '.' + document.getName() + " has no binding");
		}
		automationContext.put(binding + ".zoomIn", Locator.widget(viewName, Kind.zoomIn, sanitise(binding), null, null, null));
	}

	/**
	 * Registers list-grid action and selection locators for a list grid, tree grid or list repeater.
	 *
	 * @param queryName the grid's query, or {@code null}
	 * @param modelName the grid's model, or {@code null}
	 */
	private final void visitListGrid(@Nullable String queryName, @Nullable String modelName) {
		String key = NavigateList.listGridIdentifier(
				automationContext,
				module.getName(),
				queryName,
				document.getName(),
				modelName);
		Integer ordinal = Integer.valueOf(listGridOrdinal++);

		automationContext.put(key + ".new", Locator.widget(viewName, Kind.listGrid, null, ordinal, "new", null));
		automationContext.put(key + ".zoom", Locator.widget(viewName, Kind.listGrid, null, ordinal, "zoom", null));
		automationContext.put(key + ".select", Locator.widget(viewName, Kind.listGrid, null, ordinal, "rows", ROW_CELL));
		if ((queryName == null) && (modelName != null)) {
			// also as module.model, as a script naming the model in query="" (as for a query) identifies it
			String alias = module.getName() + '.' + modelName;
			automationContext.put(alias + ".new", Locator.widget(viewName, Kind.listGrid, null, ordinal, "new", null));
			automationContext.put(alias + ".zoom", Locator.widget(viewName, Kind.listGrid, null, ordinal, "zoom", null));
			automationContext.put(alias + ".select", Locator.widget(viewName, Kind.listGrid, null, ordinal, "rows", ROW_CELL));
		}
	}

	/**
	 * Registers the data-grid toolbar and row-selection locators, and inline cell locators for inline grids.
	 *
	 * @param grid the data grid or data repeater
	 * @param inline whether the grid edits its rows inline
	 */
	private final void visitDataGrid(@Nonnull AbstractDataWidget grid, boolean inline) {
		String binding = grid.getBinding();
		if (binding == null) { // validated as required when the view is loaded
			throw new MetaDataException("Data widget in view " + view.getName() + " of document " + module.getName() + '.' + document.getName() + " has no binding");
		}
		String name = sanitise(binding);

		automationContext.put(binding + ".new", Locator.widget(viewName, Kind.dataGrid, name, null, "new", null));
		automationContext.put(binding + ".zoom", Locator.widget(viewName, Kind.dataGrid, name, null, "zoom", null));
		automationContext.put(binding + ".edit", Locator.widget(viewName, Kind.dataGrid, name, null, "edit", null));
		automationContext.put(binding + ".remove", Locator.widget(viewName, Kind.dataGrid, name, null, "remove", null));
		automationContext.put(binding + ".select", Locator.widget(viewName, Kind.dataGrid, name, null, "rows", ROW_CELL));

		if (inline) {
			// marks the grid as inline: New adds a row to edit in place instead of opening a view
			automationContext.put(binding + ".inline", Locator.widget(viewName, Kind.dataGrid, name, null, null, null));

			Document rowDocument = gridDocument(grid);
			List<? extends TabularColumn> columns = grid.getColumns();
			for (TabularColumn column : columns) {
				if ((rowDocument != null) && (column instanceof DataGridBoundColumn bound)) {
					String columnBinding = bound.getBinding();
					if (columnBinding != null) {
						WidgetReference reference = bound.getInputWidget();
						Bound input = (reference == null) ? null : reference.getWidget();
						if (input == null) {
							visitDefaultBinding(columnBinding, rowDocument, grid);
						}
						else {
							// the input names no binding of its own: its cell is the column's
							visitInput(input, columnBinding, grid);
						}
					}
				}
			}
		}
	}

	/**
	 * Visits a reusable component fragment by traversing its contained metadata.
	 *
	 * @param component the component metadata defining the fragment
	 */
	private void visitComponent(@Nonnull Component component) {
		for (MetaData widget : component.getFragment(customer, currentUxUi).getContained()) {
			visitWidget(widget);
		}
	}

	/**
	 * Visits all view actions and registers corresponding locators.
	 */
	private final void visitActions() {
		String name = view.getName();

		for (Action action : view.getActions()) {
			visit(name, (ActionImpl) action);
		}
	}

	/**
	 * Dispatches an action to implicit-action expansion or custom-action handling.
	 *
	 * @param viewName the current view name
	 * @param action the action metadata definition
	 */
	private void visit(@Nullable String viewType, @Nonnull ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (classNamed(implicitName)) {
			// an upload, download, import or export action's button carries the action's name (its class)
			visitAction(action.getName());
		}
		else if (implicitName != null) {
			visit(viewType, implicitName);
			// a named action of an implicit kind is also addressed by its name
			String name = action.getName();
			if ((! name.equals(implicitName.toString())) && (! ImplicitActionName.DEFAULTS.equals(implicitName))) {
				automationContext.put(name, Locator.widget(viewName, Kind.button, implicitName.toString(), null, null, null));
			}
		} else {
			String name = action.getName();
			automationContext.put(name, Locator.widget(viewName, Kind.button, buttonName(name), null, null, null));
		}
	}

	/**
	 * Expands implicit actions into the SmartClient locator set appropriate for the current view type.
	 *
	 * @param viewType the current view name
	 * @param implicitName the implicit action being expanded
	 */
	private void visit(@Nullable String viewType, @Nonnull ImplicitActionName implicitName) {
		if (ImplicitActionName.DEFAULTS.equals(implicitName)) {
			if (ViewType.list.toString().equals(viewType)) {
				visit(viewType, ImplicitActionName.New);
			} else {
				for (ImplicitActionName value : ImplicitActionName.values()) {
					if (ImplicitActionName.DEFAULTS != value
							&& ImplicitActionName.New != value
							&& ImplicitActionName.Report != value
							&& ImplicitActionName.BizExport != value
							&& ImplicitActionName.BizImport != value
							&& ImplicitActionName.Download != value
							&& ImplicitActionName.Upload != value
							&& ImplicitActionName.Navigate != value
							&& ImplicitActionName.Print != value) {
						visit(viewType, value);
					}
				}
			}
		} else if (ImplicitActionName.Remove.equals(implicitName) ||
					ImplicitActionName.ZoomOut.equals(implicitName) ||
					ImplicitActionName.OK.equals(implicitName) ||
					ImplicitActionName.Save.equals(implicitName) ||
					ImplicitActionName.Cancel.equals(implicitName) ||
					ImplicitActionName.Delete.equals(implicitName) ||
					ImplicitActionName.Add.equals(implicitName) ||
					ImplicitActionName.Edit.equals(implicitName)) {
			visitAction(implicitName.toString());
		}
	}

	/**
	 * Registers an action button locator by action name; implicit actions use their implicit name.
	 *
	 * @param actionName the action name
	 */
	private final void visitAction(@Nonnull String actionName) {
		automationContext.put(actionName, Locator.widget(viewName, Kind.button, actionName, null, null, null));
	}

	/**
	 * Registers tab-selection locators by localised tab title, under {@code <title> Tab} and,
	 * for nested tabs, under the {@code ::}-separated path {@code <outer>::<inner> Tab}.
	 *
	 * @param title the localised tab title
	 */
	private final void visitTab(@Nonnull String title) {
		Locator locator = Locator.widget(viewName, Kind.tab, title, null, null, null);
		automationContext.put(title + " Tab", locator);
		if (tabPath.size() > 1) {
			automationContext.put(String.join("::", tabPath) + " Tab", locator);
		}
	}

	/**
	 * Localises a tab title.
	 *
	 * @param tab the tab
	 * @return the localised title
	 */
	private static @Nonnull String tabTitle(@Nonnull Tab tab) {
		String title = tab.getTitle();
		String internationalised = Util.i18n(title);
		return (internationalised == null) ? title : internationalised;
	}

	/**
	 * Sanitises a binding the way the SmartClient renderer names form items and grids.
	 *
	 * @param binding the binding
	 * @return the sanitised binding
	 */
	@SuppressWarnings("null") // a non-null binding sanitises to a non-null name
	private static @Nonnull String sanitise(@Nonnull String binding) {
		return BindUtil.sanitiseBinding(binding);
	}
}

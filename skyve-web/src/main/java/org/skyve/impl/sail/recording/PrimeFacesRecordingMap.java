package org.skyve.impl.sail.recording;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.primefaces.component.column.Column;
import org.primefaces.component.datatable.DataTable;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.sail.execution.pf.PrimeFacesAutomationContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateEditContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateListContext;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

/**
 * Builds the client-id map that the PrimeFaces SAIL recorder names gestures with: for each element of a PrimeFaces
 * page that a SAIL step can act on, its client id and what the step calls it.
 *
 * <p>PrimeFaces client ids are generated, so the recorder cannot read a binding or an action name from the page, as
 * the SmartClient recorder does. The map is built with the same mock render that the PrimeFaces executors use to find
 * elements ({@link PrimeFacesAutomationContext}, with the default component and layout builders of {@code s:view}).
 * It is the reverse of what the executors look up, so a step named from the map finds the same element when replayed.
 *
 * <p>The views are built as the current user ({@code CORE.getUser()}), with their real roles, as the page was. An
 * element the user cannot see is not rendered, which shifts the generated ids, so a map built for another user would
 * name the wrong elements. Build it only while the session records ({@link SailRecording#isRecording}); it costs a
 * component tree build of the view.
 *
 * <p>The map is JSON:
 * <pre>
 * {"module":"m", "document":"D", "uxui":"tablet",      (an edit page: its edit view, and its create view if it has one)
 *  "list":{"module":"m","document":"D","query":"q"},   (a list page instead: the list the context shows)
 *  "ids":{"s12":{"binding":"text","widget":"textField"},
 *         "s40":{"step":"save"},
 *         "s41":{"step":"action","name":"RunFromToolbar"},
 *         "s42":{"step":"upload","action":"ActionsUpload"},
 *         "s50":{"step":"tabSelect","path":"Rows"},
 *         "s60":{"dataGrid":"rows"}, "s60:s70":{"step":"dataGridZoom","binding":"rows"}, ...
 *         "s80":{"listGrid":"m.qContacts","module":"m","query":"qContacts"}, "s80:s91":{"step":"listGridZoom",...}, ...},
 *  "grids":{"rows":{"columns":[{"binding":"name"},{"title":"Row action"}]},
 *           "m.qContacts":{"columns":[{"binding":"name","title":"Name"}]}}}
 * </pre>
 * <ul>
 * <li>A bound widget gives its binding and the widget's kind ({@code textField}, {@code checkBox}, {@code combo},
 * {@code lookupDescription}, {@code contentUpload} and so on); which step a gesture on it makes is the recorder's
 * choice. Every other entry names its step: {@code ok}, {@code save}, {@code delete}, {@code cancel}, {@code zoomOut},
 * {@code remove}, {@code action} (with {@code name}), {@code upload} (with {@code action}), {@code tabSelect} (with
 * {@code path}), {@code zoomIn}, and the data grid and list grid steps for their New, zoom, remove and select controls.</li>
 * <li>A grid's own entry ({@code dataGrid} or {@code listGrid}) names the grid as its steps do, and its columns are
 * under {@code grids} in the grid's order. A list grid's attributes are what a list grid step in the view would name it
 * with; on a list page, the context's own list is {@code list}, and its steps need no attributes. A column of a list
 * grid rendered as a PrimeFaces data table also has its header's client id ({@code id}) and, if it can be filtered, the
 * comparison its filter makes ({@code operator}: {@code contains} or {@code equals}), from which the recorder writes
 * the table's filters and sorts. A date, date time, timestamp or time column of a Vue list grid has the format pattern
 * of its converter ({@code format}), in which the recorder writes the column's filter value.</li>
 * <li>An element inside a data table has a row in its page id, which the map leaves out: the page's
 * {@code s60:3:s70} is the map's {@code s60:s70} ({@link PrimeFacesAutomationContext#clientId(UIComponent, Integer)}).
 * The recorder takes the row from the page.</li>
 * <li>When one element is reached by several names (a model's list grid is also named by its model alone), the
 * fullest name is kept.</li>
 * <li>A Vue list grid (the default for lists) makes its toolbar, rows and buttons in the browser, so only its container
 * is in the map; the page marks them with {@code skyve-vue-list-grid} classes. A list grid rendered as a PrimeFaces
 * data table also has its New, zoom and select controls.</li>
 * </ul>
 *
 * <p>Threading: stateless; each call builds its own component tree on the calling thread, under a mock Faces context
 * that replaces the thread's current one (a request's) for the build. In a real Faces context the builders would take
 * their generated ids from the request's page bean, so the ids would continue the page's and the page bean's counter
 * would move on.
 */
public final class PrimeFacesRecordingMap {
	private static final String STEP = "step";
	private static final String BINDING = "binding";
	private static final String MODULE = "module";
	private static final String DOCUMENT = "document";
	private static final String QUERY = "query";
	private static final String MODEL = "model";
	private static final String TITLE = "title";
	private static final String TAB_SUFFIX = " Tab";
	private static final String CONTAINS = "contains";
	private static final String EQUALS = "equals";

	private PrimeFacesRecordingMap() {
		// static only
	}

	/**
	 * Builds the map for an edit page: the document's edit view, and its create view if it has one, as the page
	 * renders them together.
	 *
	 * <p>Complexity: one component tree build of each view, then O(c) in the components the views register.
	 *
	 * @param moduleName	the document's module; must not be {@code null}
	 * @param documentName	the document; must not be {@code null}
	 * @param uxui	the UX/UI the page is rendered in; must not be {@code null}
	 * @param userAgentType	the page's user agent type; must not be {@code null}
	 * @return the map as JSON; never {@code null}
	 */
	public static @Nonnull String edit(@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String uxui,
										@Nonnull UserAgentType userAgentType) {
		PushEditContext push = new PushEditContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		PrimeFacesAutomationContext context = newContext(moduleName, documentName, uxui, userAgentType);
		context.setViewType(ViewType.edit);
		try (MockFacesContext ignored = MockFacesContext.replacingCurrent()) {
			context.generate(new PrimeFacesGenerateEditContext(push, new SkyveComponentBuilderChain(), new ResponsiveLayoutBuilder()));
		}

		Map<String, Object> result = new LinkedHashMap<>();
		result.put(MODULE, moduleName);
		result.put(DOCUMENT, documentName);
		result.put("uxui", uxui);
		putIdsAndGrids(context, null, result);
		return JSON.marshall(result);
	}

	/**
	 * Builds the map for a list page: the list grid that shows a document's default query, a query or a model, named as
	 * {@code navigateList} names it.
	 *
	 * <p>Complexity: one component tree build of the list grid.
	 *
	 * @param moduleName	the list's module; must not be {@code null}
	 * @param documentName	the document whose default query is listed, or the model's document; or {@code null}
	 * @param queryName	the query listed, or {@code null}
	 * @param modelName	the model listed, or {@code null}
	 * @param uxui	the UX/UI the page is rendered in; must not be {@code null}
	 * @param userAgentType	the page's user agent type; must not be {@code null}
	 * @return the map as JSON; never {@code null}
	 */
	public static @Nonnull String list(@Nonnull String moduleName,
										@Nullable String documentName,
										@Nullable String queryName,
										@Nullable String modelName,
										@Nonnull String uxui,
										@Nonnull UserAgentType userAgentType) {
		PushListContext push = new PushListContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		// a document's list shows its default query, which the list grid finds by the document's name, as the page does
		push.setQueryName(((queryName == null) && (modelName == null)) ? documentName : queryName);
		push.setModelName(modelName);
		PrimeFacesAutomationContext context = newContext(moduleName, documentName, uxui, userAgentType);
		context.setViewType(ViewType.list);
		context.setList(push);
		try (MockFacesContext ignored = MockFacesContext.replacingCurrent()) {
			context.generate(new PrimeFacesGenerateListContext(push, new SkyveComponentBuilderChain()));
		}

		Map<String, Object> list = new LinkedHashMap<>();
		putIfNotNull(list, MODULE, moduleName);
		putIfNotNull(list, DOCUMENT, documentName);
		putIfNotNull(list, QUERY, queryName);
		putIfNotNull(list, MODEL, modelName);

		Map<String, Object> result = new LinkedHashMap<>();
		result.put("uxui", uxui);
		result.put("list", list);
		putIdsAndGrids(context, push.getIdentifier(context), result);
		return JSON.marshall(result);
	}

	private static @Nonnull PrimeFacesAutomationContext newContext(@Nonnull String moduleName,
																	@Nullable String documentName,
																	@Nonnull String uxui,
																	@Nonnull UserAgentType userAgentType) {
		PrimeFacesAutomationContext result = new PrimeFacesAutomationContext();
		result.setModuleName(moduleName);
		result.setDocumentName(documentName);
		result.setUxui(uxui);
		result.setUserAgentType(userAgentType);
		return result;
	}

	/**
	 * Puts "ids" (client id to entry) and "grids" (grid name to columns) into the result.
	 *
	 * @param context	the generated context
	 * @param listIdentifier	the identifier of a list page's own list grid, or null for an edit page
	 * @param result	the map to put into
	 */
	private static void putIdsAndGrids(@Nonnull PrimeFacesAutomationContext context,
										@Nullable String listIdentifier,
										@Nonnull Map<String, Object> result) {
		Map<String, Map<String, Object>> ids = new TreeMap<>();
		// the identifier each client id was mapped from, to keep the fullest name when an element has several
		Map<String, String> idIdentifiers = new TreeMap<>();
		Map<String, Object> grids = new TreeMap<>();
		for (String identifier : context.getIdentifiers()) {
			List<UIComponent> components = context.getFacesComponents(identifier);
			List<Object> widgets = context.getSkyveWidgets(identifier);
			if ((components == null) || (widgets == null)) {
				continue;
			}
			int size = components.size();
			for (int i = 0; i < size; i++) {
				Object widget = widgets.get(i);
				UIComponent component = components.get(i);
				Map<String, Object> entry = entry(identifier, widget, listIdentifier);
				if ((entry != null) && inPage(component, widget)) {
					putId(PrimeFacesAutomationContext.clientId(component), identifier, entry, ids, idIdentifiers);
				}
				putGrid(context, identifier, widget, component, grids);
			}
		}
		result.put("ids", ids);
		result.put("grids", grids);
	}

	/**
	 * A component with no id (a Vue list grid's placeholder child) is not in the page, and a bound widget with no parent
	 * (a data grid column's widget) is not in the tree the page renders, so neither has a client id to give.
	 */
	private static boolean inPage(@Nonnull UIComponent component, @Nonnull Object widget) {
		return (component.getId() != null) && ((component.getParent() != null) || (! (widget instanceof Bound)));
	}

	/** Maps a client id to its entry, keeping the fullest name when an element is reached by several. */
	private static void putId(@Nonnull String clientId,
								@Nonnull String identifier,
								@Nonnull Map<String, Object> entry,
								@Nonnull Map<String, Map<String, Object>> ids,
								@Nonnull Map<String, String> idIdentifiers) {
		String mappedFrom = idIdentifiers.get(clientId);
		if ((mappedFrom == null) || (parts(identifier) > parts(mappedFrom))) {
			ids.put(clientId, entry);
			idIdentifiers.put(clientId, identifier);
		}
	}

	private static int parts(@Nonnull String identifier) {
		return identifier.split("\\.", -1).length;
	}

	/**
	 * Returns the entry for a collected component, or null when it is not one a step acts on.
	 */
	@SuppressWarnings("java:S3776") // a flat dispatch on the collected widget's kind
	private static @Nullable Map<String, Object> entry(@Nonnull String identifier,
														@Nonnull Object widget,
														@Nullable String listIdentifier) {
		Map<String, Object> result = new LinkedHashMap<>();
		if (widget instanceof ImplicitActionName implicitName) {
			putAction(implicitName.toString(), implicitName, result);
		}
		else if (widget instanceof Action action) {
			putAction(action.getName(), action.getImplicitName(), result);
		}
		else if (widget instanceof org.skyve.impl.metadata.view.widget.Button) {
			putAction(identifier, null, result);
		}
		else if (widget instanceof Tab) {
			result.put(STEP, "tabSelect");
			result.put("path", identifier.endsWith(TAB_SUFFIX) ? identifier.substring(0, identifier.length() - TAB_SUFFIX.length()) : identifier);
		}
		else if (widget instanceof ZoomIn zoomIn) {
			result.put(STEP, "zoomIn");
			result.put(BINDING, zoomIn.getBinding());
		}
		else if (widget instanceof DataGrid grid) {
			putDataGrid(identifier, grid, result);
		}
		else if ((widget instanceof String) || (widget instanceof ListGrid)) {
			putListGrid(identifier, widget instanceof ListGrid, listIdentifier, result);
		}
		else if (widget instanceof Bound bound) {
			result.put(BINDING, bound.getBinding());
			String kind = widget.getClass().getSimpleName();
			result.put("widget", Character.toLowerCase(kind.charAt(0)) + kind.substring(1));
		}
		else {
			return null;
		}
		return result;
	}

	/** Puts a data grid's entry: the grid itself, or one of its controls' steps. */
	private static void putDataGrid(@Nonnull String identifier, @Nonnull DataGrid grid, @Nonnull Map<String, Object> result) {
		String binding = grid.getBinding();
		String gesture = gesture(identifier, binding);
		if (gesture == null) {
			result.put("dataGrid", binding);
		}
		else {
			result.put(STEP, "dataGrid" + gesture);
			result.put(BINDING, binding);
		}
	}

	/**
	 * Puts a list grid's entry: the grid itself (its identifier), or one of its controls' steps (its identifier is the
	 * grid's with the control's suffix), with the grid's attributes unless it is the list page's own list.
	 */
	private static void putListGrid(@Nonnull String identifier,
										boolean control,
										@Nullable String listIdentifier,
										@Nonnull Map<String, Object> result) {
		String gridIdentifier = control ? identifier.substring(0, identifier.lastIndexOf('.')) : identifier;
		String gesture = control ? gesture(identifier, gridIdentifier) : null;
		if (gesture == null) {
			result.put("listGrid", gridIdentifier);
		}
		else {
			result.put(STEP, "listGrid" + gesture);
		}
		if (! gridIdentifier.equals(listIdentifier)) {
			putListGridAttributes(gridIdentifier, result);
		}
	}

	/**
	 * Returns "New", "Zoom", "Remove" or "Select" for a grid control's identifier ({@code grid.new} and so on), or null
	 * for the grid itself.
	 */
	private static @Nullable String gesture(@Nonnull String identifier, @Nullable String gridIdentifier) {
		if ((gridIdentifier == null) || identifier.equals(gridIdentifier)) {
			return null;
		}
		String suffix = identifier.substring(gridIdentifier.length() + 1);
		return switch (suffix) {
			case "new" -> "New";
			case "zoom" -> "Zoom";
			case "remove" -> "Remove";
			case "select" -> "Select";
			default -> null;
		};
	}

	private static void putAction(@Nonnull String name, @Nullable ImplicitActionName implicitName, @Nonnull Map<String, Object> result) {
		ImplicitActionName implicit = implicitName;
		if (implicit == null) {
			for (ImplicitActionName value : ImplicitActionName.values()) {
				if (value.toString().equals(name)) {
					implicit = value;
				}
			}
		}
		if (implicit != null) {
			switch (implicit) {
				case OK:
					result.put(STEP, "ok");
					return;
				case Save:
					result.put(STEP, "save");
					return;
				case Delete:
					result.put(STEP, "delete");
					return;
				case Cancel:
					result.put(STEP, "cancel");
					return;
				case ZoomOut:
					result.put(STEP, "zoomOut");
					return;
				case Remove:
					result.put(STEP, "remove");
					return;
				case Upload:
					result.put(STEP, "upload");
					result.put("action", name);
					return;
				default:
					break;
			}
		}
		result.put(STEP, "action");
		result.put("name", name);
	}

	/**
	 * Puts a view's list grid attributes, parsed from its identifier: {@code module.query}, or
	 * {@code module.document.model} (see the collecting component builder).
	 */
	private static void putListGridAttributes(@Nonnull String gridIdentifier, @Nonnull Map<String, Object> result) {
		String[] parts = gridIdentifier.split("\\.");
		result.put(MODULE, parts[0]);
		if (parts.length == 3) {
			result.put(DOCUMENT, parts[1]);
			result.put(MODEL, parts[2]);
		}
		else if (parts.length == 2) {
			result.put(QUERY, parts[1]);
		}
	}

	/**
	 * Puts a grid's columns under its name, once.
	 */
	private static void putGrid(@Nonnull PrimeFacesAutomationContext context,
									@Nonnull String identifier,
									@Nonnull Object widget,
									@Nonnull UIComponent component,
									@Nonnull Map<String, Object> grids) {
		if (widget instanceof DataGrid grid) {
			String binding = grid.getBinding();
			if ((binding != null) && (! grids.containsKey(binding))) {
				grids.put(binding, Map.of("columns", dataGridColumns(grid)));
			}
		}
		else if ((widget instanceof String) && (! grids.containsKey(identifier))) {
			ListModel<Bean> model = context.getListModel(identifier);
			if (model != null) {
				grids.put(identifier, Map.of("columns", listGridColumns(model, findChild(component, DataTable.class))));
			}
		}
	}

	/**
	 * A data grid's columns: each one's binding, if it is bound, and title.
	 */
	private static @Nonnull List<Map<String, Object>> dataGridColumns(@Nonnull DataGrid grid) {
		List<DataGridColumn> columns = grid.getColumns();
		List<Map<String, Object>> result = new ArrayList<>(columns.size());
		for (DataGridColumn column : columns) {
			Map<String, Object> c = new LinkedHashMap<>();
			if (column instanceof DataGridBoundColumn bound) {
				putIfNotNull(c, BINDING, bound.getBinding());
			}
			putIfNotNull(c, TITLE, column.getLocalisedTitle());
			result.add(c);
		}
		return result;
	}

	/**
	 * A list grid's columns, from its model: each one's binding, name, title, and whether it is hidden. A column of a
	 * PrimeFaces data table also has the client id of its header ({@code id}), which its sort request names, and, if
	 * it can be filtered, the comparison its typed or chosen filter makes ({@code operator}). A date, date time,
	 * timestamp or time column of a Vue list grid has its converter's format pattern ({@code format}): the grid's
	 * cells and filter calendar show the converter's format, so a filter on the column is written in it. A Vue list
	 * grid's columns leave out the query's columns that are not projected, as the grid does
	 * ({@code VueListGridScript}).
	 *
	 * @param model	the list grid's model
	 * @param table	the data table that renders the list grid, or null for a Vue list grid
	 */
	static @Nonnull List<Map<String, Object>> listGridColumns(@Nonnull ListModel<Bean> model, @Nullable DataTable table) {
		Map<String, Column> tableColumns = tableColumnsByField(table);
		List<MetaDataQueryColumn> columns = model.getColumns();
		List<Map<String, Object>> result = new ArrayList<>(columns.size());
		for (MetaDataQueryColumn column : columns) {
			if ((table == null) && (column instanceof MetaDataQueryProjectedColumn projected) && (! projected.isProjected())) {
				continue;
			}
			Map<String, Object> c = new LinkedHashMap<>();
			String binding = column.getBinding();
			String name = column.getName();
			putIfNotNull(c, BINDING, binding);
			putIfNotNull(c, "name", name);
			putIfNotNull(c, TITLE, model.determineColumnTitle(column));
			if (column.isHidden()) {
				c.put("hidden", Boolean.TRUE);
			}
			// a column with neither a name nor a binding (a list model's column made in code) is no data table field
			String field = (name != null) ? name : binding;
			Column tableColumn = (field == null) ? null : tableColumns.get(field);
			if (tableColumn != null) {
				putTableColumn(model, binding, tableColumn, c);
			}
			else if ((table == null) && (binding != null)) {
				putIfNotNull(c, "format", dateOrTimeFormat(columnAttribute(model, binding)));
			}
			result.add(c);
		}
		return result;
	}

	/** A data table's columns by their field (a list column's name, or else its binding); none for a Vue list grid. */
	private static @Nonnull Map<String, Column> tableColumnsByField(@Nullable DataTable table) {
		Map<String, Column> result = new TreeMap<>();
		if (table != null) {
			for (UIComponent child : table.getChildren()) {
				if ((child instanceof Column tableColumn) && (tableColumn.getField() != null)) {
					result.put(tableColumn.getField(), tableColumn);
				}
			}
		}
		return result;
	}

	/** Puts a data table column's header id and, if it can be filtered, its filter's comparison. */
	private static void putTableColumn(@Nonnull ListModel<Bean> model,
										@Nullable String binding,
										@Nonnull Column tableColumn,
										@Nonnull Map<String, Object> c) {
		c.put("id", PrimeFacesAutomationContext.clientId(tableColumn));
		if ((binding != null) && tableColumn.isFilterable()) {
			c.put("operator", filterOperator(model, binding));
		}
	}

	/**
	 * The comparison a PrimeFaces data table's filter of a column makes, as {@code SkyveLazyDataModel} applies it:
	 * {@code contains} for text without a domain, a variant domain's description, an association's business key and
	 * the implicit bizKey; {@code equals} otherwise (a constant domain's combo, a boolean's check box, numbers, dates).
	 */
	private static @Nonnull String filterOperator(@Nonnull ListModel<Bean> model, @Nonnull String binding) {
		Attribute attribute = columnAttribute(model, binding);
		if (attribute == null) {
			return binding.endsWith(Bean.BIZ_KEY) ? CONTAINS : EQUALS;
		}
		DomainType domain = attribute.getDomainType();
		AttributeType type = attribute.getAttributeType();
		if ((domain == DomainType.variant) || (type == AttributeType.association)) {
			return CONTAINS;
		}
		boolean textual = (type == AttributeType.text) || (type == AttributeType.memo) || (type == AttributeType.markup) || (type == AttributeType.colour);
		return (textual && (domain == null)) ? CONTAINS : EQUALS;
	}

	/** The attribute a list grid column's binding reaches from its model's driving document, or null (an implicit attribute). */
	private static @Nullable Attribute columnAttribute(@Nonnull ListModel<Bean> model, @Nonnull String binding) {
		Document driving = model.getDrivingDocument();
		if (driving == null) {
			return null;
		}
		Customer customer = CORE.getUser().getCustomer();
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, customer.getModule(driving.getOwningModuleName()), driving, binding);
		return target.getAttribute();
	}

	/**
	 * The format pattern of a date, date time, timestamp or time attribute's converter (the customer's default converter
	 * for its type when it names none), or null for another attribute.
	 */
	private static @Nullable String dateOrTimeFormat(@Nullable Attribute attribute) {
		if (attribute instanceof ConvertibleField field) {
			AttributeType type = field.getAttributeType();
			if ((type == AttributeType.date) || (type == AttributeType.dateTime) || (type == AttributeType.timestamp) || (type == AttributeType.time)) {
				Converter<?> converter = field.getConverterForCustomer(CORE.getUser().getCustomer());
				return (converter == null) ? null : converter.getFormatPattern();
			}
		}
		return null;
	}

	/** The component, or the first of its descendants, that is of the type; or null. */
	private static @Nullable <T extends UIComponent> T findChild(@Nonnull UIComponent component, @Nonnull Class<T> type) {
		if (type.isInstance(component)) {
			return type.cast(component);
		}
		for (UIComponent child : component.getChildren()) {
			T result = findChild(child, type);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	private static void putIfNotNull(@Nonnull Map<String, Object> map, @Nonnull String key, @Nullable Object value) {
		if (value != null) {
			map.put(key, value);
		}
	}
}

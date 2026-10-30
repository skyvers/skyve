package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import jakarta.el.ValueExpression;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.context.FacesContext;

/**
 * This component isn't in the skyve.taglib.xml and has no @Component annotation and is not meant to be used on an XHTML page. 
 * The resources skyvevue/index.js and skyvevue/index.css are brought in view view.xhtml (if enabled)
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate Vue list-grid script keys.
public class VueListGridScript extends UIOutput {
	private String containerId;

	private String owningModuleName;
	private String owningDocumentName;
	private String drivingModuleName;
	private String drivingDocumentName;
	private String queryName;
	private String modelName;

	private String contextId;

	private Boolean showAdd;
	private Boolean showZoom;
	private Boolean showFilter;
	private Boolean showSummary;
	private Boolean showSnap;
	private String stickyHeaderAnchorSelector;
	
	private String selectedRemoteCommand;

	// Used to resolve the current bean, conversation and view binding each time the grid is rendered
	private String managedBeanName;
	// Filter parameters and parameters from the view's listGrid definition - see setParameters()
	private List<Map<String, String>> gridParameters;

	// The UX/UI name used to determine default column widths and alignment
	private String uxui;
	// Whether the row context menu (View Detail/Popout Detail) is shown
	private boolean showContextMenu = true;

	// NB attribute keys must not match a property of this class (eg "managedBeanName")
	// as Faces would call the property setter on put() and recurse
	private static final String MANAGED_BEAN_ATTRIBUTE = "managedBean";
	private static final String UXUI_ATTRIBUTE = "uxui";
	private static final String CONTEXT_MENU_ATTRIBUTE = "contextMenu";

	/**
	 * The name of the value expression that disables the add (new) button when true.
	 */
	public static final String DISABLE_ADD_VALUE_EXPRESSION = "disableAdd";
	/**
	 * The name of the value expression that disables zooming (button, row click and context menu) when true.
	 */
	public static final String DISABLE_ZOOM_VALUE_EXPRESSION = "disableZoom";

	// Keys of the maps in the params list
	private static final String PARAM_NAME_KEY = "name";
	private static final String PARAM_OPERATOR_KEY = "operator";
	private static final String PARAM_VALUE_KEY = "value";
	private static final String PARAM_VALUE_BINDING_KEY = "valueBinding";
	private static final String PARAM_NEW_NAME_KEY = "newName";
	
	/**
	 * This exists so that Faces can instantiate the view tree and reinstate the attributes.
	 */
	public VueListGridScript() {
		// nothing to see here
	}
	
	/**
	 * This is called to set state.
	 *
	 * @param containerId the DOM container ID where the Vue grid is mounted
	 * @param owningModuleName the owning module name
	 * @param owningDocumentName the owning document name
	 * @param drivingModuleName the driving module name
	 * @param drivingDocumentName the driving document name
	 * @param queryName the optional query name
	 * @param modelName the optional model name
	 * @param contextId the optional context ID used for state restoration
	 * @param showAdd whether add actions are visible
	 * @param showZoom whether zoom actions are visible
	 * @param showFilter whether filter controls are visible
	 * @param showSummary whether summary controls are visible
	 * @param showSnap whether snap controls are visible
	 * @param selectedRemoteCommand the optional remote command for selected row callbacks
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public VueListGridScript(String containerId,
								String owningModuleName,
								String owningDocumentName,
								String drivingModuleName,
								String drivingDocumentName,
								String queryName,
								String modelName,
								String contextId,
								boolean showAdd,
								boolean showZoom,
								boolean showFilter,
								boolean showSummary,
								boolean showSnap,
								String selectedRemoteCommand) {
		this(containerId,
				owningModuleName,
				owningDocumentName,
				drivingModuleName,
				drivingDocumentName,
				queryName,
				modelName,
				contextId,
				showAdd,
				showZoom,
				showFilter,
				showSummary,
				showSnap,
				selectedRemoteCommand,
				null);
	}

	/**
	 * This is called to set state.
	 *
	 * @param containerId the DOM container ID where the Vue grid is mounted
	 * @param owningModuleName the owning module name
	 * @param owningDocumentName the owning document name
	 * @param drivingModuleName the driving module name
	 * @param drivingDocumentName the driving document name
	 * @param queryName the optional query name
	 * @param modelName the optional model name
	 * @param contextId the optional context ID used for state restoration
	 * @param showAdd whether add actions are visible
	 * @param showZoom whether zoom actions are visible
	 * @param showFilter whether filter controls are visible
	 * @param showSummary whether summary controls are visible
	 * @param showSnap whether snap controls are visible
	 * @param selectedRemoteCommand the optional remote command for selected row callbacks
	 * @param stickyHeaderAnchorSelector optional selector used to anchor the sticky header
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public VueListGridScript(String containerId,
								String owningModuleName,
								String owningDocumentName,
								String drivingModuleName,
								String drivingDocumentName,
								String queryName,
								String modelName,
								String contextId,
								boolean showAdd,
								boolean showZoom,
								boolean showFilter,
								boolean showSummary,
								boolean showSnap,
								String selectedRemoteCommand,
								String stickyHeaderAnchorSelector) {
		Map<String, Object> attributes = getAttributes();

		this.containerId = containerId;
		attributes.put("containerId", containerId);

		this.owningModuleName = owningModuleName;
		attributes.put("owningModuleName", owningModuleName);
		this.owningDocumentName = owningDocumentName;
		attributes.put("owningDocumentName", owningDocumentName);
		this.drivingModuleName = drivingModuleName;
		attributes.put("drivingModuleName", drivingModuleName);
		this.drivingDocumentName = drivingDocumentName;
		attributes.put("drivingDocumentName", drivingDocumentName);
		this.queryName = queryName;
		if (queryName != null) {
			attributes.put("queryName", queryName);
		}
		this.modelName = modelName;
		if (modelName != null) {
			attributes.put("modelName", modelName);
		}
		
		this.contextId = contextId;
		if (contextId != null) {
			attributes.put("contextId", contextId);
		}

		this.showAdd = Boolean.valueOf(showAdd);
		attributes.put("showAdd", this.showAdd);
		this.showZoom = Boolean.valueOf(showZoom);
		attributes.put("showZoom", this.showZoom);
		this.showFilter = Boolean.valueOf(showFilter);
		attributes.put("showFilter", this.showFilter);
		this.showSummary = Boolean.valueOf(showSummary);
		attributes.put("showSummary", this.showSummary);
		this.showSnap = Boolean.valueOf(showSnap);
		attributes.put("showSnap", this.showSnap);

		this.selectedRemoteCommand = selectedRemoteCommand;
		if (selectedRemoteCommand != null) {
			attributes.put("selectedRemoteCommand", selectedRemoteCommand);
		}

		this.stickyHeaderAnchorSelector = stickyHeaderAnchorSelector;
		if (stickyHeaderAnchorSelector != null) {
			attributes.put("stickyHeaderAnchorSelector", stickyHeaderAnchorSelector);
		}
	}

	/**
	 * Sets the name of the managed bean (the {@link FacesView}) that holds the current bean and conversation.
	 * This is required for an embedded list grid so that its parameter values, conversation id and
	 * view binding are current whenever the grid is rendered.
	 *
	 * @param managedBeanName the managed bean name, or {@code null}
	 */
	public void setManagedBeanName(String managedBeanName) {
		this.managedBeanName = managedBeanName;
		Map<String, Object> attributes = getAttributes();
		if (managedBeanName == null) {
			attributes.remove(MANAGED_BEAN_ATTRIBUTE);
		}
		else {
			attributes.put(MANAGED_BEAN_ATTRIBUTE, managedBeanName);
		}
	}

	/**
	 * Sets the UX/UI name used to determine default column widths and alignment.
	 *
	 * @param uxui the UX/UI name, or {@code null}
	 */
	public void setUxUiName(String uxui) {
		this.uxui = uxui;
		Map<String, Object> attributes = getAttributes();
		if (uxui == null) {
			attributes.remove(UXUI_ATTRIBUTE);
		}
		else {
			attributes.put(UXUI_ATTRIBUTE, uxui);
		}
	}

	/**
	 * Sets whether the row context menu (View Detail/Popout Detail) is shown.
	 *
	 * @param showContextMenu {@code true} to show the context menu
	 */
	public void setShowContextMenu(boolean showContextMenu) {
		this.showContextMenu = showContextMenu;
		getAttributes().put(CONTEXT_MENU_ATTRIBUTE, Boolean.valueOf(showContextMenu));
	}

	/**
	 * Sets the filter parameters and parameters defined on the list grid in the view.
	 * These are sent to the smartlist servlet the same way the SmartClient list grid sends them -
	 * filter parameters as criteria and parameters as ":name" request parameters.
	 * Values are resolved against the current bean whenever the grid is rendered.
	 *
	 * @param filterParameters the filter parameters, or {@code null}
	 * @param parameters the parameters, or {@code null}
	 */
	public void setParameters(List<FilterParameter> filterParameters, List<Parameter> parameters) {
		ArrayList<Map<String, String>> result = new ArrayList<>();
		if (filterParameters != null) {
			for (FilterParameter parameter : filterParameters) {
				String filterBinding = parameter.getFilterBinding();
				result.add(newParam(BindUtil.sanitiseBinding(filterBinding),
										SmartClientFilterOperator.fromFilterOperator(parameter.getOperator()).name(),
										parameter.getValueBinding(),
										parameter.getValue(),
										filterBinding));
			}
		}
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				String name = parameter.getName();
				result.add(newParam(":" + BindUtil.sanitiseBinding(name),
										SmartClientFilterOperator.equals.name(),
										parameter.getValueBinding(),
										parameter.getValue(),
										name));
			}
		}

		if (result.isEmpty()) {
			gridParameters = null;
			getAttributes().remove("params");
		}
		else {
			gridParameters = result;
			getAttributes().put("params", result);
		}
	}

	private static Map<String, String> newParam(String name,
													String operator,
													String valueBinding,
													String value,
													String newName) {
		Map<String, String> result = new TreeMap<>();
		result.put(PARAM_NAME_KEY, name);
		result.put(PARAM_OPERATOR_KEY, operator);
		if (valueBinding != null) {
			result.put(PARAM_VALUE_BINDING_KEY, valueBinding);
		}
		if (value != null) {
			result.put(PARAM_VALUE_KEY, value);
		}
		result.put(PARAM_NEW_NAME_KEY, newName);
		return result;
	}

	/**
	 * Encodes and emits the generated script payload for the list grid component.
	 *
	 * @param context the current Faces context
	 * @throws IOException if script output cannot be written
	 */
	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		// Always regenerate the script as the conversation, current bean and therefore
		// parameter values can change between renders when the list grid is embedded in a view.
		grabAttributes(getAttributes());
		createScriptOutput(context);

		super.encodeBegin(context);
	}

	/**
	 * Restores transient fields from the component attribute map.
	 *
	 * @param attributes the component attributes map
	 */
	private void grabAttributes(Map<String, Object> attributes) {
		this.containerId = (String) attributes.get("containerId");

		this.owningModuleName = (String) attributes.get("owningModuleName");
		this.owningDocumentName = (String) attributes.get("owningDocumentName");
		this.drivingModuleName = (String) attributes.get("drivingModuleName");
		this.drivingDocumentName = (String) attributes.get("drivingDocumentName");
		this.queryName = (String) attributes.get("queryName");
		this.modelName = (String) attributes.get("modelName");
		
		this.contextId = (String) attributes.get("contextId");
		
		this.showAdd = (Boolean) attributes.get("showAdd");
		this.showZoom = (Boolean) attributes.get("showZoom");
		this.showFilter = (Boolean) attributes.get("showFilter");
		this.showSummary = (Boolean) attributes.get("showSummary");
		this.showSnap = (Boolean) attributes.get("showSnap");
		this.stickyHeaderAnchorSelector = (String) attributes.get("stickyHeaderAnchorSelector");
		
		this.selectedRemoteCommand = (String) attributes.get("selectedRemoteCommand");

		this.managedBeanName = (String) attributes.get(MANAGED_BEAN_ATTRIBUTE);
		this.uxui = (String) attributes.get(UXUI_ATTRIBUTE);
		this.showContextMenu = (! Boolean.FALSE.equals(attributes.get(CONTEXT_MENU_ATTRIBUTE)));
		@SuppressWarnings("unchecked")
		List<Map<String, String>> attributeParams = (List<Map<String, String>>) attributes.get("params");
		this.gridParameters = attributeParams;
	}

	/**
	 * Builds the JavaScript bootstrap payload and stores it as this component's value.
	 *
	 * @param context the current Faces context, or {@code null}
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void createScriptOutput(FacesContext context) {
		final User user = CORE.getUser();
		final Customer customer = user.getCustomer();
		Module owningModule = customer.getModule(owningModuleName);
		Document owningDocument = owningModule.getDocument(customer, owningDocumentName);

		// Resolve the current state of the view when the grid is embedded
		FacesView view = resolveView(context);
		String currentContextId = contextId;
		String viewBinding = null;
		Bean currentBean = null;
		if (view != null) {
			AbstractWebContext webContext = view.getWebContext();
			if (webContext != null) {
				currentContextId = webContext.getWebId();
			}
			viewBinding = view.getViewBinding();
			BeanMapAdapter adapter = view.getCurrentBean();
			if (adapter != null) {
				currentBean = adapter.getBean();
			}
		}

		Map<String, Object> params = new TreeMap<>();
		// Mount on the exact client id of the container (if we know it)
		UIComponent parent = getParent();
		params.put("containerId", ((context != null) && (parent != null)) ? parent.getClientId(context) : containerId);
		params.put("owningModule", owningModuleName);
		if (queryName != null) {
			params.put("query", queryName);
		}
		params.put("owningDocument", owningDocumentName);
		if (modelName != null) {
			params.put("model", modelName);
		}
		if (currentContextId != null) {
			params.put("contextId", currentContextId);
		}
		if (viewBinding != null) {
			params.put("contextBinding", BindUtil.sanitiseBinding(viewBinding));
		}
		if (this.gridParameters != null) {
			List<Map<String, Object>> filterParams = new ArrayList<>(this.gridParameters.size());
			Map<String, Object> newParams = new TreeMap<>();
			for (Map<String, String> param : this.gridParameters) {
				String value = resolveParamValue(param, currentBean);
				Map<String, Object> filterParam = new TreeMap<>();
				filterParam.put(PARAM_NAME_KEY, param.get(PARAM_NAME_KEY));
				filterParam.put(PARAM_OPERATOR_KEY, param.get(PARAM_OPERATOR_KEY));
				filterParam.put(PARAM_VALUE_KEY, value);
				filterParams.add(filterParam);
				if (value != null) {
					newParams.put(param.get(PARAM_NEW_NAME_KEY), value);
				}
			}
			params.put("params", filterParams);
			params.put("newParams", newParams);
		}
		params.put("showAdd", showAdd);
		params.put("showZoom", showZoom);
		params.put("showContextMenu", Boolean.valueOf(showContextMenu));
		params.put(DISABLE_ADD_VALUE_EXPRESSION, Boolean.valueOf(evaluateCondition(context, DISABLE_ADD_VALUE_EXPRESSION)));
		params.put(DISABLE_ZOOM_VALUE_EXPRESSION, Boolean.valueOf(evaluateCondition(context, DISABLE_ZOOM_VALUE_EXPRESSION)));
		params.put("showFilter", showFilter);
		params.put("showSummary", showSummary);
		params.put("showSnap", showSnap);
		if (stickyHeaderAnchorSelector != null) {
			params.put("stickyHeaderAnchorSelector", stickyHeaderAnchorSelector);
		}

		if (selectedRemoteCommand != null) {
			Map<String, Object> actions = new TreeMap<>();
			actions.put("selected", selectedRemoteCommand);
		// TODO actions.put("edited", null);
		// TODO actions.put("deleted", null);
			params.put("actions", actions);
		}

		final List<MetaDataQueryColumn> columnDefns;
		final Module drivingModule;
		final Document drivingDocument;
		if (modelName == null) {
			if (queryName == null) {
				throw new IllegalStateException("Query name and model name can't be null");
			}
			MetaDataQueryDefinition queryDefn = owningModule.getMetaDataQuery(queryName);
			if (queryDefn == null) {
				queryDefn = owningModule.getDocumentDefaultQuery(customer, owningDocumentName);
			}

			drivingModule = queryDefn.getDocumentModule(customer);
			drivingModuleName = drivingModule.getName();
			drivingDocumentName = queryDefn.getDocumentName();
			drivingDocument = drivingModule.getDocument(customer, drivingDocumentName);
			columnDefns = queryDefn.getColumns();
		}
		else {
			ListModel<Bean> listModel = owningDocument.getListModel(customer, modelName, true);
			columnDefns = listModel.getColumns();
			drivingDocument = listModel.getDrivingDocument();
			drivingDocumentName = drivingDocument.getName();
			drivingModuleName  = drivingDocument.getOwningModuleName();
			drivingModule = customer.getModule(drivingModuleName);
		}
		params.put("drivingModule", drivingModuleName);
		params.put("drivingDocument", drivingDocumentName);

		List<Map<String, Object>> columns = new ArrayList<>(columnDefns.size());
		for (MetaDataQueryColumn columnDefn : columnDefns) {
			// Don't process non-projected columns
			if ((columnDefn instanceof MetaDataQueryProjectedColumn projected) && 
					(! projected.isProjected())) {
				continue;
			}

			SmartClientQueryColumnDefinition scColDefn = SmartClientViewRenderer.getQueryColumn(user,
																									customer,
																									drivingModule,
																									drivingDocument,
																									columnDefn,
																									true,
																									uxui);
			String binding = columnDefn.getBinding();
			TargetMetaData tmd = Binder.getMetaDataForBinding(customer, drivingModule, drivingDocument, binding);

			columns.add(new ColumnMetaData(columnDefn, scColDefn, tmd, customer).toMap());
		}
		params.put("columns", columns);

		// Called straight away so that, on an ajax re-render, the existing grid is put back in
		// place of the new container before the page is painted (SKYVE.listgrid defers a new mount)
		StringJoiner sj = new StringJoiner(" \n");
		sj.add("<script>")
			.add("  SKYVE.listgrid(")
			// parameter values come from the bean, so don't let them close the script element
			.add(JSON.marshall(params).replace("</", "<\\/"))
			.add("  );")
			.add("</script>");

		setValue(sj.toString());
	}

	/**
	 * Resolve the managed bean for this grid, if it is a {@link FacesView}.
	 *
	 * @param context the current Faces context, or {@code null}
	 * @return the view, or {@code null} if there is no managed bean
	 */
	private FacesView resolveView(FacesContext context) {
		if ((context == null) || (managedBeanName == null)) {
			return null;
		}
		Object result = context.getApplication().evaluateExpressionGet(context, "#{" + managedBeanName + "}", Object.class);
		return (result instanceof FacesView view) ? view : null;
	}

	/**
	 * Evaluate a boolean value expression set on this component (eg a disable condition).
	 *
	 * @param context the current Faces context, or {@code null}
	 * @param name the value expression name
	 * @return {@code true} if the value expression exists and evaluates to true
	 */
	private boolean evaluateCondition(FacesContext context, String name) {
		ValueExpression ve = getValueExpression(name);
		Object result = null;
		if (ve == null) {
			// Faces stores literal value expressions as attribute values
			result = getAttributes().get(name);
		}
		else if (context != null) {
			result = ve.getValue(context.getELContext());
		}
		return Boolean.TRUE.equals(result) || String.valueOf(true).equals(result);
	}

	/**
	 * Resolve a parameter value as a string the smartlist servlet can convert.
	 * Beans are sent as their bizId, enumerations as their code and other values in their serialised form.
	 *
	 * @param param the parameter definition
	 * @param bean the current bean, or {@code null}
	 * @return the value, or {@code null}
	 */
	static String resolveParamValue(Map<String, String> param, Bean bean) {
		String valueBinding = param.get(PARAM_VALUE_BINDING_KEY);
		if (valueBinding == null) {
			String value = param.get(PARAM_VALUE_KEY);
			if ((value != null) && (bean != null) && (value.indexOf('{') >= 0)) {
				return Binder.formatMessage(value, bean);
			}
			return value;
		}

		if (bean == null) {
			return null;
		}
		Object value = Binder.get(bean, valueBinding);
		if (value == null) {
			return null;
		}
		if (value instanceof Bean valueBean) {
			return valueBean.getBizId();
		}
		if (value instanceof Enumeration enumeration) {
			return enumeration.toCode();
		}
		return value.toString();
	}

	/**
	 * Utility class to group together the metadata we need to aggregate
	 */
	private static class ColumnMetaData {
		private final MetaDataQueryColumn mdQueryColumn;
		private final SmartClientQueryColumnDefinition scQueryColumnDefn;
		private final TargetMetaData targetMetaData;
		private final Customer customer;

		private static final Map<String, String> attributeTypeConversions =
				new ImmutableMap.Builder<String, String>().put("decimal2", "numeric")
															.put("decimal5", "numeric")
															.put("decimal10", "numeric")
															.put("integer", "numeric")
															.put("longInteger", "numeric")
															.put("enumeration", "enum")
															.put("bool", "boolean")
															.put("colour", "text")
															.put("geometry", "text")
															.put("memo", "text")
															.put("markup", "text")
															.put("id", "text")
															.build();

		private static final Map<Class<?>, String> implicitTypeConversions =
				new ImmutableMap.Builder<Class<?>, String>().put(String.class, "text").put(Integer.class, "numeric").put(Boolean.class, "boolean").build();

		/**
		 * Creates column metadata used to serialise list-grid column configuration.
		 *
		 * @param mdQueryColumn the metadata query column definition
		 * @param scQueryColumnDefn the SmartClient query-column definition
		 * @param targetMetaData the resolved target metadata for the column binding
		 * @param customer the current customer
		 */
		private ColumnMetaData(MetaDataQueryColumn mdQueryColumn,
								SmartClientQueryColumnDefinition scQueryColumnDefn,
								TargetMetaData targetMetaData,
								Customer customer) {
			this.mdQueryColumn = mdQueryColumn;
			this.scQueryColumnDefn = scQueryColumnDefn;
			this.targetMetaData = targetMetaData;
			this.customer = customer;
		}

		private String getBinding() {
			return scQueryColumnDefn.getName();
		}

		private String getTitle() {
			return scQueryColumnDefn.getTitle();
		}

		private String getType() {
			// Content columns are displayed as a thumbnail image or a link
			if (mdQueryColumn instanceof MetaDataQueryContentColumn contentColumn) {
				return DisplayType.thumbnail.equals(contentColumn.getDisplay()) ? "image" : "link";
			}

			Attribute attribute = targetMetaData.getAttribute();
			if (attribute == null) {
				// Defaulting to text for unhandled implicit type attributes
				Class<?> implicitAttrType = targetMetaData.getType();
				return implicitTypeConversions.getOrDefault(implicitAttrType, "text");
			}

			AttributeType type = attribute.getAttributeType();
			// Treat constant domains like enumerations
			if ((AttributeType.text.equals(type)) && DomainType.constant.equals(attribute.getDomainType())) {
				type = AttributeType.enumeration;
			}
			return flattenType(type.name());
		}

		/**
		 * Flattens framework attribute types into grid field types.
		 *
		 * @param inType the source attribute type
		 * @return the flattened field type
		 */
		private static String flattenType(String inType) {
			String resultType = attributeTypeConversions.get(inType);
			if (resultType != null) {
				return resultType;
			}

			return inType;
		}

		private String getConverterName() {
			return Optional.ofNullable(targetMetaData.getAttribute()).filter(ConvertibleField.class::isInstance)
					.map(ConvertibleField.class::cast).map(cf -> cf.getConverterForCustomer(customer))
					.map(ConverterName::valueOf).map(ConverterName::name).orElse(null);
		}

		private boolean isSortable() {
			if (mdQueryColumn instanceof MetaDataQueryProjectedColumn mdcpc) {
				return mdcpc.isSortable();
			}

			return false;
		}

		private boolean isFilterable() {
			return scQueryColumnDefn.isCanFilter();
		}

		private Map<String, String> getValueMap() {
			return scQueryColumnDefn.getValueMap();
		}
		
		private boolean isHidden() {
			return mdQueryColumn.isHidden();
		}

		@Override
		public String toString() {
			return MoreObjects.toStringHelper(this).omitNullValues().add("binding", getBinding())
					.add("title", getTitle()).add("type", getType()).add("converterName", getConverterName())
					.add("sortable", isSortable()).add("filterable", isFilterable()).toString();
		}
		
		private Map<String, Object> toMap() {
			Map<String, Object> result = new TreeMap<>();
			
			result.put("field", getBinding());
			result.put("header", getTitle());
			result.put("type", getType());
			result.put("sortable", Boolean.valueOf(isSortable()));
			result.put("filterable", Boolean.valueOf(isFilterable()));
			result.put("hidden", Boolean.valueOf(isHidden()));
			result.put("escape", Boolean.valueOf(scQueryColumnDefn.isEscape()));
			Integer pixelWidth = scQueryColumnDefn.getPixelWidth();
			if (pixelWidth != null) {
				result.put("pixelWidth", pixelWidth);
			}
			HorizontalAlignment align = scQueryColumnDefn.getAlign();
			if (align != null) {
				result.put("align", align.toTextAlignmentString());
			}
			if (mdQueryColumn instanceof MetaDataQueryContentColumn contentColumn) {
				Integer pixelHeight = contentColumn.getPixelHeight();
				if (pixelHeight != null) {
					result.put("pixelHeight", pixelHeight);
				}
				String emptyThumbnailRelativeFile = contentColumn.getEmptyThumbnailRelativeFile();
				if (emptyThumbnailRelativeFile != null) {
					result.put("emptyThumbnailRelativeFile", emptyThumbnailRelativeFile);
				}
			}
			String converterName = getConverterName();
			if (converterName != null) {
				result.put("converter", converterName);
			}

			Map<String, String> values = getValueMap();
			if (values != null) {
				List<Map<String, String>> enumValues = new ArrayList<>(values.size());
				for (Entry<String, String> entry : values.entrySet()) {
					Map<String, String> enumValue = new TreeMap<>();
					enumValue.put("value", entry.getKey());
					enumValue.put("label", entry.getValue());
					enumValues.add(enumValue);
				}
				result.put("enumValues", enumValues);
			}
			
			return result;
		}
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this).omitNullValues().add("owningModuleName", owningModuleName)
				.add("owningDocumentName", owningDocumentName).add("queryName", queryName).add("modelName", modelName)
				.add("contextId", contextId).add("containerId", containerId).toString();
	}
}

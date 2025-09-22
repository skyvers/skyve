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
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
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
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import jakarta.faces.component.UIOutput;
import jakarta.faces.context.FacesContext;

/**
 * This component isn't in the skyve.taglib.xml and has no @Component annotation and is not meant to be used on an XHTML page. 
 * The resources skyvevue/index.js and skyvevue/index.css are brought in view view.xhtml (if enabled)
 */
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
	
	private String selectedRemoteCommand;
	
	/**
	 * This exists so that Faces can instantiate the view tree and reinstate the attributes.
	 */
	public VueListGridScript() {
		// nothing to see here
	}
	
	/**
	 * This is called to set state.
	 * 
	 * @param containerId
	 * @param moduleName
	 * @param documentName
	 * @param queryName
	 * @param modelName
	 * @param contextId
	 * @param showAdd
	 * @param showZoom
	 * @param showFilter
	 * @param showSummary
	 * @param showSnap
	 * @param selectedRemoteCommand
	 */
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
	}

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		Map<String, Object> attributes = getAttributes();
		if (Boolean.TRUE.toString().equals(attributes.get("dynamic")) || (getValue() == null)) {
			grabAttributes(attributes);
			createScriptOutput();
		}

		super.encodeBegin(context);
	}

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
		
		this.selectedRemoteCommand = (String) attributes.get("selectedRemoteCommand");
	}

	private void createScriptOutput() {
		final User user = CORE.getUser();
		final Customer customer = user.getCustomer();
		Module owningModule = customer.getModule(owningModuleName);
		Document owningDocument = owningModule.getDocument(customer, owningDocumentName);

		Map<String, Object> params = new TreeMap<>();
		params.put("containerId", containerId);
		params.put("owningModule", owningModuleName);
		if (queryName != null) {
			params.put("query", queryName);
		}
		params.put("owningDocument", owningDocumentName);
		if (modelName != null) {
			params.put("model", modelName);
		}
		if (contextId != null) {
			params.put("contextId", contextId);
		}
		params.put("showAdd", showAdd);
		params.put("showZoom", showZoom);
		params.put("showFilter", showFilter);
		params.put("showSummary", showSummary);
		params.put("showSnap", showSnap);

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
																									queryName);
			String binding = columnDefn.getBinding();
			TargetMetaData tmd = Binder.getMetaDataForBinding(customer, drivingModule, drivingDocument, binding);

			columns.add(new ColumnMetaData(columnDefn, scColDefn, tmd, customer).toMap());
		}
		params.put("columns", columns);

		StringJoiner sj = new StringJoiner(" \n");
		sj.add("<script>")
			.add("  setTimeout(() => {")
			.add("    SKYVE.listgrid(")
			.add(JSON.marshall(params))
			.add("    );")
			.add("  }, 0);")
			.add("</script>");

		setValue(sj.toString());
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

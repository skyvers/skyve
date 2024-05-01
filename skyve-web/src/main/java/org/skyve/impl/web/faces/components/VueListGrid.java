package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.StringJoiner;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import jakarta.faces.application.ResourceDependencies;
import jakarta.faces.application.ResourceDependency;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("unused")
@FacesComponent(VueListGrid.COMPONENT_TYPE)
@ResourceDependencies({ @ResourceDependency(library = "skyvevue", name = "index.js"),
        @ResourceDependency(library = "skyvevue", name = "index.css"),
        @ResourceDependency(library = "skyvevue", name = "Inter-italic.var.woff2"),
        @ResourceDependency(library = "skyvevue", name = "Inter-roman.var.woff2") })
public class VueListGrid extends UIOutput {

    @SuppressWarnings("hiding")
    public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.VueListGrid";

    @Override
    public void encodeBegin(FacesContext context) throws IOException {
        Map<String, Object> attributes = getAttributes();

        // NB:- I might need to let this be evaluated always since this just isnt
        // structural but may have expressions that need re-evaluating
        // TODO Need to try this out.
        if ((getValue() == null) || Boolean.TRUE.toString()
                                                .equals(attributes.get("dynamic"))) {

            String out;
            try {
                out = generateHtml(attributes);
                setValue(out);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private String generateHtml(Map<String, Object> attributes) throws Exception {
        final String moduleName = (String) attributes.get("module");
        final String documentName = (String) attributes.get("document");
        final String queryName = (String) attributes.get("query");

        ListGridParams params = new ListGridParams();
        params.setTargetSelector("#grid");
        params.setModule(moduleName);
        params.setQuery(queryName);

        // Column definitions
        Customer customer = CORE.getUser()
                                .getCustomer();
        Module module = customer.getModule(moduleName);
        Document document = module.getDocument(customer, documentName);

        MetaDataQueryDefinition queryDefn = module.getMetaDataQuery(queryName);
        if (queryDefn == null) {
            queryDefn = module.getDocumentDefaultQuery(customer, documentName);
        }

        for (MetaDataQueryColumn mdQueryColumn : queryDefn.getColumns()) {

            SmartClientQueryColumnDefinition scColDefn = SmartClientViewRenderer.getQueryColumn(CORE.getUser(),
                    customer, module, document, mdQueryColumn, true, queryName);

            ColumnDefinition colDefn = ColumnDefinition.fromSCColumnDefinition(scColDefn);

            params.getColumns()
                  .add(colDefn);
        }

        ObjectMapper mapper = new ObjectMapper();
        String paramsString = mapper.writeValueAsString(params);

        StringJoiner sj = new StringJoiner(" \n");
        sj.add("<div id=\"grid\"></div>")
          .add("<script>")
          .add("window.addEventListener('load', () => {")
          .add("                    SKYVE.listgrid(")
          .add(paramsString)
          .add(");")
          .add("});")
          .add(" </script>");

        return sj.toString();
    }

    private static class ListGridParams {
        private String targetSelector;
        private String module;
        private String query;
        private List<ColumnDefinition> columns = new ArrayList<>();

        public String getTargetSelector() {
            return targetSelector;
        }

        public void setTargetSelector(String targetSelector) {
            this.targetSelector = targetSelector;
        }

        public String getModule() {
            return module;
        }

        public void setModule(String module) {
            this.module = module;
        }

        public String getQuery() {
            return query;
        }

        public void setQuery(String query) {
            this.query = query;
        }

        public List<ColumnDefinition> getColumns() {
            return columns;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("targetSelector", targetSelector)
                              .add("module", module)
                              .add("query", query)
                              .add("columns", columns)
                              .toString();
        }
    }

    private static class ColumnDefinition {
        private String field;
        private String header;
        private boolean sortable = true;
        private boolean filterable = true;
        private List<EnumValue> enumValues = new ArrayList<>(0);
        private String type;
        
		private static final Map<String, String> typeConversions = new ImmutableMap.Builder<String, String>()
				.put("bizDecimal2", "numeric")
				.put("bizDecimal5", "numeric")
				.put("bizDecimal10", "numeric")
				.put("integer", "numeric")
				.put("HH24_MI", "time")
				.put("DD_MMM_YYYY_HH24_MI_SS", "timestamp")
				.put("DD_MMM_YYYY", "date")
				.put("DD_MMM_YYYY_HH24_MI", "dateTime")
				.put("enum", "enum")
				.put("boolean", "boolean")
				.put("richText", "text")
				.put("text", "text")
				.build();  

		public static ColumnDefinition fromSCColumnDefinition(SmartClientQueryColumnDefinition scColDefn) {
			ColumnDefinition cd = new ColumnDefinition();

			cd.field = scColDefn.getName();
			cd.header = scColDefn.getTitle();
			cd.type = flattenType(scColDefn.getType());
			cd.sortable = scColDefn.isCanSortClientOnly();
			cd.filterable = scColDefn.isCanFilter();

            Optional.ofNullable(scColDefn.getValueMap())
                    .ifPresent(map -> map.entrySet()
                                         .stream()
                                         .map(EnumValue::fromEnumeratedValue)
                                         .forEach(cd.getEnumValues()::add));

            return cd;
        }

		private static String flattenType(String inType) {
			String resultType = typeConversions.get(inType);
			if (resultType != null) {
				return resultType;
			}

			throw new DomainException("Unable to convert column type: " + inType);
		}

        public String getField() {
            return field;
        }

        public String getHeader() {
            return header;
        }

        public boolean isSortable() {
            return sortable;
        }

        public boolean isFilterable() {
            return filterable;
        }

        public String getType() {
            return type;
        }

        public List<EnumValue> getEnumValues() {
            return enumValues;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("field", field)
                              .add("header", header)
                              .add("sortable", sortable)
                              .add("filterable", filterable)
                              .add("type", type)
                              .add("enumValues", enumValues)
                              .toString();
        }
    }

    private static class EnumValue {
        private String value;
        private String label;

        public static EnumValue fromEnumeratedValue(Map.Entry<String, String> input) {
            EnumValue result = new EnumValue();

            result.value = input.getKey();
            result.label = input.getValue();

            return result;
        }

        public String getValue() {
            return value;
        }

        public String getLabel() {
            return label;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("value", value)
                              .add("label", label)
                              .toString();
        }
    }
}

package org.skyve.impl.web.faces.components;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.StringUtils.isEmpty;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.stream.Stream;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.component.remotecommand.RemoteCommand;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import jakarta.faces.application.ResourceDependency;
import jakarta.faces.component.FacesComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("unused")
@FacesComponent(VueListGrid.COMPONENT_TYPE)
@ResourceDependency(library = "skyvevue", name = "index.js")
@ResourceDependency(library = "skyvevue", name = "index.css")
public class VueListGrid extends HtmlPanelGroup {

    private static Logger logger = LogManager.getLogger(VueListGrid.class);

    @SuppressWarnings("hiding")
    public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.VueListGrid";

    private String containerId;
    private String ourUniqueId;

    private String moduleName;

    private String documentName;

    private String queryName;

    private String modelName;

    private String contextId;

    private Actions actions;

    private ListGrid grid;

    @Override
    public void encodeBegin(FacesContext context) throws IOException {

        getChildren().clear();
        grabAttributes();

        logger.debug("Creating VueListGrid: {}", this);

        HtmlPanelGroup containerGroup = createContainerDiv();

        UIOutput uiout = createScriptOutput();
        containerGroup.getChildren()
                      .add(uiout);

        super.encodeBegin(context);
        logger.debug("Created VueListGrid: {}", this);
    }

    private void grabAttributes() {
        Map<String, Object> attributes = getAttributes();
        this.moduleName = (String) attributes.get("module");
        this.documentName = (String) attributes.get("document");
        this.queryName = (String) attributes.get("query");
        this.modelName = (String) attributes.get("model");
        this.contextId = (String) attributes.get("contextId");

        actions = (Actions) attributes.get(Actions.KEY);
        if (actions == null) {
            actions = new Actions();
        }
    }

    /**
     * Create a container div to hold the div we'll render the Vue grid into. We'll also set
     * aside it's id to hand to the Javascript later.
     * 
     * @return
     */
    private HtmlPanelGroup createContainerDiv() {

        ourUniqueId = RandomStringUtils.randomAlphabetic(10);

        HtmlPanelGroup containerGroup = new HtmlPanelGroup();
        containerGroup.setLayout("block");
        containerGroup.setId(ourUniqueId);
        getChildren().add(containerGroup);

        this.containerId = containerGroup.getClientId();

        HtmlPanelGroup targetGroup = new HtmlPanelGroup();
        targetGroup.setLayout("block");
        targetGroup.setStyleClass("vue-list-grid-container");
        containerGroup.getChildren()
                      .add(targetGroup);

        namespaceAndOutputCommands(containerGroup);

        return containerGroup;
    }

    /**
     * Rename all the RemoteCommands we're about to output with unique names, this
     * needs to happen before creating the ListGridParams that will be serialised
     * to JSON.
     * 
     * @param containerGroup
     */
    private void namespaceAndOutputCommands(HtmlPanelGroup containerGroup) {

        List<RemoteCommand> allCommands = Stream.of(actions.getSelectedCommand(), actions.getEditedCommand(),
                actions.getDeletedCommand())
                                                .filter(Objects::nonNull)
                                                .collect(toList());

        for (RemoteCommand rc : allCommands) {

            String name = rc.getName();
            rc.setName(ourUniqueId + "_" + name);

            logger.trace("Adding RemoteCommand: {}", rc.getName());
            containerGroup.getChildren()
                          .add(rc);
        }
    }

    private UIOutput createScriptOutput() throws JsonProcessingException {

        Customer customer = CORE.getUser()
                                .getCustomer();
        Module module = customer.getModule(moduleName);
        Document document = module.getDocument(customer, documentName);

        ListGridParams params = new ListGridParams();
        params.setTargetSelector("[id='" + containerId + "'] .vue-list-grid-container");
        params.setModule(moduleName);
        params.setDocument(documentName);
        params.setQuery(queryName);
        params.setModel(modelName);
        params.setContextId(contextId);
        params.actions = ClientActions.fromActions(actions);

        final List<MetaDataQueryColumn> columns;
        if (isEmpty(modelName)) {
            columns = findQueryColumns(customer, module);
        } else {
            ListModel<Bean> listModel = findListModel(customer, document);
            columns = listModel.getColumns();
            document = listModel.getDrivingDocument();
            module = customer.getModule(document.getOwningModuleName());
        }

        for (MetaDataQueryColumn mdQueryColumn : columns) {

            SmartClientQueryColumnDefinition scColDefn = SmartClientViewRenderer.getQueryColumn(
                    CORE.getUser(), customer, module, document, mdQueryColumn, true, queryName);

            String binding = mdQueryColumn.getBinding();
            TargetMetaData tmd = Binder.getMetaDataForBinding(customer, module, document, binding);

            ColumnMetaData md = new ColumnMetaData(mdQueryColumn, scColDefn, tmd, customer);
            ColumnDefinition colDefn = ColumnDefinition.fromColumnMetaData(md);
            logger.trace("Created column definition: {}", colDefn);

            params.getColumns()
                  .add(colDefn);
        }

        ObjectMapper mapper = new ObjectMapper();
        String paramsString = mapper.writeValueAsString(params);

        StringJoiner sj = new StringJoiner(" \n");
        sj.add("<script>")
          .add("  setTimeout(() => {")
          .add("    SKYVE.listgrid(")
          .add(paramsString)
          .add("    );")
          .add("  }, 0);")
          .add("</script>");

        String val = sj.toString();
        UIOutput out = new UIOutput();
        out.setValue(val);
        return out;
    }

    private ListModel<Bean> findListModel(Customer customer, Document document) {

        return document.getListModel(customer, modelName, true);
    }

    private List<MetaDataQueryColumn> findQueryColumns(Customer customer, Module module) {
        MetaDataQueryDefinition queryDefn = module.getMetaDataQuery(queryName);
        if (queryDefn == null) {
            queryDefn = module.getDocumentDefaultQuery(customer, documentName);
            logger.debug("Using document default query definition: {}", queryDefn);
        }

        return queryDefn.getColumns();
    }

    /**
     * Utility class to group together the metadata we need to aggregate
     */
    private static class ColumnMetaData {
        private final MetaDataQueryColumn mdQueryColumn;
        private final SmartClientQueryColumnDefinition scQueryColumnDefn;
        private final TargetMetaData targetMetaData;
        private final Customer customer;

        private static final Map<String, String> attributeTypeConversions = //
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

        private static final Map<Class<?>, String> implicitTypeConversions = //
                new ImmutableMap.Builder<Class<?>, String>().put(String.class, "text")
                                                            .put(Integer.class, "numeric")
                                                            .put(Boolean.class, "boolean")
                                                            .build();

        public ColumnMetaData(
                MetaDataQueryColumn mdQueryColumn,
                SmartClientQueryColumnDefinition scQueryColumnDefn,
                TargetMetaData targetMetaData,
                Customer customer) {
            this.mdQueryColumn = mdQueryColumn;
            this.scQueryColumnDefn = scQueryColumnDefn;
            this.targetMetaData = targetMetaData;
            this.customer = customer;
        }

        public String getBinding() {
            return scQueryColumnDefn.getName();
        }

        public String getTitle() {
            return scQueryColumnDefn.getTitle();
        }

        public String getType() {

            Attribute attribute = targetMetaData.getAttribute();
            if (attribute == null) {
                // Defaulting to text for unhandled implicit type attributes
                Class<?> implicitAttrType = targetMetaData.getType();
                return implicitTypeConversions.getOrDefault(implicitAttrType, "text");
            }

            return flattenType(attribute.getAttributeType()
                                        .name());
        }

        private static String flattenType(String inType) {
            String resultType = attributeTypeConversions.get(inType);
            if (resultType != null) {
                return resultType;
            }

            return inType;
        }

        public String getConverterName() {

            return Optional.ofNullable(targetMetaData.getAttribute())
                           .filter(ConvertableField.class::isInstance)
                           .map(ConvertableField.class::cast)
                           .map(cf -> cf.getConverterForCustomer(customer))
                           .map(ConverterName::valueOf)
                           .map(ConverterName::name)
                           .orElse(null);
        }

        public boolean isSortable() {

            if (mdQueryColumn instanceof MetaDataQueryProjectedColumn mdcpc) {
                return mdcpc.isSortable();
            }

            return false;
        }

        public boolean isFilterable() {
            return scQueryColumnDefn.isCanFilter();
        }

        public Map<String, String> getValueMap() {
            return scQueryColumnDefn.getValueMap();
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .omitNullValues()
                              .add("binding", getBinding())
                              .add("title", getTitle())
                              .add("type", getType())
                              .add("converterName", getConverterName())
                              .add("sortable", isSortable())
                              .add("filterable", isFilterable())
                              .toString();
        }
    }

    /**
     * We'll pull together all the params we'll need to supply to the javascript
     * SKYVE.listgrid function in this class. Will get serialised to JSON and
     * sent to the browser.
     */
    @JsonInclude(Include.NON_EMPTY)
    private static class ListGridParams {
        private String targetSelector;
        private String module;
        private String query;
        private String document;
        private String model;
        private String contextId;
        private List<ColumnDefinition> columns = new ArrayList<>();
        private ClientActions actions = new ClientActions();

        public String getTargetSelector() {
            return targetSelector;
        }

        public void setTargetSelector(String targetSelector) {
            this.targetSelector = targetSelector;
        }

        public String getDocument() {
            return document;
        }

        public void setDocument(String document) {
            this.document = document;
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

        public void setModel(String model) {
            this.model = model;
        }

        public String getModel() {
            return model;
        }

        public String getContextId() {
            return contextId;
        }

        public void setContextId(String contextId) {
            this.contextId = contextId;
        }

        public ClientActions getActions() {
            return actions;
        }

        public void setActions(ClientActions actions) {
            this.actions = actions;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("targetSelector", targetSelector)
                              .add("module", module)
                              .add("query", query)
                              .add("document", document)
                              .add("contextId", contextId)
                              .add("columns", columns)
                              .add("actions", actions)
                              .toString();
        }
    }

    @JsonInclude
    private static class ClientActions {
        private String selected;
        private String edited;
        private String deleted;

        public static ClientActions fromActions(Actions actions) {

            ClientActions result = new ClientActions();

            result.selected = actions.selectedCommand == null ? null : actions.selectedCommand.getName();
            result.edited = actions.editedCommand == null ? null : actions.editedCommand.getName();
            result.deleted = actions.deletedCommand == null ? null : actions.deletedCommand.getName();

            return result;
        }

        public String getSelected() {
            return selected;
        }

        public void setSelected(String selected) {
            this.selected = selected;
        }

        public String getEdited() {
            return edited;
        }

        public void setEdited(String edited) {
            this.edited = edited;
        }

        public String getDeleted() {
            return deleted;
        }

        public void setDeleted(String deleted) {
            this.deleted = deleted;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("selected", selected)
                              .add("edited", edited)
                              .add("deleted", deleted)
                              .toString();
        }
    }

    @JsonInclude(Include.NON_EMPTY)
    private static class ColumnDefinition {
        private String field;
        private String header;
        private boolean sortable = true;
        private boolean filterable = true;
        private List<EnumValue> enumValues = new ArrayList<>(0);
        private String type;
        private String converter;

        public static ColumnDefinition fromColumnMetaData(ColumnMetaData metadata) {
            ColumnDefinition cd = new ColumnDefinition();

            cd.field = metadata.getBinding();
            cd.header = metadata.getTitle();
            cd.type = metadata.getType();
            cd.sortable = metadata.isSortable();
            cd.filterable = metadata.isFilterable();
            cd.converter = metadata.getConverterName();

            Optional.ofNullable(metadata.getValueMap())
                    .ifPresent(map -> map.entrySet()
                                         .stream()
                                         .map(EnumValue::fromEnumeratedValue)
                                         .forEach(cd.getEnumValues()::add));

            return cd;
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

        public String getConverter() {
            return converter;
        }

        public List<EnumValue> getEnumValues() {
            return enumValues;
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .omitNullValues()
                              .add("field", field)
                              .add("header", header)
                              .add("sortable", sortable)
                              .add("filterable", filterable)
                              .add("type", type)
                              .add("converter", converter)
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

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                          .omitNullValues()
                          .add("moduleName", moduleName)
                          .add("documentName", documentName)
                          .add("queryName", queryName)
                          .add("modelName", modelName)
                          .add("contextId", contextId)
                          .add("containerId", containerId)
                          .toString();
    }

    public ListGrid getGrid() {
        return grid;
    }

    public void setGrid(ListGrid grid) {
        this.grid = grid;
    }

    public static class Actions {
        public static final String KEY = "actions";

        private RemoteCommand selectedCommand;
        private RemoteCommand editedCommand;
        private RemoteCommand deletedCommand;

        public RemoteCommand getSelectedCommand() {
            return selectedCommand;
        }

        public void setSelectedCommand(RemoteCommand selectedCommand) {
            this.selectedCommand = selectedCommand;
        }

        public RemoteCommand getEditedCommand() {
            return editedCommand;
        }

        public void setEditedCommand(RemoteCommand editedCommand) {
            this.editedCommand = editedCommand;
        }

        public RemoteCommand getDeletedCommand() {
            return deletedCommand;
        }

        public void setDeletedCommand(RemoteCommand deletedCommand) {
            this.deletedCommand = deletedCommand;
        }
    }
}

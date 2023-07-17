package org.skyve.impl.generate.client.flutter;

import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.model.list.ListModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class FlutterListView extends FlutterView {

    protected Logger log = LoggerFactory.getLogger(getClass());

    private Document document;
    private ListModel<Bean> model;
    private MetaDataQueryDefinition query;

    public FlutterListView(FlutterGenerator generator, String moduleName, String viewName) {
        super(generator, moduleName, viewName);
    }

    void setModel(Module module, Document document, String name, ListModel<Bean> model) {
        this.document = document;
        this.model = model;
    }

    void setQuery(Module module, Document document, MetaDataQueryDefinition query) {
        this.document = document;
        this.query = query;
    }

    @Override
    protected void create(FileWriter fw) throws IOException {

        String documentName = document.getName();
        Map<String, String> subs = new TreeMap<>();
        subs.put("##PROJECT##", getConfig().getProjectName());
        subs.put("##MODULE##", moduleName);
        subs.put("##VIEW##", viewName);
        subs.put("##QUERY##", query.getName());
        subs.put("##EDIT_DART##", BindUtil.toJavaStaticIdentifier(documentName)
                                          .toLowerCase());
        subs.put("##EDIT_CLASS##", BindUtil.toJavaTypeIdentifier(moduleName + documentName));
        subs.put("##CLASS##", className);

        addQuerySubstitutes(subs);

        log.debug("Creating list view '{}' with query: {}", viewName, query.getName());
        fw.write(substitute("templates/list.dart", subs));
    }

    private void addQuerySubstitutes(Map<String, String> subs) {
        String description = "";
        String column1 = null;
        String column2 = null;

        List<MetaDataQueryColumn> columns = null;
        if (query != null) {
            columns = query.getColumns();
            description = query.getLocalisedDescription();
        } else if (model != null) {
            columns = model.getColumns();
            description = model.getLocalisedDescription();
        }

        if (columns != null) {
            for (MetaDataQueryColumn column : columns) {
                if ((column1 != null) && (column2 != null)) {
                    break;
                }
                // don't show hidden columns
                if (column.isHidden()) {
                    continue;
                }
                // don't show unprojected columns
                if ((column instanceof MetaDataQueryProjectedColumn)
                        && (!((MetaDataQueryProjectedColumn) column).isProjected())) {
                    continue;
                }

                String columnName = column.getBinding();
                if (columnName == null) {
                    columnName = column.getName();
                }
                if (column1 == null) {
                    column1 = columnName;
                } else {
                    column2 = columnName;
                }
            }
        }

        subs.put("##DESCRIPTION##", description);
        if (column1 == null) {
            column1 = Bean.DOCUMENT_ID;
        }
        subs.put("##COLUMN1##", column1);
        if (column2 == null) {
            column2 = Bean.DOCUMENT_ID;
        }
        subs.put("##COLUMN2##", column2);
    }

}

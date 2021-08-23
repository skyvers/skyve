package org.skyve.impl.generate.pwa.react;

import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;


class ReactListView extends ReactComponent {
	private Module module;
	private Document document;
	private String modelName;
	private ListModel<Bean> model;
	private MetaDataQueryDefinition query;
	
	public ReactListView(ReactGenerator generator, String moduleName, String componentName) {
		super(generator, moduleName, componentName);
	}

	void setModel(Module module, Document document, String name, ListModel<Bean> model) {
		this.module = module;
		this.document = document;
		this.modelName = name;
		this.model = model;
	}

	void setQuery(Module module, Document document, MetaDataQueryDefinition query) {
		this.module = module;
		this.document = document;
		this.query = query;
	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		fw.write("import React from 'react';\n");
		fw.write("import {View} from '../../View';\n");
		fw.write("import {DataTable} from 'primereact/datatable';\n");
		fw.write("import {Column} from 'primereact/column';\n\n");

		fw.append("export class ").append(moduleName).append(componentName).append(" extends View {\n");
		fw.write("\tconstructor() {\n");
		fw.write("\t\tsuper();\n");
		fw.write("\t\tthis.state = {};\n");
		fw.write("\t}\n\n");

		renderComponentDidMount(fw);
		
		fw.write("\trender() {\n");
		fw.write("\t\treturn (\n");
		renderDataTable(fw);
		fw.write("\t\t);\n");
		fw.write("\t}\n");

		fw.write("}\n");
	}

	private void renderComponentDidMount(FileWriter fw) throws IOException {
		fw.write("\tcomponentDidMount() {\n");
		if (model != null) {
			fw.append("\t\tthis.fetchModel('").append(model.getDrivingDocument().getOwningModuleName()).append("', '");
			fw.append(modelName).append("', 0, 75).then(data => this.setState({data: data}));\n");
		}
		else if (query != null) {
			fw.append("\t\tthis.fetchQuery('").append(query.getOwningModule().getName()).append("', '");
			fw.append(query.getName()).append("', 0, 75).then(data => this.setState({data: data}));\n");
		}
		fw.write("\t}\n\n");
	}
	
	private void renderDataTable(FileWriter fw) throws IOException {
		fw.write("\t\t\t<DataTable value={this.state.data}\n");
		fw.write("\t\t\t\t\t\tselectionMode=\"single\"\n");
		fw.write("\t\t\t\t\t\tonSelectionChange={e => this.props.history.push('/' + e.data.bizModule + '/' + e.data.bizDocument + '/' + e.data.bizId)}>\n");

		List<MetaDataQueryColumn> columns = (query != null) ? query.getColumns() : ((model == null) ? null : model.getColumns());
		if (columns != null) {
			User u = CORE.getUser();
			Customer c = u.getCustomer();
			for (MetaDataQueryColumn column : columns) {
				// don't show hidden columns
				if (column.isHidden()) {
					continue;
				}
				// don't show unprojected columns
				if ((column instanceof MetaDataQueryProjectedColumn) && 
						(! ((MetaDataQueryProjectedColumn) column).isProjected())) {
					continue;
				}
// TODO Fix				SmartClientQueryColumnDefinition def = SmartClientGenerateUtils.getQueryColumn(u, c, module, document, column, false);
				String columnName = column.getBinding();
				if (columnName == null) {
					columnName = column.getName();
				}
				fw.append("\t\t\t\t<Column field=\"").append(columnName).append("\" header=\"").append(column.getDisplayName()).append("\" />\n");
// TODO Fix	            fw.append("\t\t\t\t<Column field=\"").append(def.getName()).append("\" header=\"").append(def.getTitle()).append("\" />\n");
			}
		}
		fw.write("\t\t\t</DataTable>\n");
	}
}

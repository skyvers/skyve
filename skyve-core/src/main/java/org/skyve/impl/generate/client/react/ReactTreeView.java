package org.skyve.impl.generate.client.react;

import java.io.FileWriter;
import java.io.IOException;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;


public class ReactTreeView extends ReactComponent {
	@SuppressWarnings("unused")
	private ListModel<Bean> model;
	@SuppressWarnings("unused")
	private MetaDataQueryDefinition query;
	
	public ReactTreeView(ReactGenerator generator, String moduleName, String componentName) {
		super(generator, moduleName, componentName);
	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		fw.write("import React from 'react';\n");
		fw.write("import {View} from '../../View';\n");
		fw.write("import {DataTable} from 'primereact/datatable';\n");
		fw.write("import {Column} from 'primereact/column'\n\n");

		fw.write("export class ");
		fw.write(moduleName);
		fw.write(componentName);
		fw.write(" extends View {\n");
		
		fw.write("\tconstructor() {\n");
		fw.write("\t\tsuper();\n");
		fw.write("\t\tthis.state = {};\n");
		fw.write("\t}\n");
		
		fw.write("\trender() {\n");
		fw.write("\t\treturn (\n");
		fw.write("\t\t\t<h1>Tree</h1>\n");
		fw.write("\t\t);\n");
		fw.write("\t}\n");

		fw.write("}\n");
	}
}

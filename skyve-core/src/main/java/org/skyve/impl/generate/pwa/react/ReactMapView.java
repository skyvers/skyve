package org.skyve.impl.generate.pwa.react;

import java.io.FileWriter;
import java.io.IOException;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;


public class ReactMapView extends ReactComponent {
	private ListModel<Bean> model;
	private MetaDataQueryDefinition query;
	
	public ReactMapView(ReactGenerator generator, String moduleName, String componentName) {
		super(generator, moduleName, componentName);
	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		fw.write("import React from 'react';\n");
		fw.write("import {View} from '../../View';\n");
		fw.write("import {DataTable} from 'primereact/datatable';\n");
		fw.write("import {Column} from 'primereact/column'\n\n");

		fw.append("export class ").append(moduleName).append(componentName).append(" extends View {\n");
		fw.write("\tconstructor() {\n");
		fw.write("\t\tsuper();\n");
		fw.write("\t\tthis.state = {};\n");
		fw.write("\t}\n");
		
		fw.write("\trender() {\n");
		fw.write("\t\treturn (\n");
		fw.write("\t\t\t<h1>Map</h1>\n");
		fw.write("\t\t);\n");
		fw.write("\t}\n");

		fw.write("}\n");
	}
}

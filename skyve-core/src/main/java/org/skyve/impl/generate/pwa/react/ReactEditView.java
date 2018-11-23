package org.skyve.impl.generate.pwa.react;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

public class ReactEditView extends ReactComponent {
	private Module module;
	private Document document;
	private View createView;
	private View editView;
	
	public ReactEditView(ReactGenerator generator, String moduleName, String componentName) {
		super(generator, moduleName, componentName);
	}

	public void setViews(Module module, Document document) {
		this.module = module;
		this.document = document;
        editView = generator.repository.getView(generator.uxui,
        											generator.customer,
        											document,
        											ViewType.edit.toString());
        createView = generator.repository.getView(generator.uxui,
        											generator.customer,
        											document,
        											ViewType.create.toString());
 	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		Map<String, String> imports = new TreeMap<>();
		StringBuilder editJsx = new StringBuilder(2096);
		StringBuilder createJsx = new StringBuilder(2096);

		// Wrap edit and create views in a Fragment if necessary
		boolean bothViews = (editView != null) && (createView != null);

		if (editView != null) {
			new PrimeReactViewVisitor(generator.customer, module, document, editView, imports, editJsx, bothViews).visit();
		}
		if (createView != null) {
			new PrimeReactViewVisitor(generator.customer, module, document, createView, imports, createJsx, bothViews).visit();
		}

		fw.write("import React from 'react';\n");
		fw.write("import {View} from '../../View';\n");
		
		for (String key : imports.keySet()) {
			fw.append("import ").append(key).append(" from '").append(imports.get(key)).append("';\n");
		}
		fw.write("\n");
		
		fw.append("export class ").append(moduleName).append(componentName).append(" extends View {\n");
		fw.write("\tconstructor() {\n");
		fw.write("\t\tsuper();\n");
		fw.write("\t\tthis.state = {};\n");
		fw.write("\t}\n\n");
		
		fw.write("\tcomponentDidMount() {\n");
		fw.append("\t\tthis.edit('").append(moduleName).append("', '").append(document.getName()).append("', this.props.match.params.bizId)\n");
		fw.write("\t\t\t.then(data => this.setState(data[0]));\n");
	    fw.write("\t}\n\n");

		fw.write("\trender() {\n");

		if (bothViews) {
			fw.write("\t\tif (this.state.created) {\n");
			fw.write("\t\t\treturn (\n");
			fw.write(editJsx.toString());
			fw.write("\t\t\t);\n");
			fw.write("\t\t}\n");
			fw.write("\t\telse {\n");
			fw.write("\t\t\treturn (\n");
			fw.write(createJsx.toString());
			fw.write("\t\t\t);\n");
			fw.write("\t\t}\n");
		}
		else {
			fw.write("\t\treturn (\n");
			if (editView != null) {
				fw.write(editJsx.toString());
			}
			if (createView != null) {
				fw.write(createJsx.toString());
			}
			fw.write("\t\t);\n");
		}

		fw.write("\t}\n");
		fw.write("}\n");
	}
}

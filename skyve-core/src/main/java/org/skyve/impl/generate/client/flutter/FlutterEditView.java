package org.skyve.impl.generate.client.flutter;

import static org.skyve.impl.generate.client.flutter.FlutterGenerator.INDENT;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

public class FlutterEditView extends FlutterView {
	private Module module;
	private Document document;
	private View createView;
	private View editView;
	
	public FlutterEditView(FlutterGenerator generator, String moduleName, String viewName) {
		super(generator, moduleName, viewName);
	}

	public void setViews(Module module, Document document) {
		this.module = module;
		this.document = document;
		Customer c = CORE.getCustomer();
        editView = document.getView(generator.uxui, c, ViewType.edit.toString());
        createView = document.getView(generator.uxui, c, ViewType.create.toString());
 	}

	@Override
	protected void create(FileWriter fw) throws IOException {
		Set<String> imports = new TreeSet<>();
		String editDart = null;
		String createDart = null;

		// Wrap edit and create views in a Fragment if necessary
		boolean bothViews = (editView != null) && (createView != null);

		User u = CORE.getUser();
		if (editView != null) {
			FlutterViewRenderer v = new FlutterViewRenderer(u, module, document, editView, generator.uxui, imports);
			v.visit();
			editDart = v.getResult().toString();
		}
		if (createView != null) {
			FlutterViewRenderer v = new FlutterViewRenderer(u, module, document, createView, generator.uxui, imports);
			v.visit();
			createDart = v.getResult().toString();
		}

		Map<String, String> subs = new TreeMap<>();
		subs.put("##PROJECT##", generator.projectName);
		subs.put("##MODULE##", moduleName);
		subs.put("##DOCUMENT##", document.getName());
		subs.put("##CLASS##", className);
		
		StringBuilder sb = new StringBuilder(256);
		for (String key : imports) {
			sb.append("import 'package:").append(generator.projectName).append("/").append(key).append(".dart';\n");
		}
		subs.put("##IMPORTS##", sb.toString());
		
//		if (bothViews) {
//		fw.write("\t\tif (this.state.created) {\n");
//		fw.write("\t\t\treturn (\n");
//		fw.write((editJsx == null) ? "" : editJsx.toString());
//		fw.write("\t\t\t);\n");
//		fw.write("\t\t}\n");
//		fw.write("\t\telse {\n");
//		fw.write("\t\t\treturn (\n");
//		fw.write((createJsx == null) ? "" : createJsx.toString());
//		fw.write("\t\t\t);\n");
//		fw.write("\t\t}\n");
//	}
//	else {
		subs.put("##DART##", editDart);
		//		fw.write("\t\treturn (\n");
//		if (editView != null) {
//			fw.write((editJsx == null) ? "" : editJsx.toString());
//		}
//		if (createView != null) {
//			fw.write((createJsx == null) ? "" : createJsx.toString());
//		}
//		fw.write("\t\t);\n");
//	}

		fw.write(substitute("templates/edit.dart", subs));

//		fw.write("import React from 'react';\n");
//		fw.write("import {View} from '../../View';\n");
//		
//		for (String key : imports) {
//			fw.write("import 'package:");
//			fw.write(key);
//			fw.write("';\n");
//		}
//		fw.write("\n");
//		
//		fw.write("export class ");
//		fw.write(moduleName);
//		fw.write(viewName);
//		fw.write(" extends View {\n");
//		
//		fw.write("\tconstructor() {\n");
//		fw.write("\t\tsuper();\n");
//		fw.write("\t\tthis.state = {};\n");
//		fw.write("\t}\n\n");
//		
//		fw.write("\tcomponentDidMount() {\n");
//		
//		fw.write("\t\tthis.edit('");
//		fw.write(moduleName);
//		fw.write("', '");
//		fw.write(document.getName());
//		fw.write("', this.props.match.params.bizId)\n");
//		
//		fw.write("\t\t\t.then(data => this.setState(data[0]));\n");
//	    fw.write("\t}\n\n");
//
//		fw.write("\trender() {\n");
//
//		if (bothViews) {
//			fw.write("\t\tif (this.state.created) {\n");
//			fw.write("\t\t\treturn (\n");
//			fw.write((editJsx == null) ? "" : editJsx.toString());
//			fw.write("\t\t\t);\n");
//			fw.write("\t\t}\n");
//			fw.write("\t\telse {\n");
//			fw.write("\t\t\treturn (\n");
//			fw.write((createJsx == null) ? "" : createJsx.toString());
//			fw.write("\t\t\t);\n");
//			fw.write("\t\t}\n");
//		}
//		else {
//			fw.write("\t\treturn (\n");
//			if (editView != null) {
//				fw.write((editJsx == null) ? "" : editJsx.toString());
//			}
//			if (createView != null) {
//				fw.write((createJsx == null) ? "" : createJsx.toString());
//			}
//			fw.write("\t\t);\n");
//		}
//
//		fw.write("\t}\n");
//		fw.write("}\n");
	}
}

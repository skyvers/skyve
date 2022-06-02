package org.skyve.impl.generate.pwa.react;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class ReactNativeEditView extends ReactComponent {
	private Module module;
	private Document document;
	private View createView;
	private View editView;

	public ReactNativeEditView(ReactGenerator generator, String moduleName, String componentName) {
		super(generator, moduleName, componentName);
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
		Map<String, String> imports = new TreeMap<>();
		String editJsx = null;
		String createJsx = null;

		// Wrap edit and create views in a Fragment if necessary
		boolean bothViews = (editView != null) && (createView != null);

		User u = CORE.getUser();
		if (editView != null) {
			ReactViewRenderer v = new ReactNativeViewRenderer(u, module, document, editView, imports, bothViews);
			v.visit();
			editJsx = v.getResult().toString();
		}
		if (createView != null) {
			ReactViewRenderer v = new ReactNativeViewRenderer(u, module, document, createView, imports, bothViews);
			v.visit();
			createJsx = v.getResult().toString();
		}

		fw.write("import React from 'react';\n");
		fw.write("import { useNavigation } from '@react-navigation/native';\n");

		for (String key : imports.keySet()) {
			fw.write("import ");
			fw.write(key);
			fw.write(" from '");
			fw.write(imports.get(key));
			fw.write("';\n");
		}
		fw.write("\n");

		fw.write("export default function ");
		fw.write(moduleName);
		fw.write(componentName);
		fw.write("(props) {\n");

		if (bothViews) {
			fw.write("\t\tif (this.state.created) {\n");
			addNavigationOptions(fw, editView, "\t\t\t");
			fw.write("\t\t\treturn (\n");
			fw.write((editJsx == null) ? "" : editJsx.toString());
			fw.write("\t\t\t);\n");
			fw.write("\t\t}\n");
			fw.write("\t\telse {\n");
			addNavigationOptions(fw, createView, "\t\t\t");
			fw.write("\t\t\treturn (\n");
			fw.write((createJsx == null) ? "" : createJsx.toString());
			fw.write("\t\t\t);\n");
			fw.write("\t\t}\n");
		}
		else {
			if (editView != null) {
				addNavigationOptions(fw, editView, "\t");
			}
			if (createView != null) {
				addNavigationOptions(fw, createView, "\t");
			}
			fw.write("\treturn (\n");
			if (editView != null) {
				fw.write((editJsx == null) ? "" : editJsx.toString());
			}
			if (createView != null) {
				fw.write((createJsx == null) ? "" : createJsx.toString());
			}
			fw.write("\t);\n");
		}

		fw.write("}\n");
	}

	private static void addNavigationOptions(FileWriter fw, View view, String startingIndent) throws IOException {
		fw.write(startingIndent);
		fw.write("const navigation = useNavigation();\n");
		fw.write(startingIndent);
		fw.write("navigation.setOptions({\n");
		fw.write(startingIndent);
		fw.write("\ttitle: '");
		fw.write(view.getLocalisedTitle());
		fw.write("',\n");
		fw.write(startingIndent);
		fw.write("});\n\n");
	}
}

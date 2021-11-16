package org.skyve.impl.generate.pwa.react;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.MenuRenderer;

public class ReactRouter {
	private static String[] EDIT_VIEW_PARAMS = new String[] {"bizId"};
	
	private ReactGenerator generator;
	private Set<String> imports = new TreeSet<>();
	private Set<String> routes = new TreeSet<>();
	
	public ReactRouter(ReactGenerator generator) {
		this.generator = generator;
	}
	
	void create() throws IOException {
		File router = new File(generator.srcSkyvePath, "Router.js");
		if (router.exists()) {
			router.delete();
		}

		try (FileWriter fw = new FileWriter(router)) {
			fw.write("import React, {Component, Fragment} from 'react';\n");
			fw.write("import axios from 'axios';\n");
			fw.write("import {Route} from 'react-router-dom';\n");
			fw.write("import {View} from './View';\n\n");

			menuImportsAndRoutes();
			viewImportsAndRoutes();
			
			for (String imprt : imports) {
				fw.write(imprt);
			}

			fw.write("\nexport class Router extends Component {\n");
			fw.write("\tstatic createAppMenu(callback) {\n");
			fw.write("\t\taxios.get('/skyve/primeinit', {params: {mod: 'admin'}})\n");
			fw.write("\t\t\t\t.then(res => callback(eval(res.data)))\n");
			fw.write("\t\t\t\t.catch(error => View.processError(error));\n");
			fw.write("\t}\n\n");
			
			fw.write("\trender() {\n");
			fw.write("\t\treturn (\n");
			fw.write("\t\t\t<Fragment>\n");
			
			for (String route : routes) {
				fw.write(route);
			}

			fw.write("\t\t\t</Fragment>\n");
			fw.write("\t\t);\n");
			fw.write("\t}\n");
			fw.write("}\n");
			fw.flush();
		}
	}
	
	private void viewImportsAndRoutes() {
		Customer c = CORE.getCustomer();
		for (Module m : c.getModules()) {
			String moduleName = m.getName();
			Map<String, DocumentRef> refs = m.getDocumentRefs();
			for (String documentName : refs.keySet()) {
				DocumentRef ref = refs.get(documentName);
				if (ref.getOwningModuleName().equals(moduleName)) {
					Document d = m.getDocument(c, documentName);
					ReactEditView view = new ReactEditView(generator, moduleName, documentName);
					view.setViews(m, d);
					processItem(view, null);
					processItem(view, EDIT_VIEW_PARAMS);
				}
			}
		}
	}
	
	private void menuImportsAndRoutes() {
		Customer c = CORE.getCustomer();
		new MenuRenderer(generator.uxui, null) {
			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName,
											String icon16,
											String iconStyleClass) {
				String moduleName = itemModule.getName();
				String modelName = item.getModelName();
				String componentName = ((modelName == null) ? itemQueryName : modelName) + "Cal";
				processItem(new ReactCalendarView(generator, moduleName, componentName), null);
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String componentName = itemDocument.getName();
				ReactEditView view = new ReactEditView(generator, moduleName, componentName);
				view.setViews(itemModule, itemDocument);
				processItem(view, null);
				processItem(view, EDIT_VIEW_PARAMS);
			}

			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String modelName = item.getModelName();
				String componentName = ((modelName == null) ? itemQueryName : modelName) + "List";
				ReactListView component = new ReactListView(generator, moduleName, componentName);
				if (modelName != null) { // model driven
					component.setModel(itemModule,
										itemDocument,
										modelName,
										itemDocument.getListModel(c, modelName, false));
				}
				else if (item.getQueryName() != null) { // query driven
					component.setQuery(menuModule,
										itemDocument,
										menuModule.getMetaDataQuery(itemQueryName));
				}
				else { // document driven
					component.setQuery(menuModule,
										itemDocument,
										menuModule.getDocumentDefaultQuery(c, itemDocument.getName()));
				}
				processItem(component, null);
			}
			
			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String modelName = item.getModelName();
				String componentName = ((modelName == null) ? itemQueryName : modelName) + "Map";
				processItem(new ReactMapView(generator, moduleName, componentName), null);
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String modelName = item.getModelName();
				String componentName = ((modelName == null) ? itemQueryName : modelName) + "Tree";
				processItem(new ReactTreeView(generator, moduleName, componentName), null);
			}
		}.render(c);
	}
	
	private void processItem(ReactComponent component, String[] params) {
		String moduleName = component.moduleName;
		String componentName = component.componentName;
		
		String format = "import {%s%s} from './views/%s/%s';\n";
		imports.add(String.format(format, moduleName, componentName, moduleName, componentName));

		StringBuilder route = new StringBuilder(128);
		route.append("\t\t\t\t<Route exact path=\"/").append(moduleName).append('/').append(componentName);
		if (params != null) {
			for (String param : params) {
				route.append("/:").append(param);
			}
		}
		route.append("\" component={").append(moduleName).append(componentName).append("} />\n");
		routes.add(route.toString());
		
		generator.components.add(component);
	}
}

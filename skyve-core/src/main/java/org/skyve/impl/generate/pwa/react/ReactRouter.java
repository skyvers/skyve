package org.skyve.impl.generate.pwa.react;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
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
			fw.write("import {Route} from 'react-router-dom';\n\n");
			
			menuImportsAndRoutes();
			viewImportsAndRoutes();
			
			for (String imprt : imports) {
				fw.write(imprt);
			}

			fw.write("\nexport class Router extends Component {\n");
			fw.write("\tstatic createAppMenu(callback) {\n");
			fw.write("\t\taxios.get('/skyve/primeinit', {params: {mod: 'admin'}})\n");
			fw.write("\t\t\t\t.then(res => callback(eval(res.data)));\n");
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
		for (Module m : generator.customer.getModules()) {
			String moduleName = m.getName();
			Map<String, DocumentRef> refs = m.getDocumentRefs();
			for (String documentName : refs.keySet()) {
				DocumentRef ref = refs.get(documentName);
				if (ref.getOwningModuleName().equals(moduleName)) {
					Document d = m.getDocument(generator.customer, documentName);
					ReactEditView view = new ReactEditView(generator, moduleName, documentName);
					view.setViews(m, d);
					processItem(view, EDIT_VIEW_PARAMS);
				}
			}
		}
	}
	
	private void menuImportsAndRoutes() {
		new MenuRenderer(generator.uxui, null, null) {
			@Override
			@SuppressWarnings("synthetic-access")
			public void renderCalendarItem(CalendarItem item,
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
			@SuppressWarnings("synthetic-access")
			public void renderEditItem(EditItem item,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String componentName = itemDocument.getName();
				ReactEditView view = new ReactEditView(generator, moduleName, componentName);
				view.setViews(itemModule, itemDocument);
				processItem(view, EDIT_VIEW_PARAMS);
			}

			@Override
			@SuppressWarnings("synthetic-access")
			public void renderListItem(ListItem item,
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
										generator.repository.getListModel(generator.customer,
																			itemDocument,
																			modelName,
																			false));
				}
				else if (item.getQueryName() != null) { // query driven
					component.setQuery(itemModule,
										itemDocument,
										itemModule.getMetaDataQuery(itemQueryName));
				}
				else { // document driven
					component.setQuery(itemModule,
										itemDocument,
										itemModule.getDocumentDefaultQuery(generator.customer, itemDocument.getName()));
				}
				processItem(component, null);
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void renderMapItem(MapItem item,
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
			@SuppressWarnings("synthetic-access")
			public void renderTreeItem(TreeItem item,
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
		}.render(generator.customer);
	}
	
	private void processItem(ReactComponent component, String[] params) {
		String moduleName = component.moduleName;
		String componentName = component.componentName;
		
		String format = "import {%s%s} from './views/%s/%s';\n";
		imports.add(String.format(format, moduleName, componentName, moduleName, componentName));

		StringBuilder route = new StringBuilder(128);
		route.append("\t\t\t\t<Route path=\"/").append(moduleName).append('/').append(componentName);
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

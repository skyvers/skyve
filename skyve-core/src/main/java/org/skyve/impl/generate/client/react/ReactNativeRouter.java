package org.skyve.impl.generate.client.react;

import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.*;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.MenuRenderer;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class ReactNativeRouter {
	private static String[] EDIT_VIEW_PARAMS = new String[] {"bizId"};

	private ReactGenerator generator;
	private Set<String> imports = new TreeSet<>();
	private Set<String> routes = new TreeSet<>();

	public ReactNativeRouter(ReactGenerator generator) {
		this.generator = generator;
	}

	void create() throws IOException {
		File router = new File(generator.srcSkyvePath, "Router.js");
		if (router.exists()) {
			router.delete();
		}

		try (FileWriter fw = new FileWriter(router)) {
			fw.write("// NOTE: This file is generated and will be overwritten when running the ReactGenerator.\n");
			fw.write("import React from 'react';\n");
			fw.write("import { StyleSheet } from 'react-native';\n");
			fw.write("import { createStackNavigator } from '@react-navigation/stack';\n");
			fw.write("import Icon from 'react-native-vector-icons/FontAwesome';\n");
			fw.write("\n");
			fw.write("import * as c from '../constants';\n");
			fw.write("import Home from '../screens/home/Home';\n");
			fw.write("\n");

			menuImportsAndRoutes();
			viewImportsAndRoutes();

			for (String imprt : imports) {
				fw.write(imprt);
			}

			fw.write("\nexport default function Router() {\n");
			fw.write("\tconst Stack = createStackNavigator();\n");
			fw.write("\n");
			fw.write("\treturn (\n");
			fw.write("\t\t<Stack.Navigator>\n");
			fw.write("\t\t\t<Stack.Screen name=\"Home\" component={Home}\n");
			fw.write("\t\t\t\toptions={({ navigation }) => ({\n");
			fw.write("\t\t\t\t\theaderLeft: () => (\n");
			fw.write("\t\t\t\t\t\t<Icon style={styles.icon} name=\"bars\" size={20} onPress={() => {navigation.openDrawer()}} />\n");
			fw.write("\t\t\t\t\t),\n");
			fw.write("\t\t\t\ttitle: `${c.APP_NAME}`\n");
			fw.write("\t\t\t})}/>\n");

			for (String route : routes) {
				fw.write(route);
			}

			fw.write("\t\t</Stack.Navigator> \n");
			fw.write("\t);\n");
			fw.write("}\n");
			fw.write("\n");

			fw.write("const styles = StyleSheet.create({\n");
			fw.write("\ticon: {\n");
			fw.write("\t\tpadding: 20,\n");
			fw.write("\t},\n");
			fw.write("});");

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
					ReactNativeEditView view = new ReactNativeEditView(generator, moduleName, documentName);
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
				//processItem(new ReactCalendarView(generator, moduleName, componentName), null);
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
				//processItem(view, null);
				//processItem(view, EDIT_VIEW_PARAMS);
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
				//processItem(component, null);
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
				//processItem(new ReactMapView(generator, moduleName, componentName), null);
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
				//processItem(new ReactTreeView(generator, moduleName, componentName), null);
			}
		}.render(c);
	}

	private void processItem(ReactComponent component, String[] params) {
		String moduleName = component.moduleName;
		String componentName = component.componentName;

		// Note we use an alias that includes the module name to avoid issues when components have the same name.
		String format = "import %s%s from './views/%s/%s';\n";
		imports.add(String.format(format, moduleName, componentName, moduleName, componentName));

		StringBuilder route = new StringBuilder(128);
		route.append("\t\t\t<Stack.Screen name=\"").append(moduleName).append(componentName);
		route.append("\" component={").append(moduleName).append(componentName).append("} />\n");
		routes.add(route.toString());

		generator.components.add(component);
	}
}

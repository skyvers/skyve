package org.skyve.impl.generate.client.flutter;

import static org.skyve.impl.generate.client.flutter.FlutterGenerator.INDENT;

import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
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
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.menu.MenuRenderer;

public class FlutterRouting {
	private FlutterGenerator generator;
	private Set<String> imports = new TreeSet<>();
	private Set<String> routes = new TreeSet<>();
	private StringBuilder menu = new StringBuilder(1024);
	private int indentationLevel = 0;
	
	public FlutterRouting(FlutterGenerator generator) {
		this.generator = generator;
		menu.append("const menu = [\n");
	}
	
	void create() throws IOException {
		menuImportsAndRoutes();
		viewImportsAndRoutes();
		
		Map<String, String> substitutions = new TreeMap<>();
		StringBuilder sb = new StringBuilder(1024);
		sb.append("import 'package:").append(generator.projectName).append("/widgets/skyve_menu.dart';");
		imports.add(sb.toString());
		sb.setLength(0);
		imports.forEach(i -> sb.append(i).append('\n'));
		sb.setLength(sb.length() - 1); // remove \n
		substitutions.put("##IMPORTS##", sb.toString());
		
		menu.setLength(menu.length() - 2); // remove ,\n
		menu.append("\n];");
		substitutions.put("##MENU##", menu.toString());

		
		sb.setLength(0);
		sb.append("final List<GoRoute> _goRoutes = [\n");
		routes.forEach(r -> sb.append(INDENT).append(INDENT).append(r).append('\n'));
		sb.append(INDENT).append("];\n");
		substitutions.put("##ROUTES##", sb.toString());

		generator.refreshFile("lib/main.dart", "lib/main.dart", substitutions);
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
					FlutterEditView view = new FlutterEditView(generator, moduleName, documentName);
					view.setViews(m, d);
					processItem(view, null);
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
				String viewName = ((modelName == null) ? itemQueryName : modelName) + "Cal";
				processItem(new FlutterCalendarView(generator, moduleName, viewName), item);
			}
			
			@Override
			public void renderMenuGroup(MenuGroup menuGroup, Module menuModule) {
				openMenuGroup(menuGroup.getLocalisedName());
			}

			@Override
			public void renderedMenuGroup(MenuGroup menuGroup, Module menuModule) {
				closeMenuGroup();
			}

			@Override
			public void renderModuleMenu(org.skyve.metadata.module.menu.Menu skyveMenu, Module menuModule, boolean open) {
				openMenuModule(menuModule.getLocalisedTitle());
			}

			@Override
			public void renderedModuleMenu(org.skyve.metadata.module.menu.Menu skyveMenu, Module menuModule, boolean open) {
				closeMenuModule();
			}

			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				String moduleName = itemModule.getName();
				String viewName = itemDocument.getName();
				FlutterEditView view = new FlutterEditView(generator, moduleName, viewName);
				view.setViews(itemModule, itemDocument);
				processItem(view, item);
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
				String viewName = ((modelName == null) ? itemQueryName : modelName) + "List";
				FlutterListView component = new FlutterListView(generator, moduleName, viewName);
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
				processItem(component, item);
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
				String viewName = ((modelName == null) ? itemQueryName : modelName) + "Map";
				processItem(new FlutterMapView(generator, moduleName, viewName), item);
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
				String viewName = ((modelName == null) ? itemQueryName : modelName) + "Tree";
				processItem(new FlutterTreeView(generator, moduleName, viewName), item);
			}
		}.render(c);
	}
	
	private void processItem(FlutterView view, MenuItem item) {
		final String className = view.className;

		// Add the import
		StringBuilder sb = new StringBuilder(128);
		sb.append("import 'package:").append(generator.projectName).append("/views/").append(view.moduleName).append('/').append(view.fileName).append("';");
		imports.add(sb.toString());

		indent();
		// TODO implement FA icon
		if (item != null) {
			menu.append("SkyveMenuData(label: '").append(item.getLocalisedName()).append("', routeName: ").append(className)
					.append(".routeName").append(", icon: null),\n");
		}

		// Create the GoRoute declaration
		String pattern = "GoRoute(path: %s.routeName, builder: (context, state) => %s(queryParams: state.queryParams)),";
		String goRoute = String.format(pattern, className, className);
		routes.add(goRoute);
		
		generator.views.add(view);
	}

	private void openMenuModule(String moduleName) {
		indent();
		menu.append("SkyveMenuModule(label: '").append(moduleName).append("', items: [\n");
		indentationLevel++;
	}

	private void closeMenuModule() {
		closeCollapsibleMenu();
	}

	private void openMenuGroup(String menuGroupName) {
		indent();
		menu.append("SkyveMenuGroup(label: '").append(menuGroupName).append("', children: [\n");
		indentationLevel++;
	}

	private void closeMenuGroup() {
		closeCollapsibleMenu();
	}

	private void closeCollapsibleMenu() {
		indentationLevel--;
		indent();
		menu.append("]),\n");
	}

	private void indent() {
		for (int indentCount = 0; indentCount <= indentationLevel; indentCount++) {
			menu.append(INDENT);
		}
	}
}

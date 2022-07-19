package org.skyve.impl.metadata.user;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;

import org.skyve.CORE;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

class AccessProcessor {
	private User user;
	private Customer customer;
	private Map<String, Menu> moduleMenuMap;
	private Map<String, Set<String>> accesses;
	private Router router;

	private Module module;
	private String moduleName;
	private Menu menu;
	
	AccessProcessor(User user, Map<String, Menu> moduleMenuMap, Map<String, Set<String>> accesses) {
		this.user = user;
		this.customer = user.getCustomer();
		this.moduleMenuMap = moduleMenuMap;
		this.accesses = accesses;
		router = CORE.getRepository().getRouter();
	}
	
	/**
	 * This method determines the accesses allowed.
	 * It is synchronized on the current thread of execution to enable dev mode to function with multiple threads re-processing the accesses.
	 */
	void process() {
		synchronized (Thread.currentThread()) {
			for (Entry<String, Menu> entry : moduleMenuMap.entrySet()) {
				moduleName = entry.getKey();
				module = customer.getModule(moduleName);
				menu = entry.getValue();
	
				processMenuItems(menu.getItems());
			}
		}
System.out.println(accesses.size());
	}

	private void processMenuItems(List<MenuItem> items) {
		for (MenuItem item : items) {
			// NB Disregard LinkItem as it is outside of accesses
			if (item instanceof MenuGroup) {
				processMenuItems(((MenuGroup) item).getItems());
			}
			else if (item instanceof EditItem) {
				EditItem edit = (EditItem) item;
				String documentName = edit.getDocumentName();
				addMenuAccess(UserAccess.singular(moduleName, documentName), edit.getUxUis());
				Document document = module.getDocument(customer, documentName);
				processViews(document);
			}
			else if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
				AbstractDocumentOrQueryOrModelMenuItem aggregate = (AbstractDocumentOrQueryOrModelMenuItem) item;
				String queryName = aggregate.getQueryName();
				if (queryName != null) {
					addMenuAccess(UserAccess.queryAggregate(moduleName, queryName), aggregate.getUxUis());
				}
				else {
					String documentName = aggregate.getDocumentName();
					DocumentRef ref = module.getDocumentRefs().get(documentName);
					queryName = ref.getDefaultQueryName();
					if (queryName != null) {
						addMenuAccess(UserAccess.queryAggregate(moduleName, queryName), aggregate.getUxUis());
					}
					else {
						String modelName = aggregate.getModelName();
						if (modelName != null) {
							addMenuAccess(UserAccess.modelAggregate(moduleName, documentName, modelName), aggregate.getUxUis());
						}
						else {
							addMenuAccess(UserAccess.documentAggregate(moduleName, documentName), aggregate.getUxUis());
						}
					}
				}
			}
		}
	}
	
	private void processViews(Document document) {
		Set<String> processedUxUis = new TreeSet<>();
		
		for (UxUiMetadata uxui : router.getUxUis()) {
			String uxuiName = uxui.getName();
			View createView = document.getView(uxuiName, customer, ViewType.create.toString());
			String uxuiKey = ViewType.create.toString() + createView.getOverriddenUxUiName();

			// create and edit view are the same - use edit view
			if (ViewType.edit.toString().equals(createView.getName())) {
				if (! processedUxUis.contains(uxuiKey)) {
					processView(document, createView);
					processedUxUis.add(uxuiKey);
				}
			}
			else {
				if (! processedUxUis.contains(uxuiKey)) {
					processView(document, createView);
					processedUxUis.add(uxuiKey);
				}

				View editView = document.getView(uxuiName, customer, ViewType.edit.toString());
				uxuiKey = createView.getOverriddenUxUiName();
				if (uxuiKey == null) {
					uxuiKey = "";
				}
				if (! processedUxUis.contains(uxuiKey)) {
					processView(document,editView);
					processedUxUis.add(uxuiKey);
				}
			}
		}
	}
	
	private void processView(Document document, View view) {
		final String overriddenUxUi = view.getOverriddenUxUiName();
		
		new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view) {
			@Override
			public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
/*
				// TODO  What about inline chart models.
				ChartBuilderMetaData metaDataModel = chart.getModel();
				if (metaDataModel != null) {
					String moduleName = metaDataModel.getModuleName();
					String queryName = metaDataModel.getQueryName();
					if (queryName != null) {
						addViewAccess(UserAccess.queryAggregate(moduleName, queryName), aggregate.getUxUis());
					}
					else {
						String documentName = aggregate.getDocumentName();
						DocumentRef ref = module.getDocumentRefs().get(documentName);
						queryName = ref.getDefaultQueryName();
						if (queryName != null) {
							addMenuAccess(UserAccess.queryAggregate(moduleName, queryName), aggregate.getUxUis());
						}
						else {
							String modelName = aggregate.getModelName();
							if (modelName != null) {
								addMenuAccess(UserAccess.modelAggregate(moduleName, documentName, modelName), aggregate.getUxUis());
							}
							else {
								addMenuAccess(UserAccess.documentAggregate(moduleName, documentName), aggregate.getUxUis());
							}
						}
					}
					String documentName = metaDataModel.getDocumentName();
					String queryName = metaDataModel.getQueryName();
					addViewAccess(UserAccess., moduleName);
				}
				chart.getModelName();
				accessVectors.add(AccessVector.queryAggregate(moduleName, moduleName))
				addViewAccess(UserAccess.queryAggregate(moduleName, "CHART" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
*/
			}

			@Override
			public void visitDataGrid(org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid grid, boolean parentVisible, boolean parentEnabled) {
//				zoomIn.getBinding()
//				Reference reference = null; // get reference for the document name
//				String documentName = reference.getDocumentName();
//				accessVectors.add(AccessVector.singular(moduleName, documentName))
				addViewAccess(UserAccess.queryAggregate(moduleName, "DATAGRID" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			// NB DataRepeater cannot zoom in
			
			@Override
			public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
//				grid.getModelName();
//				grid.getQueryName();
//				
//				accessVectors - fuck need driving document
				addViewAccess(UserAccess.queryAggregate(moduleName, "LISTGRID" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
				addViewAccess(UserAccess.singular(moduleName, "LISTGRID NEW DOCUMENT" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			@Override
			public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
//				repeater.getModelName();
//				repeater.getQueryName()
				// NB ListRepeater cannot zoom in
				addViewAccess(UserAccess.queryAggregate(moduleName, "LISTREPEATER" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			@Override
			public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				// TODO query name "query" or get document binding attribute and check if query, else use default doc query
//				String queryName = lookup.getQuery();
//				lookup.getBinding();
//				Reference reference = null; // get reference for the query
//				queryName = reference.getQueryName();
//				String documentName = reference.getDocumentName();
//				DocumentRef ref = module.getDocumentRefs().get(documentName);
//				queryName = ref.getDefaultQueryName();
				addViewAccess(UserAccess.queryAggregate(moduleName, "LOOKUP" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
				addViewAccess(UserAccess.singular(moduleName, "LOOKUP NEW DOCUMENT" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			@Override
			public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
//				map.getModelName();
				addViewAccess(UserAccess.queryAggregate(moduleName, "MAP" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
				addViewAccess(UserAccess.singular(moduleName, "MAP NEW DOCUMENT" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			@Override
			public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
//				grid.getModelName();
//				grid.getQueryName();
				addViewAccess(UserAccess.queryAggregate(moduleName, "TREEGRID" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
				addViewAccess(UserAccess.singular(moduleName, "TREEGRID NEW DOCUMENT" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
			
			@Override
			public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
//				zoomIn.getBinding()
//				Reference reference = null; // get reference for the document name
//				String documentName = reference.getDocumentName();
//				accessVectors.add(AccessVector.singular(moduleName, documentName))
				addViewAccess(UserAccess.singular(moduleName, "ZOOMIN NEW DOCUMENT" + UUID.randomUUID().toString()), view.getOverriddenUxUiName());
			}
		}.visit();
	}
	
	private void addMenuAccess(@NotNull UserAccess access, @NotNull Set<String> uxuis) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) {
			if (! uxuis.isEmpty()) {
				accessUxUis = new HashSet<>(uxuis);
				accesses.put(accessString, accessUxUis);
			}
			else {
				accesses.putIfAbsent(accessString, null);
			}
		}
		else {
			accessUxUis.addAll(uxuis);
		}
	}

//	private UserAccess determineUserAccess(String queryName, )
	private void addViewAccess(@NotNull UserAccess access, @Nullable String uxui) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) {
			if (uxui != null) {
				accessUxUis = new HashSet<>();
				accessUxUis.add(uxui);
			}
			accesses.put(accessString, accessUxUis);
		}
		else {
			accessUxUis.add(uxui);
		}
	}
}

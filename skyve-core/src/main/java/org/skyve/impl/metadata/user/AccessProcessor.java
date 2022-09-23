package org.skyve.impl.metadata.user;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.impl.bind.BindUtil;
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
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;

class AccessProcessor {
	static final Set<String> ALL_UX_UIS = Collections.emptySet();
	
	private Customer customer;
	private Map<String, Menu> moduleMenuMap;
	private Map<String, Set<String>> accesses;
	private Router router;

	AccessProcessor(Customer customer, Map<String, Menu> moduleMenuMap, Map<String, Set<String>> accesses) {
		this.customer = customer;
		this.moduleMenuMap = moduleMenuMap;
		this.accesses = accesses;
		router = CORE.getRepository().getRouter();
	}
	
	/**
	 * This method determines the accesses allowed.
	 * It is synchronized on the current thread of execution to enable dev mode to function with multiple threads re-processing the accesses in a serialised fashion.
	 */
	void process() {
		synchronized (Thread.currentThread()) {
			for (Entry<String, Menu> entry : moduleMenuMap.entrySet()) {
				final String moduleName = entry.getKey();
				final Module module = customer.getModule(moduleName);
				processModuleHome(module, moduleName);
				
				final Menu menu = entry.getValue();
				processMenuItems(menu.getItems(), module, moduleName);
			}
			processedUxUiViews.clear();
		}
//for (String a : accesses.keySet()) {
//	System.out.println(a);
//	for (String u : accesses.get(a)) {
//		System.out.println(" u=" + u);
//	}
//}
	}

	private Set<String> processedUxUiViews = new TreeSet<>();

	private void processModuleHome(final Module module, final String moduleName) {
		String homeDocumentName = module.getHomeDocumentName();
		ViewType homeRef = module.getHomeRef();
		if (homeRef == ViewType.list) {
			DocumentRef ref = module.getDocumentRefs().get(homeDocumentName);
			String queryName = ref.getDefaultQueryName();
			if (queryName != null) {
				addAccessFromMenuItem(UserAccess.queryAggregate(moduleName, queryName), Collections.emptySet());
			}
			else {
				addAccessFromMenuItem(UserAccess.documentAggregate(moduleName, homeDocumentName), Collections.emptySet());
			}
		}
		else if (homeRef == ViewType.edit) {
			addAccessFromMenuItem(UserAccess.singular(moduleName, homeDocumentName), Collections.emptySet());
			Document document = module.getDocument(customer, homeDocumentName);
			processViews(document);
		}
	}
	
	private void processMenuItems(final List<MenuItem> items, final Module module, final String moduleName) {
		for (MenuItem item : items) {
			// NB Disregard LinkItem as it is outside of accesses
			if (item instanceof MenuGroup) {
				processMenuItems(((MenuGroup) item).getItems(), module, moduleName);
			}
			else if (item instanceof EditItem) {
				EditItem edit = (EditItem) item;
				String documentName = edit.getDocumentName();
				addAccessFromMenuItem(UserAccess.singular(moduleName, documentName), edit.getUxUis());
				Document document = module.getDocument(customer, documentName);
				processViews(document);
			}
			else if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
				AbstractDocumentOrQueryOrModelMenuItem aggregate = (AbstractDocumentOrQueryOrModelMenuItem) item;
				String documentName = null;
				String queryName = aggregate.getQueryName();
				Set<String> uxuis = aggregate.getUxUis();
				if (queryName != null) {
					addAccessFromMenuItem(UserAccess.queryAggregate(moduleName, queryName), uxuis);
					MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
					documentName = query.getDocumentName();
				}
				else {
					documentName = aggregate.getDocumentName();
					DocumentRef ref = module.getDocumentRefs().get(documentName);
					queryName = ref.getDefaultQueryName();
					if (queryName != null) {
						addAccessFromMenuItem(UserAccess.queryAggregate(moduleName, queryName), uxuis);
					}
					else {
						String modelName = aggregate.getModelName();
						if (modelName != null) {
							addAccessFromMenuItem(UserAccess.modelAggregate(moduleName, documentName, modelName), uxuis);
						}
						else {
							addAccessFromMenuItem(UserAccess.documentAggregate(moduleName, documentName), uxuis);
						}
					}
				}
				
				addAccessFromMenuItem(UserAccess.singular(moduleName, documentName), uxuis);
				Document document = module.getDocument(customer, documentName);
				processViews(document);
			}
		}
	}
	
	private void processViews(Document document) {
		for (UxUiMetadata uxui : router.getUxUis()) {
			String uxuiName = uxui.getName();
			View createView = document.getView(uxuiName, customer, ViewType.create.toString());
			String uxuiViewKey = null;

			// create and edit view are the same - use edit view
			if (ViewType.edit.toString().equals(createView.getName())) {
				uxuiViewKey = document.getOwningModuleName() + '.' + document.getName() + ':' + createView.getOverriddenUxUiName();
				if (! processedUxUiViews.contains(uxuiViewKey)) {
					processedUxUiViews.add(uxuiViewKey);
					processView(document, createView);
				}
			}
			else {
				uxuiViewKey = document.getOwningModuleName() + '.' + document.getName() + ':' + ViewType.create.toString() + createView.getOverriddenUxUiName();
				if (! processedUxUiViews.contains(uxuiViewKey)) {
					processedUxUiViews.add(uxuiViewKey);
					processView(document, createView);
				}

				View editView = document.getView(uxuiName, customer, ViewType.edit.toString());
				uxuiViewKey = document.getOwningModuleName() + '.' + document.getName() + ':' + editView.getOverriddenUxUiName();
				if (! processedUxUiViews.contains(uxuiViewKey)) {
					processedUxUiViews.add(uxuiViewKey);
					processView(document, editView);
				}
			}
		}
	}
	
	private void processView(Document document, View view) {
		final String overriddenUxUi = view.getOverriddenUxUiName();
		final String documentName = document.getName();
		final Module module = customer.getModule(document.getOwningModuleName());
		final String moduleName = module.getName();
		
		new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view) {
			@Override
			public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
				ChartBuilderMetaData metaDataModel = chart.getModel();
				if (metaDataModel != null) {
					String modelModuleName = metaDataModel.getModuleName();
					String modelQueryName = metaDataModel.getQueryName();
					if (modelQueryName != null) {
						addAccessFromView(UserAccess.queryAggregate(modelModuleName, modelQueryName), overriddenUxUi);
					}
					else {
						Module modelModule = customer.getModule(modelModuleName);
						String moduleDocumentName = metaDataModel.getDocumentName();
						DocumentRef ref = modelModule.getDocumentRefs().get(documentName);
						modelQueryName = ref.getDefaultQueryName();
						if (modelQueryName != null) {
							addAccessFromView(UserAccess.queryAggregate(modelModuleName, modelQueryName), overriddenUxUi);
						}
						else {
							addAccessFromView(UserAccess.documentAggregate(modelModuleName, moduleDocumentName), overriddenUxUi);
						}
					}
				}
				else {
					String modelName = chart.getModelName();
					addAccessFromView(UserAccess.modelAggregate(moduleName, documentName, modelName), overriddenUxUi);
				}
			}

			private String dataGridBinding = null;
			
			@Override
			public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
				if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
					dataGridBinding = grid.getBinding();
					accessThroughBinding(dataGridBinding);
				}
			}

			@Override
			public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
				dataGridBinding = null;
			}
			
			// NB DataRepeater cannot zoom in

			@Override
			public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
				String modelName = grid.getModelName();
				String queryName = grid.getQueryName();
				if (modelName != null) {
					addAccessFromView(UserAccess.modelAggregate(moduleName, documentName, modelName), overriddenUxUi);
				}
				else {
					addAccessFromView(UserAccess.queryAggregate(moduleName, queryName), overriddenUxUi);
				}

				if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
					Document drivingDocument = null;
					String drivingModuleName = null;
					String drivingDocumentName = null;
					
					if (modelName != null) {
						ListModel<Bean> model = document.getListModel(customer, modelName, true);
						drivingDocument = model.getDrivingDocument();
						drivingModuleName = drivingDocument.getOwningModuleName();
						drivingDocumentName = drivingDocument.getName();
					}
					else {
						MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
						drivingDocumentName = query.getDocumentName();
						Module drivingModule = query.getDocumentModule(customer);
						drivingModuleName = drivingModule.getName();
						drivingDocument = drivingModule.getDocument(customer, drivingDocumentName);
					}

					UserAccess singular = UserAccess.singular(drivingModuleName, drivingDocumentName);
					boolean has = hasViewAccess(singular);
					addAccessFromView(singular, overriddenUxUi);
					if (! has) {
						processViews(drivingDocument);
					}
				}
			}
			
			@Override
			public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
				String modelName = repeater.getModelName();
				if (modelName != null) {
					addAccessFromView(UserAccess.modelAggregate(moduleName, documentName, modelName), overriddenUxUi);
				}
				else {
					String queryName = repeater.getQueryName();
					addAccessFromView(UserAccess.queryAggregate(moduleName, queryName), overriddenUxUi);
				}
				// NB ListRepeater cannot zoom in
			}
			
			@Override
			public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				String binding = lookup.getBinding();
				if (dataGridBinding != null) {
					if (binding == null) { // binding can be null when placed in a data grid
						binding = dataGridBinding;
					}
					else {
						StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
						sb.append(dataGridBinding).append('.').append(binding);
						binding = sb.toString();
					}
				}
				
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				Reference targetReference = (Reference) target.getAttribute();
				String targetDocumentName = targetReference.getDocumentName();

				// Check lookup query
				String queryName = lookup.getQuery();
				// Maybe the reference has a query
				if (queryName == null) {
					queryName = targetReference.getQueryName();
				}
				// Look for the default query
				if (queryName == null) {
					queryName = module.getDocumentDefaultQuery(customer, targetDocumentName).getName();
				}
				// Otherwise use the document name
				queryName = targetDocumentName;
				addAccessFromView(UserAccess.queryAggregate(moduleName, queryName), overriddenUxUi);

				if (! Boolean.FALSE.equals(lookup.getEditable())) {
					Document targetDocument = module.getDocument(customer, targetDocumentName);
					String targetModuleName = targetDocument.getOwningModuleName();
					UserAccess singular = UserAccess.singular(targetModuleName, targetDocumentName);
					boolean has = hasViewAccess(singular);
					addAccessFromView(singular, overriddenUxUi);
					if (! has) {
						processViews(targetDocument);
					}
				}
			}
			
			@Override
			public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
				String modelName = map.getModelName();
				addAccessFromView(UserAccess.modelAggregate(moduleName, documentName, modelName), overriddenUxUi);

				// NB Can't work out what the map can navigate to - needs to be added to the router manually.
			}
			
			@Override
			public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
				CompleteType type = text.getComplete();
				if (type == CompleteType.previous) {
					String binding = text.getBinding();
					if (dataGridBinding != null) {
						StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
						sb.append(dataGridBinding).append('.').append(binding);
						binding = sb.toString();
					}
					addAccessFromView(UserAccess.previousComplete(moduleName, documentName, binding), overriddenUxUi);
				}
			}
			
			@Override
			public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
				visitListGrid(grid, parentVisible, parentEnabled);
			}
			
			@Override
			public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
				accessThroughBinding(zoomIn.getBinding());
			}

			private void accessThroughBinding(String binding) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);

				Document relatedDocument = null;
				if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
					relatedDocument = target.getDocument().getParentDocument(customer);
				}
				else {
					Relation targetRelation = (Relation) target.getAttribute();
					String relatedDocumentName = targetRelation.getDocumentName();
					relatedDocument = module.getDocument(customer, relatedDocumentName);
				}
				@SuppressWarnings("null")
				String relatedModuleName = relatedDocument.getOwningModuleName();
				UserAccess singular = UserAccess.singular(relatedModuleName, relatedDocument.getName());
				boolean has = hasViewAccess(singular);
				addAccessFromView(singular, overriddenUxUi);
				if (! has) {
					processViews(relatedDocument);
				}
			}
		}.visit();
	}
	
	private void addAccessFromMenuItem(@NotNull UserAccess access, @NotNull Set<String> uxuis) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) { // DNE
			if (! uxuis.isEmpty()) {
				accessUxUis = new HashSet<>(uxuis);
				accesses.put(accessString, accessUxUis);
			}
			else {
				accesses.putIfAbsent(accessString, ALL_UX_UIS);
			}
		}
		else if (accessUxUis != ALL_UX_UIS) {
			if (! uxuis.isEmpty()) {
				accessUxUis.addAll(uxuis);
			}
			else {
				accesses.put(accessString, ALL_UX_UIS);
			}
		}
	}

	private void addAccessFromView(@NotNull UserAccess access, @Nullable String uxui) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) {
			if (uxui != null) {
				accessUxUis = new HashSet<>();
				accessUxUis.add(uxui);
				accesses.put(accessString, accessUxUis);
			}
			else {
				accesses.putIfAbsent(accessString, ALL_UX_UIS);
			}
		}
		else if (accessUxUis != ALL_UX_UIS){
			if (uxui != null) {
				accessUxUis.add(uxui);
			}
			else {
				accesses.put(accessString, ALL_UX_UIS);
			}
		}
	}
	
	private boolean hasViewAccess(@NotNull UserAccess singularUserAccess) {
		return accesses.containsKey(singularUserAccess.toString());
	}
}

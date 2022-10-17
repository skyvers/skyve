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
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

class AccessProcessor {
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
	 */
	void process() {
		for (Entry<String, Menu> entry : moduleMenuMap.entrySet()) {
			final String moduleName = entry.getKey();
			final Module module = customer.getModule(moduleName);
			processModuleHome(module, moduleName);
			
			final Menu menu = entry.getValue();
			processMenuItems(menu.getItems(), module, moduleName);
			
			processRoles(module);
		}
		processedUxUiViews.clear();
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
				addAccessForUxUis(UserAccess.queryAggregate(moduleName, queryName), Collections.emptySet());
			}
			else {
				addAccessForUxUis(UserAccess.documentAggregate(moduleName, homeDocumentName), Collections.emptySet());
			}
		}
		else if (homeRef == ViewType.edit) {
			addAccessForUxUis(UserAccess.singular(moduleName, homeDocumentName), Collections.emptySet());
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
				addAccessForUxUis(UserAccess.singular(moduleName, documentName), edit.getUxUis());
				Document document = module.getDocument(customer, documentName);
				processViews(document);
			}
			else if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
				AbstractDocumentOrQueryOrModelMenuItem aggregate = (AbstractDocumentOrQueryOrModelMenuItem) item;
				String documentName = null;
				String queryName = aggregate.getQueryName();
				Set<String> uxuis = aggregate.getUxUis();
				if (queryName != null) {
					addAccessForUxUis(UserAccess.queryAggregate(moduleName, queryName), uxuis);
					MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
					documentName = query.getDocumentName();
				}
				else {
					documentName = aggregate.getDocumentName();
					DocumentRef ref = module.getDocumentRefs().get(documentName);
					queryName = ref.getDefaultQueryName();
					if (queryName != null) {
						addAccessForUxUis(UserAccess.queryAggregate(moduleName, queryName), uxuis);
					}
					else {
						String modelName = aggregate.getModelName();
						if (modelName != null) {
							addAccessForUxUis(UserAccess.modelAggregate(moduleName, documentName, modelName), uxuis);
						}
						else {
							addAccessForUxUis(UserAccess.documentAggregate(moduleName, documentName), uxuis);
						}
					}
				}
				
				addAccessForUxUis(UserAccess.singular(moduleName, documentName), uxuis);
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
	
	private void processRoles(final Module module) {
		for (Role role : module.getRoles()) {
			Map<UserAccess, Set<String>> roleAccesses = ((RoleImpl) role).getAccesses();
			for (Entry<UserAccess, Set<String>> entry : roleAccesses.entrySet()) {
				addAccessForUxUis(entry.getKey(), entry.getValue());
			}
		}
	}
	
	private void processView(Document document, View view) {
		final String overriddenUxUi = view.getOverriddenUxUiName();
		final Module module = customer.getModule(document.getOwningModuleName());
		
		Set<UserAccess> viewAccesses = view.getAccesses();
		if (viewAccesses != null) { // can be null when access control is turned off
			for (UserAccess viewAccess : viewAccesses) {
				if (viewAccess.isSingular()) {
					boolean has = hasViewAccess(viewAccess);
					addAccessForUxUi(viewAccess, overriddenUxUi);
					if (! has) {
						processViews(module.getDocument(customer, viewAccess.getDocumentName()));
					}
				}
				else {
					addAccessForUxUi(viewAccess, overriddenUxUi);
				}
			}
		}
	}
	
	private void addAccessForUxUis(@NotNull UserAccess access, @NotNull Set<String> uxuis) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) { // DNE
			if (! uxuis.isEmpty()) {
				accessUxUis = new HashSet<>(uxuis);
				accesses.put(accessString, accessUxUis);
			}
			else {
				accesses.putIfAbsent(accessString, UserAccess.ALL_UX_UIS);
			}
		}
		else if (accessUxUis != UserAccess.ALL_UX_UIS) {
			if (! uxuis.isEmpty()) {
				accessUxUis.addAll(uxuis);
			}
			else {
				accesses.put(accessString, UserAccess.ALL_UX_UIS);
			}
		}
	}

	private void addAccessForUxUi(@NotNull UserAccess access, @Nullable String uxui) {
		String accessString = access.toString();
		Set<String> accessUxUis = accesses.get(accessString);
		if (accessUxUis == null) {
			if (uxui != null) {
				accessUxUis = new HashSet<>();
				accessUxUis.add(uxui);
				accesses.put(accessString, accessUxUis);
			}
			else {
				accesses.putIfAbsent(accessString, UserAccess.ALL_UX_UIS);
			}
		}
		else if (accessUxUis != UserAccess.ALL_UX_UIS){
			if (uxui != null) {
				accessUxUis.add(uxui);
			}
			else {
				accesses.put(accessString, UserAccess.ALL_UX_UIS);
			}
		}
	}
	
	private boolean hasViewAccess(@NotNull UserAccess singularUserAccess) {
		return accesses.containsKey(singularUserAccess.toString());
	}
}

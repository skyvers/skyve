package org.skyve.impl.metadata.repository;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.CORE;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.MutableRepository;
import org.skyve.metadata.repository.OnDemandRepository;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public abstract class MutableCachedRepository extends ProvidedRepositoryDelegate implements MutableRepository, OnDemandRepository {
	/**
	 * The cache.
	 * MetaData namespace and name -> MetaData.
	 * eg customers/bizhub
	 *    modules/admin
	 *    modules/admin/Contact
	 * Thread-safe and performant for mostly-read operations.
	 * NB The population of the cache cannot use computeIfPresent() and computeIfAbsent() because the processing could
	 * 		end up trying to compute the same key within the labda functions arguments to these methods which will throw
	 * 		java.lang.IllegalStateException: Recursive update as per the API contract.
	 */
	private ConcurrentHashMap<String, Optional<MetaData>> cache = new ConcurrentHashMap<>();
	
	protected static final String ROUTER_KEY = ROUTER_NAMESPACE + ROUTER_NAME;

	@Override
	public void evictCachedMetaData(Customer customer) {
		// Clear the lot
		if (customer == null) {
			cache.clear();
		}
		else {
			// Clear any customer overrides and any modules the customer has access to.
			// NB moduleNames set is in insertion order
			Set<String> moduleNames = ((CustomerImpl) customer).getModuleEntries().keySet();
			List<String> modulePrefixes = new ArrayList<>(moduleNames.size());
			for (String moduleName : moduleNames) {
				modulePrefixes.add(MODULES_NAMESPACE + moduleName);
			}
			
			Iterator<String> i = cache.keySet().iterator();
			final String customerOverridePrefix = CUSTOMERS_NAMESPACE + customer.getName();
			while (i.hasNext()) {
				String key = i.next();
				if (key.startsWith(customerOverridePrefix)) {
					i.remove();
					continue;
				}
				for (String modulePrefix : modulePrefixes) {
					if (key.startsWith(modulePrefix)) {
						i.remove();
						break;
					}
				}
			}
		}
	}
	
	@Override
	public Router getRouter() {
		Optional<MetaData> result = cache.get(ROUTER_KEY);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				Router router = loadRouter();
				router = router.convert(ROUTER_NAME, getDelegator());
				result = Optional.of(router);
				cache.put(ROUTER_KEY, result);
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					Router r = (Router) result.get();
					if (r.getLastModifiedMillis() < routerLastModifiedMillis()) {
						Router router = loadRouter();
						router = router.convert(ROUTER_NAME, getDelegator());
						result = Optional.of(router);
						cache.put(ROUTER_KEY, result);
					}
				}
			}
		}

		return ((result == null) || result.isEmpty()) ? null : (Router) result.get();
	}
	
	@Override
	public Router setRouter(Router router) {
		Router result = router.convert(ROUTER_NAME, getDelegator());
		// Ignore dev mode flag here as we need to seed the cache in this method.
		cache.put(ROUTER_KEY, Optional.of(result));
		return result;
	}

	@Override
	public Customer getCustomer(String customerName) {
		String customerKey = CUSTOMERS_NAMESPACE + customerName;
		Optional<MetaData> result = cache.get(customerKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				CustomerMetaData customerMetaData = loadCustomer(customerName);
				Customer customer = customerMetaData.convert(customerName, getDelegator());
				result = Optional.of(customer);
				cache.put(customerKey, result);
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					Customer c = (Customer) result.get();
					if (c.getLastModifiedMillis() < customerLastModifiedMillis(customerName)) {
						CustomerMetaData customerMetaData = loadCustomer(customerName);
						Customer customer = customerMetaData.convert(customerName, getDelegator());
						result = Optional.of(customer);
						cache.put(customerKey, result);
						// Validate the current customer if we are in dev mode and the metadata has been touched
						DomainGenerator.validate(this, customerName);
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (Customer) result.get();
	}
	
	@Override
	public Customer putCustomer(CustomerMetaData customer) {
		String customerName = customer.getName();
		Customer result = customer.convert(customerName, getDelegator());
		cache.put(CUSTOMERS_NAMESPACE + customerName, Optional.of(result));
		return result;
	}

	@Override
	public Module getModule(Customer customer, String moduleName) {
		Module result = null;
		if (customer != null) {
			if (! ((CustomerImpl) customer).getModuleEntries().containsKey(moduleName)) {
				throw new MetaDataException("Module " + moduleName + " does not exist for customer " + customer.getName());
			}
			// get customer overridden
			result = getModuleInternal(customer, moduleName);
		}
		if (result == null) { // not overridden
			result = getModuleInternal(null, moduleName);
		}
		// Note we can not test for existence of the module here because it may be a repository in a DelegatedProvidedRepositoryChain (DefaultRepository)
		return result;
	}
	
	private @Nullable Module getModuleInternal(@Nullable Customer customer, @Nonnull String moduleName) {
		final String customerName = (customer == null) ? null : customer.getName();
		StringBuilder moduleKeySB = new StringBuilder(64);
		if (customerName != null) {
			moduleKeySB.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		moduleKeySB.append(MODULES_NAMESPACE).append(moduleName);
		String moduleKey = moduleKeySB.toString();
		Optional<MetaData> result = cache.get(moduleKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				ModuleMetaData moduleMetaData = loadModule(customerName, moduleName);
				Module module = convertModule(customerName, moduleName, moduleMetaData);
				result = Optional.of(module);
				cache.put(moduleKey, result);
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					Module m = (Module) result.get();
					if (m.getLastModifiedMillis() < moduleLastModifiedMillis(customerName, moduleName)) {
						ModuleMetaData moduleMetaData = loadModule(customerName, moduleName);
						Module module = convertModule(customerName, moduleName, moduleMetaData);
						result = Optional.of(module);
						cache.put(moduleKey, result);
						// Validate the current customer if we are in dev mode and the metadata has been touched
						DomainGenerator.validate(this, (customerName == null) ? CORE.getCustomer().getName() : customerName);
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (Module) result.get();
	}

	private @Nonnull Module convertModule(@Nullable String customerName, @Nonnull String moduleName, @Nonnull ModuleMetaData module) {
		String metaDataName = null;
		if (customerName != null) {
			metaDataName = new StringBuilder(64).append(moduleName).append(" (").append(customerName).append(')').toString();
		}
		else {
			metaDataName = moduleName;
		}
		return module.convert(metaDataName, getDelegator());
	}
	
	@Override
	public Module putModule(Customer customer, ModuleMetaData module) {
		String customerName = customer.getName();
		String moduleName = module.getName();
		Module result = convertModule(customerName, moduleName, module);
		
		StringBuilder moduleKey = new StringBuilder(64);
		moduleKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/').append(MODULES_NAMESPACE).append(moduleName);
		cache.put(moduleKey.toString(), Optional.of(result));
		
		return result;
	}
	
	@Override
	public Module putModule(ModuleMetaData module) {
		String moduleName = module.getName();

		Module result = convertModule(null, moduleName, module);
		cache.put(MODULES_NAMESPACE + moduleName, Optional.of(result));
		
		return result;
	}
	
	@Override
	public Document getDocument(Customer customer, Module module, String documentName) {
		Document result = null;
		if (customer != null) {
			// get customer overridden
			result = getDocumentInternal(true, customer, module, documentName);
		}
		if (result == null) { // not overridden
			result = getDocumentInternal(false, customer, module, documentName);
		}
		// Note we can not test for existence of the module here because it may be a repository in a DelegatedProvidedRepositoryChain (DefaultRepository)
		return result;
	}

	private @Nullable Document getDocumentInternal(boolean customerOverride, @Nullable Customer customer, @Nonnull Module module, @Nonnull String documentName) {
		DocumentRef ref = module.getDocumentRefs().get(documentName);
		if (ref == null) {
			throw new IllegalArgumentException(documentName + " does not exist for this module - " + module.getName());
		}
		String documentModuleName = ((ref.getReferencedModuleName() == null) ? module.getName() : ref.getReferencedModuleName());

		final String customerName = (customer == null) ? null : customer.getName();
		StringBuilder documentKeySB = new StringBuilder(64);
		if (customerOverride) {
			documentKeySB.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		documentKeySB.append(MODULES_NAMESPACE).append(documentModuleName).append('/').append(documentName);
		String documentKey = documentKeySB.toString();
		Optional<MetaData> result = cache.get(documentKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				DocumentMetaData documentMetaData = loadDocument(customerOverride ? customerName : null, documentModuleName, documentName);
				Module documentModule = getModule(customer, documentModuleName);
				Document document = convertDocument(customerName, documentModuleName, documentModule, documentName, documentMetaData);
				result = Optional.of(document);
				cache.put(documentKey, result);
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					Document d = (Document) result.get();
					if (d.getLastModifiedMillis() < documentLastModifiedMillis(customerOverride ? customerName : null, documentModuleName, documentName)) {
						DocumentMetaData documentMetaData = loadDocument(customerOverride ? customerName : null, documentModuleName, documentName);
						Module documentModule = getModule(customer, documentModuleName);
						Document document = convertDocument(customerName, documentModuleName, documentModule, documentName, documentMetaData);
						result = Optional.of(document);
						cache.put(documentKey, result);
						// Validate the current customer if we are in dev mode and the metadata has been touched
						DomainGenerator.validate(this, (customerName == null) ? CORE.getCustomer().getName() : customerName);
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (Document) result.get();
	}

	private @Nonnull Document convertDocument(@Nullable String customerName,
												@Nonnull String moduleName,
												@Nonnull Module module,
												@Nonnull String documentName,
												@Nonnull DocumentMetaData document) {
		StringBuilder metaDataName = new StringBuilder(128);
		metaDataName = new StringBuilder(64).append(moduleName).append('.').append(documentName);
		if (customerName != null) {
			metaDataName.append(" (").append(customerName).append(')');
		}
		Document result = document.convert(metaDataName.toString(), getDelegator());
		
		DocumentImpl internalResult = (DocumentImpl) result;
		internalResult.setOwningModuleName(moduleName);

		// check each document reference query name links to a module query
		for (String referenceName : result.getReferenceNames()) {
			String queryName = result.getReferenceByName(referenceName).getQueryName();
			if ((queryName != null) && (module.getMetaDataQuery(queryName) == null)) {
				StringBuilder mde = new StringBuilder(documentName);
				mde.append(" : The reference ");
				mde.append(referenceName);
				mde.append(" has a query ");
				mde.append(queryName);
				mde.append(" that does not exist in module ");
				if (customerName != null) {
					mde.append(customerName);
					mde.append(".");
				}
				mde.append(moduleName);
				
				throw new MetaDataException(mde.toString());
			}
		}

		// Add actions in privileges to the document to enable good view generation
		for (Role role : module.getRoles()) {
			for (Privilege privilege : ((RoleImpl) role).getPrivileges()) {
				if (privilege instanceof ActionPrivilege) {
					ActionPrivilege actionPrivilege = (ActionPrivilege) privilege;
					if (actionPrivilege.getDocumentName().equals(result.getName())) {
						internalResult.getDefinedActionNames().add(actionPrivilege.getName());
					}
				}
			}
		}
		
		return result;
	}
	
	@Override
	public Document putDocument(Customer customer, Module module, DocumentMetaData document) {
		String customerName = customer.getName();
		String moduleName = module.getName();
		String documentName = document.getName();
		Document result = convertDocument(customerName, moduleName, module, documentName, document);
		
		StringBuilder documentKey = new StringBuilder(64);
		documentKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/').append(MODULES_NAMESPACE).append(moduleName).append('/').append(documentName);
		cache.put(documentKey.toString(), Optional.of(result));
		
		return result;
	}

	@Override
	public Document putDocument(Module module, DocumentMetaData document) {
		String moduleName = module.getName();
		String documentName = document.getName();
		Document result = convertDocument(null, moduleName, module, documentName, document);
		
		StringBuilder documentKey = new StringBuilder(64);
		documentKey.append(MODULES_NAMESPACE).append(moduleName).append('/').append(documentName);
		cache.put(documentKey.toString(), Optional.of(result));
		
		return result;
	}

	@Override
	public View getView(String uxui,
							Customer customer, 
							Document document, 
							String name) {
		View result = null;
		if (customer != null) {
			String searchCustomerName = customer.getName();
			// get customer overridden
			if (uxui != null) {
				// get uxui specific
				result = getViewInternal(searchCustomerName, uxui, customer, document, uxui, name);
			}
			if (result == null) {
				result = getViewInternal(searchCustomerName, null, customer, document, uxui, name);
			}
		}
		
		if (result == null) { // not overridden
			if (uxui != null) {
				// get uxui specific
				result = getViewInternal(null, uxui, customer, document, uxui, name);
			}
			if (result == null) {
				result = getViewInternal(null, null, customer, document, uxui, name);
			}
		}
		
		// scaffold
		if ((result == null) && getUseScaffoldedViews()) {
			if (UtilImpl.DEV_MODE) {
				result = scaffoldView(customer, document, name, uxui);
			}
			else {
				StringBuilder key = new StringBuilder(128);
				String documentModuleName = document.getOwningModuleName();
				String documentName = document.getName();
				key.append(MODULES_NAMESPACE).append(documentModuleName).append('/');
				key.append(documentName).append('/').append(VIEWS_NAMESPACE).append(name);
				String viewKey = key.toString();
				Optional<MetaData> o = cache.get(viewKey);
				if (o == null) { // key absent
					result = scaffoldView(customer, document, name, uxui);
					if (result != null) {
						cache.putIfAbsent(viewKey, Optional.of(result));
					}
				}
			}
		}
		return result;
	}

	private @Nullable View getViewInternal(@Nullable String searchCustomerName, // the name of the customer to try to load
											@Nullable String searchUxUi, // the uxui to try to load {from getView()}
											@Nullable Customer customer,
											@Nonnull Document document,
											@Nullable String uxui, // the current uxui
											@Nonnull String viewName) {
		StringBuilder viewKeySB = new StringBuilder(128);
		if (searchCustomerName != null) {
			viewKeySB.append(CUSTOMERS_NAMESPACE).append(searchCustomerName).append('/');
		}
		viewKeySB.append(MODULES_NAMESPACE);
		String documentModuleName = document.getOwningModuleName();
		String documentName = document.getName();
		viewKeySB.append(documentModuleName).append('/').append(documentName).append('/').append(VIEWS_NAMESPACE);
		if (searchUxUi != null) {
			viewKeySB.append(searchUxUi).append('/');
		}
		viewKeySB.append(viewName);
		String viewKey = viewKeySB.toString();
		Optional<MetaData> result = cache.get(viewKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				// Load the view using the searchUxUi
				ViewMetaData viewMetaData = loadView(searchCustomerName, documentModuleName, documentName, searchUxUi, viewName);
				View view = null;
				if (viewMetaData != null) {
					// Convert the view ensuring view components within vanilla views are resolved with the current uxui
					view = convertView(searchCustomerName, searchUxUi, customer, documentModuleName, documentName, document, uxui, viewMetaData);
					if (view != null) {
						result = Optional.of(view);
						cache.put(viewKey, result);
					}
				}
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					ViewImpl view = (ViewImpl) result.get();
					// check last modified for the view
					if (view.getLastModifiedMillis() < viewLastModifiedMillis(searchCustomerName, documentModuleName, documentName, searchUxUi, viewName)) {
						// Load the view using the searchUxUi
						ViewMetaData viewMetaData = loadView(searchCustomerName, documentModuleName, documentName, searchUxUi, viewName);
						View newView = null;
						if (viewMetaData != null) {
							// Convert the view ensuring view components within vanilla views are resolved with the current uxui
							newView = convertView(searchCustomerName, searchUxUi, customer, documentModuleName, documentName, document, uxui, viewMetaData);
							if (newView != null) {
								result = Optional.of(newView);
								cache.put(viewKey, result);
								// Validate the current customer if we are in dev mode and the metadata has been touched
								DomainGenerator.validate(this, (customer == null) ? CORE.getCustomer().getName() : customer.getName());
							}
						}
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (View) result.get();
	}

	private @Nonnull ViewImpl convertView(@Nullable String searchCustomerName,
											@Nullable String searchUxUi, // only used to make the metadata name
											@Nullable Customer customer,
											@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull Document document,
											@Nullable String uxui, // the current uxui used to resolve view components
											@Nonnull ViewMetaData view) {
		StringBuilder metaDataNameSB = new StringBuilder(128);
		metaDataNameSB.append(moduleName).append('.').append(documentName).append('.');
		if (searchUxUi != null) {
			metaDataNameSB.append(searchUxUi).append('.');
		}
		metaDataNameSB.append(view.getName());
		if (searchCustomerName != null) {
			metaDataNameSB.append(" (").append(searchCustomerName).append(')');
		}
		String metaDataName = metaDataNameSB.toString();
		
		// Convert the view
		ViewImpl result = view.convert(metaDataName, getDelegator());
		result.setOverriddenCustomerName(searchCustomerName);
		result.setOverriddenUxUiName(searchUxUi);

		final Module documentModule = getModule(customer, document.getOwningModuleName());
		
		// Convert accesses in ViewMetaData to view, which requires customer/module/document context
		ViewUserAccessesMetaData accesses = view.getAccesses();
		result.convertAccesses(documentModule, documentName, metaDataName, accesses);
		
		// Resolve the view ensuring view components within vanilla views are resolved with the current uxui
		result.resolve(uxui, customer, documentModule, document, (accesses == null) ? true : accesses.isGenerate(), this);
		return result;
	}
	
	private View scaffoldView(@Nullable Customer customer, @Nonnull Document document, @Nonnull String viewName, @Nullable String uxui) {
		if (ViewType.edit.toString().equals(viewName) || 
				ViewType.pick.toString().equals(viewName) || 
				ViewType.params.toString().equals(viewName)) {
			ViewImpl result = new ViewGenerator(this).generate(customer, document, viewName);
			final Module documentModule = getModule(customer, document.getOwningModuleName());
			result.resolve(uxui, customer, documentModule, document, true, this);
			return result;
		}
		return null;
	}

	@Override
	public View putView(Customer customer, String uxui, Document document, ViewMetaData view) {
		String customerName = customer.getName();
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		View result = convertView(customerName, uxui, customer, moduleName, documentName, document, uxui, view);
		
		StringBuilder viewKey = new StringBuilder(128);
		viewKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		viewKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		viewKey.append(documentName).append('/').append(VIEWS_NAMESPACE);
		viewKey.append(uxui).append('/').append(view.getName());
		cache.put(viewKey.toString(), Optional.of(result));
		
		return result;
	}

	@Override
	public View putView(String uxui, Document document, ViewMetaData view) {
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		View result = convertView(null, uxui, null, moduleName, documentName, document, uxui, view);
		
		StringBuilder viewKey = new StringBuilder(128);
		viewKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		viewKey.append(documentName).append('/').append(VIEWS_NAMESPACE);
		viewKey.append(uxui).append('/').append(view.getName());
		cache.put(viewKey.toString(), Optional.of(result));
		
		return result;
	}
	
	@Override
	public View putView(Customer customer, Document document, ViewMetaData view) {
		String customerName = customer.getName();
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		View result = convertView(customerName, null, customer, moduleName, documentName, document, null, view);
		
		StringBuilder viewKey = new StringBuilder(128);
		viewKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		viewKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		viewKey.append(documentName).append('/');
		viewKey.append(VIEWS_NAMESPACE).append(view.getName());
		cache.put(viewKey.toString(), Optional.of(result));
		
		return result;
	}
	
	@Override
	public View putView(Document document, ViewMetaData view) {
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		View result = convertView(null, null, null, moduleName, documentName, document, null, view);
		
		StringBuilder viewKey = new StringBuilder(128);
		viewKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		viewKey.append(documentName).append('/');
		viewKey.append(VIEWS_NAMESPACE).append(view.getName());
		cache.put(viewKey.toString(), Optional.of(result));
		
		return result;
	}

	@Override
	public ActionMetaData getMetaDataAction(Customer customer, Document document, String actionName) {
		ActionMetaData result = null;
		if (customer != null) {
			// get customer overridden
			result = getMetaDataActionInternal(customer.getName(), document, actionName);
		}
		if (result == null) { // not overridden
			result = getMetaDataActionInternal(null, document, actionName);
		}
		return result;
	}

	private @Nullable ActionMetaData getMetaDataActionInternal(@Nullable String customerName,
																@Nonnull Document document,
																@Nonnull String actionName) {
		StringBuilder actionKeySB = new StringBuilder(128);
		if (customerName != null) {
			actionKeySB.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		actionKeySB.append(MODULES_NAMESPACE);
		String documentModuleName = document.getOwningModuleName();
		String documentName = document.getName();
		actionKeySB.append(documentModuleName).append('/').append(documentName).append('/').append(ACTIONS_NAMESPACE);
		actionKeySB.append(actionName).append(META_DATA_SUFFIX);
		String actionKey = actionKeySB.toString();
		Optional<MetaData> result = cache.get(actionKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				// Load the action
				ActionMetaData action = loadMetaDataAction(customerName, documentModuleName, documentName, actionName);
				if (action != null) {
					action = convertMetaDataAction(customerName, documentModuleName, documentName, action);
					if (action != null) {
						result = Optional.of(action);
						cache.put(actionKey, result);
					}
				}
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					ActionMetaData action = (ActionMetaData) result.get();
					// check last modified for the action
					if (action.getLastModifiedMillis() < metaDataActionLastModifiedMillis(customerName, documentModuleName, documentName, actionName)) {
						// Load the action
						ActionMetaData newAction = loadMetaDataAction(customerName, documentModuleName, documentName, actionName);
						if (newAction != null) {
							newAction = convertMetaDataAction(customerName, documentModuleName, documentName, newAction);
							if (newAction != null) {
								result = Optional.of(newAction);
								cache.put(actionKey, result);
							}
						}
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (ActionMetaData) result.get();
	}

	private @Nonnull ActionMetaData convertMetaDataAction(@Nullable String customerName,
															@Nonnull String moduleName,
															@Nonnull String documentName,
															@Nonnull ActionMetaData action) {
		StringBuilder metaDataNameSB = new StringBuilder(128);
		metaDataNameSB.append(moduleName).append('.').append(documentName).append('.');
		metaDataNameSB.append(action.getName());
		if (customerName != null) {
			metaDataNameSB.append(" (").append(customerName).append(')');
		}
		String metaDataName = metaDataNameSB.toString();
		
		// Convert the action
		return action.convert(metaDataName, getDelegator());
	}

	@Override
	public ActionMetaData putMetaDataAction(Customer customer, Document document, ActionMetaData action) {
		String customerName = customer.getName();
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		ActionMetaData result = convertMetaDataAction(customerName, moduleName, documentName, action);
		
		StringBuilder actionKey = new StringBuilder(128);
		actionKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		actionKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		actionKey.append(documentName).append('/');
		actionKey.append(ACTIONS_NAMESPACE).append(action.getName()).append(META_DATA_SUFFIX);
		cache.put(actionKey.toString(), Optional.of(result));
		
		return result;
	}
	
	@Override
	public ActionMetaData putMetaDataAction(Document document, ActionMetaData action) {
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		ActionMetaData result = convertMetaDataAction(null, moduleName, documentName, action);
		
		StringBuilder actionKey = new StringBuilder(128);
		actionKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		actionKey.append(documentName).append('/');
		actionKey.append(ACTIONS_NAMESPACE).append(action.getName()).append(META_DATA_SUFFIX);
		cache.put(actionKey.toString(), Optional.of(result));
		
		return result;
	}

	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		BizletMetaData result = null;
		if (customer != null) {
			// get customer overridden
			result = getMetaDataBizletInternal(customer.getName(), document);
		}
		if (result == null) { // not overridden
			result = getMetaDataBizletInternal(null, document);
		}
		return result;
	}

	private @Nullable BizletMetaData getMetaDataBizletInternal(@Nullable String customerName, @Nonnull Document document) {
		StringBuilder bizletKeySB = new StringBuilder(128);
		if (customerName != null) {
			bizletKeySB.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		bizletKeySB.append(MODULES_NAMESPACE);
		String documentModuleName = document.getOwningModuleName();
		String documentName = document.getName();
		bizletKeySB.append(documentModuleName).append('/').append(documentName).append('/');
		bizletKeySB.append(documentName).append(BIZLET_SUFFIX).append(META_DATA_SUFFIX);
		String bizletKey = bizletKeySB.toString();
		Optional<MetaData> result = cache.get(bizletKey);
		if (result != null) { // key is present
			// Load if empty
			if (result.isEmpty()) {
				// Load the bizlet
				BizletMetaData bizlet = loadMetaDataBizlet(customerName, documentModuleName, documentName);
				if (bizlet != null) {
					bizlet = convertMetaDataBizlet(customerName, documentModuleName, documentName, bizlet);
					if (bizlet != null) {
						result = Optional.of(bizlet);
						cache.put(bizletKey, result);
					}
				}
			}
			else {
				// Load if dev mode and new repository version
				if (UtilImpl.DEV_MODE) {
					BizletMetaData bizlet = (BizletMetaData) result.get();
					// check last modified for the bizlet
					if (bizlet.getLastModifiedMillis() < metaDataBizletLastModifiedMillis(customerName, documentModuleName, documentName)) {
						// Load the bizlet
						BizletMetaData newBizlet = loadMetaDataBizlet(customerName, documentModuleName, documentName);
						if (newBizlet != null) {
							newBizlet = convertMetaDataBizlet(customerName, documentModuleName, documentName, newBizlet);
							if (newBizlet != null) {
								result = Optional.of(newBizlet);
								cache.put(bizletKey, result);
							}
						}
					}
				}
			}
		}
		
		return ((result == null) || result.isEmpty()) ? null : (BizletMetaData) result.get();
	}

	private @Nonnull BizletMetaData convertMetaDataBizlet(@Nullable String customerName,
															@Nonnull String moduleName,
															@Nonnull String documentName,
															@Nonnull BizletMetaData bizlet) {
		StringBuilder metaDataNameSB = new StringBuilder(128);
		metaDataNameSB.append(moduleName).append('.').append(documentName).append('.');
		metaDataNameSB.append(documentName).append(BIZLET_SUFFIX).append(META_DATA_SUFFIX);
		if (customerName != null) {
			metaDataNameSB.append(" (").append(customerName).append(')');
		}
		String metaDataName = metaDataNameSB.toString();
		
		// Convert the bizlet
		return bizlet.convert(metaDataName, getDelegator());
	}

	@Override
	public BizletMetaData putMetaDataBizlet(Customer customer, Document document, BizletMetaData bizlet) {
		String customerName = customer.getName();
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		BizletMetaData result = convertMetaDataBizlet(customerName, moduleName, documentName, bizlet);
		
		StringBuilder bizletKey = new StringBuilder(128);
		bizletKey.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		bizletKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		bizletKey.append(documentName).append('/');
		bizletKey.append(documentName).append(BIZLET_SUFFIX).append(META_DATA_SUFFIX);
		cache.put(bizletKey.toString(), Optional.of(result));
		
		return result;
	}
	
	@Override
	public BizletMetaData putMetaDataBizlet(Document document, BizletMetaData bizlet) {
		String moduleName = document.getOwningModuleName();
		String documentName = document.getName();
		BizletMetaData result = convertMetaDataBizlet(null, moduleName, documentName, bizlet);
		
		StringBuilder bizletKey = new StringBuilder(128);
		bizletKey.append(MODULES_NAMESPACE).append(moduleName).append('/');
		bizletKey.append(documentName).append('/');
		bizletKey.append(documentName).append(BIZLET_SUFFIX).append(META_DATA_SUFFIX);
		cache.put(bizletKey.toString(), Optional.of(result));
		
		return result;
	}

	/**
	 * Called by populateKeys() implementations.
	 * @param key
	 */
	protected void addKey(@Nonnull String key) {
		cache.putIfAbsent(key, Optional.empty());
	}
	
	@Override
	public @Nullable String vtable(@Nullable String customerName, @Nonnull String key) {
		String result = null;
		if (customerName != null) {
			result = new StringBuilder(128).append(CUSTOMERS_NAMESPACE).append(customerName).append('/').append(key).toString();
			if (! cache.containsKey(result)) {
				if (cache.containsKey(key)) {
					result = key;
				}
				else {
					result = null;
				}
			}
		}
		else if (cache.containsKey(key)) {
			result = key;
		}
		return result;
	}
}
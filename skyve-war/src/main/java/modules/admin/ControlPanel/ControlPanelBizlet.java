package modules.admin.ControlPanel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.ModuleDocument;
import modules.admin.domain.Tag;

public class ControlPanelBizlet extends Bizlet<ControlPanelExtension> {
	@Override
	public ControlPanelExtension newInstance(ControlPanelExtension bean) throws Exception {
		// Set the user name to the logged in user
		UserProxyExtension user = ModulesUtil.currentAdminUserProxy();
		bean.setSailUser(user);
		bean.setSailBaseUrl(Util.getBaseUrl());
		bean.setSailTestStrategy(SailTestStrategy.None);

		// Set module name to the first non-admin module found
		ProvidedRepository r = ProvidedRepositoryFactory.get();
		for (String moduleName : r.getAllVanillaModuleNames()) {
			if (! ControlPanel.MODULE_NAME.equals(moduleName)) {
				bean.setSailModuleName(moduleName);
			}
		}

		bean.setSessionCount(Integer.valueOf(StateUtil.getSessionCount()));
		
		return bean;
	}

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		if (ControlPanel.selectedCachePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();

			String cacheName = UtilImpl.CONVERSATION_CACHE.getName();
			result.add(new DomainValue(cacheName, "Conversations"));
			cacheName = UtilImpl.CSRF_TOKEN_CACHE.getName();
			result.add(new DomainValue(cacheName, "CSRF Tokens"));
			cacheName = UtilImpl.SESSION_CACHE.getName();
			result.add(new DomainValue(cacheName, "Sessions"));
			cacheName = UtilImpl.GEO_IP_CACHE.getName();
			result.add(new DomainValue(cacheName, "GeoIPs"));
			for (HibernateCacheConfig c : UtilImpl.HIBERNATE_CACHES) {
				cacheName = c.getName();
				result.add(new DomainValue(cacheName, cacheName + " (Hibernate)"));
			}
			for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
				cacheName = c.getName();
				result.add(new DomainValue(cacheName, cacheName + " (Application)"));
			}
			
			return result;
		}
		
		else if (ControlPanel.testModuleNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
			return result;
		}
		
		return null;
	}
	
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ControlPanelExtension bean) throws Exception {

		// list documents within modules that have not already been selected
		if (ControlPanel.testDocumentNamesPropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> results = new ArrayList<>();
			if (bean.getTestModuleName() != null) {
				Module module = customer.getModule(bean.getTestModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					if (document.isPersistable()) {
						
						// check this is not already selected
						boolean alreadySelected = false;
						for (ModuleDocument n : bean.getTestDocumentNames()) {
							if(documentName.equals(n.getDocumentName())){
								alreadySelected = true;
								break;
							}
						}
						
						if(!alreadySelected) {
							// only add persistent documents
							results.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
						}
					}
				}

				// sort the list by description in case the singular alias changes the sort order
				results.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
			}
			return results;
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}
	
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ControlPanel.customerNameToSwapToPropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();
			ProvidedRepository rep = ProvidedRepositoryFactory.get();
			for (String cus : rep.getAllCustomerNames()) {
				result.add(new DomainValue(cus));
			}
			Collections.sort(result, new ModulesUtil.DomainValueSortByDescription());
			return result;
		}
		else if (ControlPanel.sailModuleNamePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();
			ProvidedRepository r = ProvidedRepositoryFactory.get();
			for (String moduleName : r.getAllVanillaModuleNames()) {
					result.add(new DomainValue(moduleName));
			}
			return result;
		}
		else if (ControlPanel.sailUxUiPropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();
			Router r = CORE.getRepository().getRouter();
			for (UxUiMetadata uxui : r.getUxUis()) {
				result.add(new DomainValue(uxui.getName()));
			}
			return result;
		}

		return null;
	}

	@Override
	public List<String> complete(String attributeName, String value, ControlPanelExtension bean) throws Exception {
		
		if(ControlPanel.testTagNamePropertyName.equals(attributeName)) {
			return ModulesUtil.getCompleteSuggestions(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, Tag.namePropertyName,value);
		}
		
		return super.complete(attributeName, value, bean);
	}
	
	@Override
	public void preRerender(String source, ControlPanelExtension bean, WebContext webContext) throws Exception {
		if ("push".equals(source)) {
			// clear list of selected documents
			bean.getTestDocumentNames().clear();
		}
		super.preRerender(source, bean, webContext);
	}
}

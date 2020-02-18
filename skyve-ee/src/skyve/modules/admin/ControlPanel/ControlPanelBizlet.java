package modules.admin.ControlPanel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import modules.admin.ModulesUtil;
import modules.admin.ModulesUtil.DomainValueSortByCode;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.UserProxy;

public class ControlPanelBizlet extends Bizlet<ControlPanelExtension> {
	private static final long serialVersionUID = -6033906392152210002L;

	@Override
	public ControlPanelExtension newInstance(ControlPanelExtension bean) throws Exception {
		// Set the user name to the logged in user
		UserProxy user = ModulesUtil.currentAdminUserProxy();
		bean.setSailUser(user);
		bean.setSailBaseUrl(Util.getSkyveContextUrl() + '/');
		bean.setSailTestStrategy(SailTestStrategy.None);

		// Set module name to the first non-admin module found
		AbstractRepository r = AbstractRepository.get();
		for (String moduleName : r.getAllVanillaModuleNames()) {
			if (! ControlPanel.MODULE_NAME.equals(moduleName)) {
				bean.setSailModuleName(moduleName);
			}
		}
		
		bean.loadStartupConfiguration();

		return bean;
	}

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		if (ControlPanel.selectedCachePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();

			String cacheName = UtilImpl.CONVERSATION_CACHE.getName();
			result.add(new DomainValue(cacheName, "Conversations"));
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
		
		return null;
	}
	
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ControlPanel.designModuleDocumentNamePropertyName.equals(attributeName)) {
			Customer c = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			for (Module m : c.getModules()) {
				for (String d : m.getDocumentRefs().keySet()) {
					result.add(new DomainValue(m.getName() + '.' + d));
				}
			}
			Collections.sort(result, new DomainValueSortByCode());
			return result;
		}
		else if (ControlPanel.customerNameToSwapToPropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();
			AbstractRepository rep = AbstractRepository.get();
			for (String cus : rep.getAllCustomerNames()) {
				result.add(new DomainValue(cus));
			}
			Collections.sort(result, new ModulesUtil.DomainValueSortByDescription());
			return result;
		}
		else if (ControlPanel.sailModuleNamePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>();
			AbstractRepository r = AbstractRepository.get();
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
}

package org.skyve.impl.metadata.repository;

import java.util.Collections;
import java.util.List;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;

/**
 * Extend or use this repository for dynamic domains where there is no on-demand loading.
 * This implementation does not have a thread safe cache and mutations should happen before the repository is placed in the chain.
 * Use this implementation when preloading all metadata on startup.
 * @author mike
 */
public class UnsynchronisedDynamicRepository extends AbstractDynamicRepository {
	@Override
	public void populateKeys() {
		// nothing to do as metadata will be added programmatically
	}

	@Override
	public Router loadRouter() {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long routerLastModifiedMillis() {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	@Override
	public CustomerMetaData loadCustomer(String customerName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long customerLastModifiedMillis(String customerName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	@Override
	public ModuleMetaData loadModule(String customerName, String moduleName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long moduleLastModifiedMillis(String customerName, String moduleName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	@Override
	public DocumentMetaData loadDocument(String customerName, String moduleName, String documentName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long documentLastModifiedMillis(String customerName, String moduleName, String documentName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	@Override
	public ViewMetaData loadView(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long viewLastModifiedMillis(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	@Override
	public ActionMetaData loadMetaDataAction(String customerName, String moduleName, String documentName, String actionName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long metaDataActionLastModifiedMillis(String customerName, String moduleName, String documentName, String actionName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}

	@Override
	public BizletMetaData loadMetaDataBizlet(String customerName, String moduleName, String documentName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public long metaDataBizletLastModifiedMillis(String customerName, String moduleName, String documentName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}

	@Override
	public List<String> getAllCustomerNames() {
		// TODO should I find the customer cache keys?
		return Collections.emptyList();
	}

	@Override
	public List<String> getAllVanillaModuleNames() {
		// TODO should I find the vanilla module cache keys?
		return Collections.emptyList();
	}

	@Override
	public boolean getUseScaffoldedViews() {
		return false;
	}
}

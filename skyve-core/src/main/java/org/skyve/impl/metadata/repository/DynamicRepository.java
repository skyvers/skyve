package org.skyve.impl.metadata.repository;

import java.util.Collections;
import java.util.List;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;

/**
 * Extend or use this repository for dynamic domains where there is no on-demand loading.
 * @author mike
 *
 */
public class DynamicRepository extends AbstractDynamicOnDemandRepository {
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
	public CustomerMetaData loadCustomer(String customerName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public ModuleMetaData loadModule(String customerName, String moduleName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public DocumentMetaData loadDocument(String customerName, String moduleName, String documentName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	@Override
	public ViewMetaData loadView(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		// nothing to do as metadata will be added programmatically
		return null;
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

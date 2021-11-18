package org.skyve.impl.metadata.repository;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.MutableRepository;
import org.skyve.metadata.view.View;

public abstract class MutableCachedRepository extends CachedRepository implements MutableRepository {
	@Override
	public Router setRouter(Router router) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Customer addCustomer(CustomerMetaData customer) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Module addModule(Customer customer, ModuleMetaData module) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Module addModule(ModuleMetaData module) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Document addDocument(Customer customer, Module module, DocumentMetaData document) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Document addDocument(Module module, DocumentMetaData document) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public View addView(String uxui, Customer customer, Document document, ViewMetaData view) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public View addView(String uxui, Document document, ViewMetaData view) {
		// TODO Auto-generated method stub
		return null;
	}
}

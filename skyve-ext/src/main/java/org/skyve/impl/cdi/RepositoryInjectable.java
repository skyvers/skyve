package org.skyve.impl.cdi;

import java.io.File;
import java.io.Serializable;

import javax.enterprise.inject.Alternative;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class RepositoryInjectable implements Repository, Serializable {
	private static final long serialVersionUID = -2932762525201765101L;

	@Override
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		return CORE.getRepository().findResourceFile(resourcePath, customerName, moduleName);
	}

	@Override
	public Router getRouter() {
		return CORE.getRepository().getRouter();
	}

	@Override
	public Customer getCustomer(String customerName) {
		return CORE.getRepository().getCustomer(customerName);
	}

	@Override
	public Object getDataFactory(Customer customer, Document document) {
		return CORE.getRepository().getDataFactory(customer, document);
	}
	
	@Override
	public User retrieveUser(String userName) {
		return CORE.getRepository().retrieveUser(userName);
	}
}

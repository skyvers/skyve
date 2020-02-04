package org.skyve.impl.cdi;

import java.util.Map;

import javax.enterprise.inject.Produces;
import javax.inject.Singleton;

import org.skyve.addin.AddInManager;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

@Singleton
public class SkyveCDIProducer {
	@Produces
	public static Persistence getPersistence() {
		return new PersistenceInjectable();
	}

	@Produces
	public static User getUser() {
		return new UserInjectable();
	}

	@Produces
	public static Customer getCustomer() {
		return new CustomerInjectable();
	}

	@Produces
	public static Map<String, Object> getStash() {
		return new StashInjectable();
	}
	
	@Produces
	public static Repository getRepository() {
		return new RepositoryInjectable();
	}
	
	@Produces
	public static AddInManager getAddInManager() {
		return new AddInManagerInjectable();
	}
}

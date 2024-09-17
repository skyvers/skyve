package org.skyve.impl.cdi;

import java.io.Serializable;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.addin.AddInManager;
import org.skyve.cache.Caching;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.report.Reporting;
import org.skyve.util.GeoIPService;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.inject.Produces;

@ApplicationScoped
public class SkyveCDIProducer implements Serializable {
	private static final long serialVersionUID = -2629541397565705456L;

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
		return CORE.getStash();
	}
	
	@Produces
	public static Repository getRepository() {
		return new RepositoryInjectable();
	}
	
	@Produces
	public static AddInManager getAddInManager() {
		return new AddInManagerInjectable();
	}

	@Produces
	public static Reporting getReporting() {
		return new ReportingInjectable();
	}

	@Produces
	public static Caching getCaching() {
		return new CachingInjectable();
	}

	@Produces
	public static JobScheduler getJobScheduler() {
		return new JobSchedulerInjectable();
	}

	@Produces
	public static NumberGenerator getNumberGenerator() {
		return new NumberGeneratorInjectable();
	}

	@Produces
	public static GeoIPService getGeoIPService() {
		return new GeoIPServiceInjectable();
	}
}

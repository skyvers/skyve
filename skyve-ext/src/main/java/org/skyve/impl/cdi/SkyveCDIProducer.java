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
import org.skyve.util.MailService;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.inject.Produces;

/**
 * Central CDI producer for Skyve injectable proxies and shared request/application state.
 * <p>
 * Each producer method returns either:
 * <br>
 * - a serializable proxy that delegates to the current Skyve runtime service, or
 * <br>
 * - a directly exposed shared value (for example, the current stash map).
 * </p>
 * This keeps CDI injection points stable while allowing Skyve's static runtime service
 * lookup and lifecycle management to remain the single source of truth.
 */
@ApplicationScoped
public class SkyveCDIProducer implements Serializable {
	private static final long serialVersionUID = -2629541397565705456L;

	/**
	 * Produces the persistence proxy for CDI injection.
	 *
	 * @return A serializable {@link Persistence} proxy.
	 */
	@Produces
	public static Persistence getPersistence() {
		return new PersistenceInjectable();
	}

	/**
	 * Produces the current user proxy for CDI injection.
	 *
	 * @return A serializable {@link User} proxy.
	 */
	@Produces
	public static User getUser() {
		return new UserInjectable();
	}

	/**
	 * Produces the current customer proxy for CDI injection.
	 *
	 * @return A serializable {@link Customer} proxy.
	 */
	@Produces
	public static Customer getCustomer() {
		return new CustomerInjectable();
	}

	/**
	 * Produces the current Skyve stash map for CDI injection.
	 *
	 * @return The current request/thread stash map.
	 */
	@Produces
	public static Map<String, Object> getStash() {
		return CORE.getStash();
	}
	
	/**
	 * Produces the metadata repository proxy for CDI injection.
	 *
	 * @return A serializable {@link Repository} proxy.
	 */
	@Produces
	public static Repository getRepository() {
		return new RepositoryInjectable();
	}
	
	/**
	 * Produces the add-in manager proxy for CDI injection.
	 *
	 * @return A serializable {@link AddInManager} proxy.
	 */
	@Produces
	public static AddInManager getAddInManager() {
		return new AddInManagerInjectable();
	}

	/**
	 * Produces the reporting service proxy for CDI injection.
	 *
	 * @return A serializable {@link Reporting} proxy.
	 */
	@Produces
	public static Reporting getReporting() {
		return new ReportingInjectable();
	}

	/**
	 * Produces the cache manager proxy for CDI injection.
	 *
	 * @return A serializable {@link Caching} proxy.
	 */
	@Produces
	public static Caching getCaching() {
		return new CachingInjectable();
	}

	/**
	 * Produces the job scheduler proxy for CDI injection.
	 *
	 * @return A serializable {@link JobScheduler} proxy.
	 */
	@Produces
	public static JobScheduler getJobScheduler() {
		return new JobSchedulerInjectable();
	}

	/**
	 * Produces the number generator proxy for CDI injection.
	 *
	 * @return A serializable {@link NumberGenerator} proxy.
	 */
	@Produces
	public static NumberGenerator getNumberGenerator() {
		return new NumberGeneratorInjectable();
	}

	/**
	 * Produces the GeoIP service proxy for CDI injection.
	 *
	 * @return A serializable {@link GeoIPService} proxy.
	 */
	@Produces
	public static GeoIPService getGeoIPService() {
		return new GeoIPServiceInjectable();
	}

	/**
	 * Produces the mail service proxy for CDI injection.
	 * <p>
	 * This proxy delegates through {@code EXT.getMailService()}, preserving global mail
	 * pre-processing (test recipient override, bogus-send handling, and sender fallback).
	 * </p>
	 *
	 * @return A serializable {@link MailService} proxy.
	 */
	@Produces
	public static MailService getMailService() {
		return new MailServiceInjectable();
	}
}

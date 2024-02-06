package org.skyve.impl.metadata.repository;

import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

public abstract class ProvidedRepositoryFactory implements ProvidedRepository {
	private static ProvidedRepository repository;
	private static final ConcurrentHashMap<Long, ProvidedRepository> THREAD_MAP = new ConcurrentHashMap<>();
	
	/**
	 * Default constructor
	 * Prevent external instantiation.
	 */
	protected ProvidedRepositoryFactory() {
		// nothing to do here
	}

	public static ProvidedRepository get() {
		ProvidedRepository result = THREAD_MAP.get(Long.valueOf(Thread.currentThread().getId()));
		if (result == null) {
			result = repository;
		}
		return result;
	}

	public static void set(@Nonnull ProvidedRepository repository) {
		ProvidedRepositoryFactory.repository = repository;
	}

	public void setForThread() {
		THREAD_MAP.put(Long.valueOf(Thread.currentThread().getId()), this);
	}
	
	public static void removeForThread() {
		THREAD_MAP.remove(Long.valueOf(Thread.currentThread().getId()));
	}

	/**
	 * Return a UserImpl with the customerName and name properties set from the user principal given.
	 */
	public static @Nullable UserImpl setCustomerAndUserFromPrincipal(@Nullable String userPrincipal) {
		UserImpl result = null;
		if (userPrincipal != null) {
			result = new UserImpl();

			// There are 3 login situations 
			// 1. Java EE web login is used and a username which includes the customer is received eg "bizhub/mike".
			// 2. Java EE web login is used but the "CUSTOMER" parameter is set in web.xml - every login should be for this customer.
			// 3. Single Sign On (SPNEGO/KERBEROS) is used - every login is the same as the network name eg "sandsm.bizhub.com.au" and "CUSTOMER" parameter is set in web.xml
			int slashIndex = userPrincipal.indexOf('/');
			if (slashIndex >= 0) {
				String customerName = userPrincipal.substring(0, slashIndex);
				String userName = userPrincipal.substring(slashIndex + 1);
				result.setName(userName);
				result.setCustomerName(customerName);
			}
			else {
				result.setName(userPrincipal);
			}
			if (UtilImpl.CUSTOMER != null) {
				result.setCustomerName(UtilImpl.CUSTOMER);
			}
		}
		
		return result;
	}
}

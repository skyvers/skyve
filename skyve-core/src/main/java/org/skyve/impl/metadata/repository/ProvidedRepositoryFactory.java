package org.skyve.impl.metadata.repository;

import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Static factory that provides access to the singleton {@link ProvidedRepository}.
 *
 * <p>The framework installs exactly one {@code ProvidedRepository} instance for the
 * lifetime of the application.  Callers obtain it via
 * {@link org.skyve.impl.metadata.repository.ProvidedRepositoryFactory#get()} rather than
 * constructing one directly.  The factory hides the concrete implementation from
 * framework code that only needs the repository contract.
 *
 * <p>Threading: the singleton reference is written once during bootstrap and read
 * without synchronisation thereafter; the write must complete-before-read (e.g. via
 * application server startup guarantees) to avoid a data race.
 *
 * @see ProvidedRepository
 */
public abstract class ProvidedRepositoryFactory implements ProvidedRepository {
	private static ProvidedRepository repository;
	
	/**
	 * Default constructor
	 * Prevent external instantiation.
	 */
	protected ProvidedRepositoryFactory() {
		// nothing to do here
	}

	/**
	 * Get the default repository or the session repository if set.
	 */
	public static @Nonnull ProvidedRepository get() {
		return repository;
	}

	/**]
	 * Set the default repository.
	 */
	public static void set(@Nonnull ProvidedRepository repository) {
		ProvidedRepositoryFactory.repository = repository;
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

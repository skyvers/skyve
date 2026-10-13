package org.skyve.impl.cdi;

import java.io.File;
import java.io.Serializable;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link Repository}.
 *
 * <p>Delegates to {@link CORE#getRepository()} so repository access keeps working
 * after CDI passivation without serializing the real repository implementation.
 */
@Alternative
public class RepositoryInjectable implements Repository, Serializable {
	private static final long serialVersionUID = -2932762525201765101L;

	/**
	 * Resolves a repository resource for the supplied customer/module context.
	 *
	 * @param resourcePath the repository-relative resource path.
	 * @param customerName the customer name used for lookup.
	 * @param moduleName the optional module name used for lookup.
	 * @return the resolved resource file, or null if no resource matches.
	 */
	@Override
	public @Nullable File findResourceFile(@Nonnull String resourcePath, @Nullable String customerName, @Nullable String moduleName) {
		return CORE.getRepository().findResourceFile(resourcePath, customerName, moduleName);
	}

	/**
	 * Returns the router used to evaluate navigation/access rules.
	 *
	 * @return the repository router.
	 */
	@Override
	public @Nullable Router getRouter() {
		return CORE.getRepository().getRouter();
	}

	/**
	 * Returns customer metadata resolved by customer name.
	 *
	 * @param customerName the customer name.
	 * @return the resolved customer metadata, or null if not found.
	 */
	@Override
	public @Nullable Customer getCustomer(@Nonnull String customerName) {
		return CORE.getRepository().getCustomer(customerName);
	}

	/**
	 * Returns the configured data factory for the given document.
	 *
	 * @param customer the owning customer metadata.
	 * @param document the target document metadata.
	 * @return the configured data factory instance, or null if none is defined.
	 */
	@Override
	public @Nullable Object getDataFactory(@Nullable Customer customer, @Nonnull Document document) {
		return CORE.getRepository().getDataFactory(customer, document);
	}
	
	/**
	 * Retrieves a user definition by username from repository-backed metadata.
	 *
	 * @param userName the username to resolve.
	 * @return the resolved user, or null if no user matches.
	 */
	@Override
	public @Nullable User retrieveUser(@Nonnull String userName) {
		return CORE.getRepository().retrieveUser(userName);
	}
}

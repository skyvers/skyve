package org.skyve.metadata.repository;

import java.io.File;

import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The primary application-level interface for the Skyve metadata repository.
 *
 * <p>A {@code Repository} provides the basic operations needed at runtime: resolving
 * resource files, obtaining the request router, loading customer configurations, and
 * retrieving users. Higher-level metadata lookup is provided by the
 * {@link ProvidedRepository} extension.
 *
 * <p>The singleton instance is obtained via {@link org.skyve.CORE#getRepository()}.
 *
 * @see ProvidedRepository
 * @see CachedRepository
 */
public interface Repository {
	/**
	 * Resolves a resource file by searching customer, module, and global resource
	 * folders in priority order.
	 *
	 * <p>Search order: customer+module folder, vanilla module folder, customer images
	 * folder, global images folder.
	 *
	 * @param resourcePath  the relative path to the resource; must not be {@code null}
	 * @param customerName  the customer name to scope the search; may be {@code null}
	 * @param moduleName    the module name to scope the search; may be {@code null}
	 * @return the resolved resource {@link File}; never {@code null}
	 */
	@Nonnull File findResourceFile(@Nonnull String resourcePath, @Nullable String customerName, @Nullable String moduleName);
	
	/**
	 * Returns the merged router combining the global router and all module-level routers.
	 *
	 * @return the active merged router; never {@code null}
	 */
	@Nonnull Router getRouter();

	/**
	 * Returns the customer configuration for the given customer name, or {@code null}
	 * if no such customer is registered.
	 *
	 * @param customerName  the customer name to resolve; must not be {@code null}
	 * @return the customer, or {@code null}
	 */
	@Nullable Customer getCustomer(@Nonnull String customerName);

	/**
	 * Returns the data factory (test fixture builder) for the given document within a
	 * customer context, or {@code null} if none is registered.
	 *
	 * @param customer      the customer context; may be {@code null}
	 * @param document      the document to look up the factory for; must not be {@code null}
	 * @return the data factory instance, or {@code null}
	 */
	@Nullable Object getDataFactory(@Nullable Customer customer, @Nonnull Document document);
	
	/**
	 * Convenience overload that resolves the {@link Document} from a module and document
	 * name before delegating to {@link #getDataFactory(Customer, Document)}.
	 *
	 * @param customer    the customer context; must not be {@code null}
	 * @param moduleName  the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @return the data factory instance, or {@code null}
	 */
	default @Nullable Object getDataFactory(@Nonnull Customer customer, @Nonnull String moduleName, @Nonnull String documentName) {
		Module m = customer.getModule(moduleName);
		Document d = m.getDocument(customer, documentName);
		return getDataFactory(customer, d);
	}
	
	/**
	 * Retrieves the user record for the given username from the data store.
	 *
	 * @param userName  the username to look up; must not be {@code null}
	 * @return the user, or {@code null} if not found
	 */
	@Nullable User retrieveUser(@Nonnull String userName);
}

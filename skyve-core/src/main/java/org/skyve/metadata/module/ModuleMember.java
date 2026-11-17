package org.skyve.metadata.module;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a piece of metadata declared in a module - the common parent of
 * the typed references ({@link org.skyve.metadata.user.ModuleRole},
 * {@link org.skyve.metadata.model.document.ModuleDocument},
 * {@link org.skyve.metadata.module.query.ModuleQuery},
 * {@link org.skyve.metadata.module.query.ModuleBizQL},
 * {@link org.skyve.metadata.module.query.ModuleSQL} and {@link ModuleJob}) that the generated
 * per-module enums implement.
 * <p/>
 * Typed references are a compile-checked shortcut for metadata names known when the
 * application is built. Every Skyve API that accepts one also accepts the module name and
 * metadata name as Strings, which remain the primitive form for dynamic domains, customer
 * overrides and names held as data.
 */
public interface ModuleMember {
	/**
	 * The name of the module that declares (or references) the metadata.
	 *
	 * @return the module name
	 */
	@Nonnull String moduleName();

	/**
	 * Resolves the declaring module through the current user's customer, so per-customer module
	 * overrides are honoured.
	 *
	 * @return the module metadata
	 */
	default @Nonnull Module toModule() {
		return toModule(CORE.getCustomer());
	}

	/**
	 * Resolves the declaring module through the given customer, so per-customer module overrides
	 * are honoured.
	 *
	 * @param customer	the customer to resolve the module for
	 * @return the module metadata
	 */
	default @Nonnull Module toModule(@Nonnull Customer customer) {
		return customer.getModule(moduleName());
	}
}

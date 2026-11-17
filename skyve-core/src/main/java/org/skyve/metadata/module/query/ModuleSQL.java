package org.skyve.metadata.module.query;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.ModuleMember;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a SQL query declared in a module - implemented by the generated
 * per-module SQL enums (e.g. <code>AdminSQL.X</code>) so Skyve APIs can accept a single
 * self-describing token in place of a module name and query name String pair.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleSQL extends ModuleMember {
	/**
	 * The query name within the module.
	 *
	 * @return the query name
	 */
	@Nonnull String queryName();

	/**
	 * Resolves this reference to the SQL query metadata through the current user's customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @return the SQL query metadata
	 * @throws MetaDataException if the module declares no such SQL query for the customer
	 */
	default @Nonnull SQLDefinition toSQL() {
		return toSQL(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the SQL query metadata through the given customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @param customer	the customer to resolve the query for
	 * @return the SQL query metadata
	 * @throws MetaDataException if the module declares no such SQL query for the customer
	 */
	default @Nonnull SQLDefinition toSQL(@Nonnull Customer customer) {
		Module module = toModule(customer);
		String queryName = queryName();
		for (QueryDefinition query : module.getMetadataQueries()) {
			if ((query instanceof SQLDefinition result) && queryName.equals(query.getName())) {
				return result;
			}
		}
		throw new MetaDataException("SQL query " + queryName + " does not exist in module " + module.getName());
	}
}

package org.skyve.metadata.module.query;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.ModuleMember;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a metadata (document) query declared in a module - implemented by
 * the generated per-module query enums (e.g. <code>AdminQuery.Q_CONTACTS</code>) so Skyve APIs
 * can accept a single self-describing token in place of a module name and query name String pair.
 * BizQL and SQL module queries have their own references, {@link ModuleBizQL} and
 * {@link ModuleSQL}.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleQuery extends ModuleMember {
	/**
	 * The query name within the module.
	 *
	 * @return the query name
	 */
	@Nonnull String queryName();

	/**
	 * Resolves this reference to the query metadata through the current user's customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @return the query metadata
	 * @throws org.skyve.metadata.MetaDataException if the module declares no such metadata query
	 * 				for the customer
	 */
	default @Nonnull MetaDataQueryDefinition toQuery() {
		return toQuery(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the query metadata through the given customer, so per-customer
	 * module overrides are honoured.
	 *
	 * @param customer	the customer to resolve the query for
	 * @return the query metadata
	 * @throws org.skyve.metadata.MetaDataException if the module declares no such metadata query
	 * 				for the customer
	 */
	default @Nonnull MetaDataQueryDefinition toQuery(@Nonnull Customer customer) {
		return toModule(customer).getNullSafeMetaDataQuery(queryName());
	}
}

package org.skyve.metadata.module.query;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.ModuleMember;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a BizQL query declared in a module - implemented by the generated
 * per-module BizQL enums (e.g. <code>AdminBizQL.X</code>) so Skyve APIs can accept a single
 * self-describing token in place of a module name and query name String pair.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleBizQL extends ModuleMember {
	/**
	 * The query name within the module.
	 *
	 * @return the query name
	 */
	@Nonnull String queryName();

	/**
	 * Resolves this reference to the BizQL query metadata through the current user's customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @return the BizQL query metadata
	 * @throws MetaDataException if the module declares no such BizQL query for the customer
	 */
	default @Nonnull BizQLDefinition toBizQL() {
		return toBizQL(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the BizQL query metadata through the given customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @param customer	the customer to resolve the query for
	 * @return the BizQL query metadata
	 * @throws MetaDataException if the module declares no such BizQL query for the customer
	 */
	default @Nonnull BizQLDefinition toBizQL(@Nonnull Customer customer) {
		Module module = toModule(customer);
		String queryName = queryName();
		for (QueryDefinition query : module.getMetadataQueries()) {
			if ((query instanceof BizQLDefinition result) && queryName.equals(query.getName())) {
				return result;
			}
		}
		throw new MetaDataException("BizQL query " + queryName + " does not exist in module " + module.getName());
	}
}

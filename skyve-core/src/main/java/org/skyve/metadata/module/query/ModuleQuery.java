package org.skyve.metadata.module.query;

import org.skyve.CORE;

/**
 * A compile-time reference to a metadata query declared in a module - implemented by the
 * generated per-module query enums (e.g. <code>AdminQuery.Q_CONTACTS</code>) so Skyve APIs can
 * accept a single self-describing token in place of a module name and query name String pair.
 */
public interface ModuleQuery {
	/**
	 * The name of the module that declares the query.
	 *
	 * @return the module name; never {@code null}
	 */
	String moduleName();

	/**
	 * The query name within the module.
	 *
	 * @return the query name; never {@code null}
	 */
	String queryName();

	/**
	 * Resolves this reference to the query metadata through the current customer,
	 * so per-customer module overrides are honoured.
	 *
	 * @return the query metadata; never {@code null}
	 */
	default MetaDataQueryDefinition toQuery() {
		return CORE.getCustomer().getModule(moduleName()).getNullSafeMetaDataQuery(queryName());
	}
}

/**
 * Customer-level metadata contracts defining multi-tenancy, theming, and extension points.
 *
 * <p>In Skyve, a <em>customer</em> represents a distinct tenant in a multi-tenant deployment.
 * Each customer has its own set of modules, roles, converters, UI resources, and lifecycle
 * hooks. The {@link org.skyve.metadata.customer.Customer} interface is the root of this
 * package and is loaded by the repository at startup for each declared customer.
 *
 * <h2>Key interfaces</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.customer.Customer} — the top-level tenant descriptor.
 *       Provides access to modules, roles, date converters, interceptors, observers, and
 *       UI/HTML/login resource overrides.</li>
 *   <li>{@link org.skyve.metadata.customer.CustomerRole} — an aggregate security role
 *       defined at the customer level, combining module-level roles for assignment to users.</li>
 *   <li>{@link org.skyve.metadata.customer.InterceptorMetaData} — metadata that holds the
 *       class name of a registered {@link org.skyve.metadata.controller.Interceptor} and
 *       provides a handle to the singleton instance.</li>
 *   <li>{@link org.skyve.metadata.customer.ObserverMetaData} — metadata that holds the
 *       class name of a registered {@link org.skyve.metadata.controller.Observer} and
 *       provides a handle to the singleton instance.</li>
 * </ul>
 *
 * <h2>Resource customisation</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.customer.UIResources} — customer-specific logo path.</li>
 *   <li>{@link org.skyve.metadata.customer.HTMLResources} — customer-specific CSS path.</li>
 *   <li>{@link org.skyve.metadata.customer.LoginResources} — customer-specific login and
 *       logged-out page URLs.</li>
 * </ul>
 *
 * <h2>Multi-tenancy model</h2>
 * Each Skyve deployment can host one or more customers. The repository resolves
 * customer-specific metadata overrides first, falling back to the base module/document
 * metadata. This package provides the contracts for the customer tier of that resolution chain.
 */
package org.skyve.metadata.customer;

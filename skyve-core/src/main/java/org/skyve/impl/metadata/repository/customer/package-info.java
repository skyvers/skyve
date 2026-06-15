/**
 * JAXB-annotated metadata classes for deserialising customer XML configuration files.
 *
 * <p>A Skyve customer is configured by a {@code customer.xml} file in the
 * {@code customers/<customerName>/} directory. The classes in this package form the
 * JAXB binding tree for that file:
 * <ul>
 *   <li>{@code CustomerMetaData} — JAXB root; maps to {@code <customer>}; holds module
 *       list, roles, interceptors, observers, UI resources, and login settings.
 *   <li>{@code CustomerModulesMetaData} — container for the customer's module list.
 *   <li>{@code CustomerModuleMetaData} — one module entry; carries name and document
 *       override list.
 *   <li>{@code CustomerModuleRoleMetaData} — role definition within a module entry.
 *   <li>{@code CustomerRolesMetaData} — container for cross-module role definitions.
 *   <li>{@code CustomerRoleMetaData} / {@code CustomerFeatureRoleMetaData} — role
 *       references assembled into the customer's permission model.
 *   <li>{@code InterceptorMetaDataImpl} — declares a Skyve
 *       {@link org.skyve.metadata.controller.ServerSideActionResult} interceptor class.
 *   <li>{@code ObserverMetaDataImpl} — declares a Skyve lifecycle observer.
 *   <li>{@code HTMLResourcesMetaData} / {@code LoginResourcesMetaData} /
 *       {@code UIResources} — UI and login customisation resource pointers.
 * </ul>
 *
 * @see org.skyve.impl.metadata.customer
 * @see org.skyve.metadata.customer.Customer
 */
package org.skyve.impl.metadata.repository.customer;

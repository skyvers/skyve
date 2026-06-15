/**
 * Internal implementation of the {@link org.skyve.metadata.customer.Customer} contract.
 *
 * <p>Contains:
 * <ul>
 *   <li>{@code CustomerImpl} — the runtime customer configuration object, populated
 *       from the customer XML file. Holds module overrides, UI settings, login
 *       configuration, and add-in registrations for a named customer (tenant).
 *   <li>{@code ExportedReference} — value object that records a cross-module document
 *       reference exported by a customer override, used during repository validation.
 * </ul>
 *
 * @see org.skyve.metadata.customer.Customer
 * @see org.skyve.impl.metadata.repository.customer
 */
package org.skyve.impl.metadata.customer;

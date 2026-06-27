/**
 * Internal implementations of the {@link org.skyve.metadata.controller} customisation
 * contracts.
 *
 * <p>Contains:
 * <ul>
 *   <li>{@code CustomisationsStaticSingleton} — startup holder for the single active
 *       {@link org.skyve.metadata.controller.Customisations} implementation; set once
 *       during application bootstrap via the customer configuration.
 *   <li>{@code NoCustomisations} — no-op implementation returned when a customer has
 *       not provided a custom {@code Customisations} class; every method returns the
 *       default/null value.
 * </ul>
 *
 * @see org.skyve.metadata.controller.Customisations
 */
package org.skyve.impl.metadata.controller;

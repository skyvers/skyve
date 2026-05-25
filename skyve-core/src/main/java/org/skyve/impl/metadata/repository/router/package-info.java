/**
 * JAXB-annotated metadata classes for deserialising the Skyve router XML configuration.
 *
 * <p>The router determines which UX/UI template set is used for a given HTTP request.
 * The classes in this package form the JAXB binding layer:
 * <ul>
 *   <li>{@code Router} — JAXB root; holds an ordered list of {@code Route} entries and
 *       the default UX/UI name.
 *   <li>{@code Route} — a single routing rule mapping an outcome to a UX/UI name and
 *       optional redirect URL.
 *   <li>{@code RouteCriteria} — the matching criteria for a route (device type, path
 *       pattern, etc.).
 *   <li>{@code UxUiMetadata} — describes a named UX/UI configuration (template
 *       directory, theme, SmartClient skin, etc.).
 *   <li>{@code TaggingUxUiSelector} — internal base interface for the
 *       {@link org.skyve.metadata.router.UxUiSelector} SPI; carries tag-based selection
 *       helpers.
 * </ul>
 *
 * @see org.skyve.metadata.router
 * @see org.skyve.metadata.router.UxUiSelector
 */
package org.skyve.impl.metadata.repository.router;

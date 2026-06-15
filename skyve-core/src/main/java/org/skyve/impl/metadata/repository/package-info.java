/**
 * Repository infrastructure: abstract dynamic repository and metadata loading
 * contracts for the Skyve metadata layer.
 *
 * <p>This package provides the core repository abstractions used by the framework to
 * load and cache module, document, customer, view, and router metadata:
 * <ul>
 *   <li>{@code AbstractDynamicRepository} — base class for the default XML-file-based
 *       repository and any dynamic (runtime-editable) repository implementations.
 *   <li>{@code UnsynchronisedDynamicRepository} — non-synchronized variant for use in
 *       read-only or test scenarios.
 *   <li>{@code ConvertibleMetaData} — shared base for metadata objects that carry an
 *       optional {@link org.skyve.domain.types.converters.Converter} declaration.
 *   <li>{@code ViewValidator} — validates assembled view XML models before the
 *       view is made available for rendering.
 * </ul>
 *
 * <p>The XML-binding metadata classes for each entity type live in the sub-packages
 * ({@code customer}, {@code document}, {@code module}, {@code router}, {@code view},
 * {@code behaviour}).
 *
 * @see org.skyve.metadata.repository
 */
package org.skyve.impl.metadata.repository;

/**
 * Converter contracts for bidirectional translation between Skyve domain types and
 * their user-facing string representations.
 *
 * <p>This package defines three collaborating types:
 * <ul>
 *   <li>{@link org.skyve.domain.types.converters.Converter} — the primary SPI: converts
 *       a typed value to a display string and back, and optionally carries a
 *       {@link org.skyve.domain.types.converters.Format} and a
 *       {@link org.skyve.domain.types.converters.Validator}.
 *   <li>{@link org.skyve.domain.types.converters.Format} — an optional input-mask and
 *       text-case constraint layered on top of the raw conversion.
 *   <li>{@link org.skyve.domain.types.converters.Validator} — an optional semantic
 *       validator that runs after a successful {@code fromDisplayValue} call to enforce
 *       domain-level constraints.
 * </ul>
 *
 * <p>Concrete converter implementations are in the type-specific sub-packages
 * ({@code date}, {@code datetime}, {@code decimal}, {@code integer}, {@code time},
 * {@code timestamp}, {@code enumeration}, {@code geometry}).
 *
 * <p>The framework applies converters in the view binding pipeline, in REST
 * serialisation, and during import/export operations. Converters are declared in
 * document attribute metadata and resolved at runtime via
 * {@link org.skyve.metadata.model.Attribute#getConverter()}.
 *
 * <p>Threading: converter instances are shared across requests and threads.
 * Implementations must be stateless (or effectively immutable after construction).
 *
 * @see org.skyve.domain.types.converters.Converter
 * @see org.skyve.domain.types.formatters
 */
package org.skyve.domain.types.converters;

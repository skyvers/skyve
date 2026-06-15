/**
 * Contract for generating unique, monotonically increasing document sequence numbers.
 *
 * <p>Skyve applications use document numbers to assign human-readable identifiers to
 * records — invoice numbers, order codes, case references, and similar sequenced fields.
 * The single interface in this package, {@link org.skyve.domain.number.NumberGenerator},
 * provides thread-safe per-field counters keyed by
 * {@code (moduleName, documentName, fieldName)}.
 *
 * <p>The runtime implementation is obtained via {@link org.skyve.EXT#getNumberGenerator()}.
 * Callers should never instantiate generators directly.
 *
 * <p>Threading: all {@code NumberGenerator} methods must be safe for concurrent use.
 * Implementations coordinate through the persistence layer to guarantee uniqueness even
 * under clustered deployments.
 *
 * @see org.skyve.domain.number.NumberGenerator
 * @see org.skyve.EXT#getNumberGenerator()
 */
package org.skyve.domain.number;

/**
 * {@link org.skyve.domain.types.DateOnly} converters providing locale-neutral date format patterns.
 *
 * <p>Each class converts a {@code DateOnly} to and from a string using a specific
 * date format pattern (e.g. {@code DD_MM_YYYY}, {@code MM_DD_YYYY}, {@code YYYY_MM_DD},
 * {@code DD_MMM_YYYY}). {@code AbstractDateConverter} is the shared base that handles
 * thread-safe {@code SimpleDateFormat} management.
 *
 * <p>Converters are stateless, thread-safe singletons obtained via
 * {@link org.skyve.domain.types.converters.Converter#getInstance()}.
 *
 * @see org.skyve.domain.types.DateOnly
 * @see org.skyve.domain.types.converters
 */
package org.skyve.domain.types.converters.date;

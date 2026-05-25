/**
 * {@link org.skyve.domain.types.DateTime} converters providing locale-neutral date-time format patterns.
 *
 * <p>Each class converts a {@code DateTime} to and from a string using a combined
 * date+time format pattern (e.g. {@code DD_MM_YYYY_HH24_MI}, {@code MM_DD_YYYY_HH_MI},
 * {@code YYYY_MM_DD_HH24_MI}). Patterns ending in {@code _HH_MI} use 12-hour clock;
 * those ending in {@code _HH24_MI} use 24-hour clock. {@code AbstractDateTimeConverter}
 * provides the shared base with thread-safe format handling.
 *
 * <p>Converters are stateless, thread-safe singletons.
 *
 * @see org.skyve.domain.types.DateTime
 * @see org.skyve.domain.types.converters
 */
package org.skyve.domain.types.converters.datetime;

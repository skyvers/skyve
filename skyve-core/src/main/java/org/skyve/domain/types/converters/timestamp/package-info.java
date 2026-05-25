/**
 * {@link org.skyve.domain.types.Timestamp} converters providing locale-neutral
 * date-time-second format patterns.
 *
 * <p>Each class converts a {@code Timestamp} to and from a string using a combined
 * date+time+seconds format pattern (e.g. {@code DD_MM_YYYY_HH24_MI_SS},
 * {@code MM_DD_YYYY_HH_MI_SS}, {@code YYYY_MM_DD_HH24_MI_SS}). Patterns ending in
 * {@code _HH_MI_SS} use 12-hour clock; those ending in {@code _HH24_MI_SS} use
 * 24-hour clock. {@code AbstractTimestampConverter} provides the shared base with
 * thread-safe format handling.
 *
 * <p>Converters are stateless, thread-safe singletons.
 *
 * @see org.skyve.domain.types.Timestamp
 * @see org.skyve.domain.types.converters
 */
package org.skyve.domain.types.converters.timestamp;

/**
 * {@link org.skyve.domain.types.TimeOnly} converters providing locale-neutral time format patterns.
 *
 * <p>Each class converts a {@code TimeOnly} to and from a string using a specific time
 * format (e.g. {@code HH_MI}, {@code HH24_MI}, {@code HH_MI_SS}, {@code HH24_MI_SS}).
 * Patterns containing {@code HH} use 12-hour clock; those containing {@code HH24} use
 * 24-hour clock. {@code AbstractTimeConverter} provides the shared base with
 * thread-safe format handling.
 *
 * <p>Converters are stateless, thread-safe singletons.
 *
 * @see org.skyve.domain.types.TimeOnly
 * @see org.skyve.domain.types.converters
 */
package org.skyve.domain.types.converters.time;

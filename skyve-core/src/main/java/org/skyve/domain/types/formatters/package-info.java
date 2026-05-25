/**
 * Type formatters that convert domain values to localised display strings.
 *
 * <p>{@link org.skyve.domain.types.formatters.Formatter} is the primary SPI: implement it
 * to produce a display string from a typed value. All implementations must be thread-safe;
 * Skyve holds exactly one instance per formatter class.
 *
 * <p>Concrete formatters:
 * <ul>
 *   <li>{@link org.skyve.domain.types.formatters.DecimalFormatter} — decimal numbers</li>
 *   <li>{@link org.skyve.domain.types.formatters.SimpleDateFormatter} — dates using a
 *       {@link java.text.SimpleDateFormat} pattern</li>
 *   <li>{@link org.skyve.domain.types.formatters.TimeDurationFormatter} — time durations</li>
 *   <li>{@link org.skyve.domain.types.formatters.StringFormatter} — plain string values</li>
 * </ul>
 *
 * <p>{@link org.skyve.domain.types.formatters.Formatters} is a registry that resolves
 * a formatter by its class name for use from metadata and binding utilities.
 *
 * @see org.skyve.domain.types.formatters.Formatter
 */
package org.skyve.domain.types.formatters;

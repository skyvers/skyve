/**
 * Skyve's first-class domain types for dates, times, fixed-precision decimals,
 * enumerations, and optimistic-lock tokens.
 *
 * <h2>Temporal types</h2>
 * <p>Four temporal types extend {@link java.util.Date} and intentionally truncate
 * precision to match their metadata declaration:
 * <ul>
 *   <li>{@link org.skyve.domain.types.DateOnly} — calendar date at midnight; no time component.
 *   <li>{@link org.skyve.domain.types.TimeOnly} — wall-clock time (hours, minutes, seconds); no date component.
 *   <li>{@link org.skyve.domain.types.DateTime} — date and time to the minute; seconds zeroed.
 *   <li>{@link org.skyve.domain.types.Timestamp} — date and time to the second; milliseconds zeroed.
 * </ul>
 * <p>All four extend {@code java.util.Date} so they are directly compatible with existing
 * date APIs and persistence mappings. Each overrides {@link java.util.Date#toString()} to
 * return a stable, serialization-safe string via the format exposed on
 * {@link org.skyve.CORE}.
 *
 * <h2>Decimal types</h2>
 * <p>Three fixed-scale decimal types extend {@link org.skyve.domain.types.Decimal}:
 * <ul>
 *   <li>{@link org.skyve.domain.types.Decimal2} — 2 decimal places; suitable for currency.
 *   <li>{@link org.skyve.domain.types.Decimal5} — 5 decimal places; suitable for rates and quantities.
 *   <li>{@link org.skyve.domain.types.Decimal10} — 10 decimal places; suitable for scientific values.
 * </ul>
 * <p>All arithmetic operations return a new instance of the same concrete type (value
 * semantics). Equality is based on {@link java.math.BigDecimal#equals}, which requires
 * identical scale; use {@link org.skyve.domain.types.Decimal#compareTo} for value
 * equality across scales.
 *
 * <h2>Enumeration</h2>
 * <p>{@link org.skyve.domain.types.Enumeration} is the interface implemented by every
 * Skyve-generated enum. The {@code toCode()} value is stored in the database; the
 * {@code toLocalisedDescription()} value is shown in the UI.
 *
 * <h2>OptimisticLock</h2>
 * <p>{@link org.skyve.domain.types.OptimisticLock} captures the user and UTC timestamp of
 * the last successful save of a {@link org.skyve.domain.PersistentBean}. The persistence
 * layer serialises it as a 17-character timestamp followed by the username.
 */
package org.skyve.domain.types;

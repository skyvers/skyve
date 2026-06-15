/**
 * Currency (monetary) converters for Skyve decimal types.
 *
 * <p>Converters in this package format decimal values as monetary amounts with a
 * currency symbol and decimal grouping:
 * <ul>
 *   <li>{@code Decimal2DollarsAndCents} — formats {@code Decimal2} as dollars and cents.
 *   <li>{@code Decimal2DollarsAndCentsAbsolute} — same as above but always positive
 *       (absolute value).
 *   <li>{@code Decimal5DollarsAndCents} — formats {@code Decimal5} as dollars and cents.
 *   <li>{@code Decimal10DollarsAndCents} — formats {@code Decimal10} as dollars and cents.
 * </ul>
 *
 * @see org.skyve.domain.types.converters.decimal
 */
package org.skyve.domain.types.converters.decimal.currency;

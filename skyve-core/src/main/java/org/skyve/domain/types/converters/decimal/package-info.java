/**
 * {@link org.skyve.domain.types.Decimal2}, {@link org.skyve.domain.types.Decimal5},
 * and {@link org.skyve.domain.types.Decimal10} converters for numeric formatting.
 *
 * <p>Converters in this package produce human-readable numeric strings in several
 * formats:
 * <ul>
 *   <li>Plain decimal ({@code Decimal2Converter}, {@code Decimal5Converter},
 *       {@code Decimal10Converter}).
 *   <li>Integer (no decimal places) — {@code Decimal2Integer}, etc.
 *   <li>Percentage — {@code Decimal2IntegerPercentage}, etc.
 *   <li>One or two decimal places — {@code Decimal5OneDecimalPlace}, etc.
 *   <li>Time duration — {@code Decimal5TimeDuration} formats decimal hours as HH:MM.
 * </ul>
 *
 * <p>The {@code currency} sub-package contains monetary formatting converters.
 *
 * @see org.skyve.domain.types.converters.decimal.currency
 * @see org.skyve.domain.types.Decimal2
 */
package org.skyve.domain.types.converters.decimal;

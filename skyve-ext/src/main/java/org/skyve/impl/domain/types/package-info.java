/**
 * Hibernate {@link org.hibernate.usertype.UserType} mappings for Skyve domain types.
 *
 * <p>Each class in this package maps a Skyve temporal, decimal, enum, or
 * optimistic-lock type to its SQL column representation:
 * <ul>
 *   <li>{@code DateOnlyUserType} — {@link org.skyve.domain.types.DateOnly} to SQL {@code DATE}
 *   <li>{@code DateTimeUserType} — {@link org.skyve.domain.types.DateTime} to SQL {@code TIMESTAMP}
 *   <li>{@code TimeOnlyUserType} — {@link org.skyve.domain.types.TimeOnly} to SQL {@code TIME}
 *   <li>{@code TimestampUserType} — {@link org.skyve.domain.types.Timestamp} to SQL {@code TIMESTAMP}
 *   <li>{@code Decimal2UserType}, {@code Decimal5UserType}, {@code Decimal10UserType}
 *       — fixed-precision decimal types to SQL {@code NUMERIC}
 *   <li>{@code EnumUserType} — Skyve enums to SQL {@code VARCHAR} via {@code code()}
 *   <li>{@code OptimisticLockUserType} — {@link org.skyve.domain.types.OptimisticLock}
 *       to its serialised string form
 * </ul>
 */
package org.skyve.impl.domain.types;

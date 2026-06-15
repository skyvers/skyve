package org.skyve.persistence;

import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Native SQL query wrapper with type-safe Skyve-type parameter binding.
 *
 * <p>Use {@code SQL} when you need the full expressiveness of native SQL — vendor-specific
 * functions, complex CTEs, or bulk operations — while still leveraging Skyve's type system
 * for parameter binding. Typed {@code putParameter} overloads handle the conversion from
 * Skyve domain types ({@link org.skyve.domain.types.DateOnly}, {@link org.skyve.domain.types.Decimal},
 * {@link org.skyve.domain.types.Enumeration}, etc.) to the appropriate JDBC types.
 *
 * <p>Supported result shapes: bean retrieval ({@link BeanQuery}), scalar values
 * ({@link ScalarQuery}), tuples ({@link TupleQuery}), data modification
 * ({@link DMLQuery}), and {@link org.apache.commons.beanutils.DynaBean} rows
 * via {@link #dynaResults()} / {@link #dynaResult()} / {@link #retrieveDyna()} /
 * {@link #dynaIterable()}.
 *
 * <p>{@link org.apache.commons.beanutils.DynaBean} results are useful when the column
 * shape is not known at compile time or does not map to a specific domain type.
 *
 * <p>Instances are created via {@link Persistence#newSQL(String)} and its overloads.
 * Not thread-safe; use within a single request thread.
 *
 * @see Persistence#newSQL(String)
 */
public interface SQL extends BeanQuery, ScalarQuery, TupleQuery, DMLQuery {
	/**
	 * Binds a {@link org.skyve.domain.types.DateOnly} parameter by name.
	 *
	 * @param name  the parameter name as it appears in the SQL string (without the colon)
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable DateOnly value);

	/**
	 * Binds a {@link org.skyve.domain.types.DateTime} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable DateTime value);

	/**
	 * Binds a {@link org.skyve.domain.types.TimeOnly} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable TimeOnly value);

	/**
	 * Binds a {@link org.skyve.domain.types.Timestamp} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Timestamp value);

	/**
	 * Binds a {@link org.skyve.domain.types.Decimal} parameter by name.
	 * The value is bound as its {@link java.math.BigDecimal} equivalent.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Decimal value);

	/**
	 * Binds an {@link Integer} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Integer value);

	/**
	 * Binds a {@link Long} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Long value);

	/**
	 * Binds a {@link String} parameter by name.
	 *
	 * @param name          the parameter name
	 * @param value         the value to bind; may be {@code null}
	 * @param memoOrMarkup  {@code true} if the string is a memo or markup field and should be
	 *                      treated as a CLOB/TEXT column type rather than a VARCHAR
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable String value, boolean memoOrMarkup);

	/**
	 * Binds a {@link Bean} reference parameter by name, using the bean's {@code bizId} as the
	 * bound value.
	 *
	 * @param name  the parameter name
	 * @param value the bean whose bizId is used as the parameter value; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Bean value);

	/**
	 * Binds a {@link org.locationtech.jts.geom.Geometry} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the geometry value; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Geometry value);

	/**
	 * Binds a {@link Boolean} parameter by name.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Boolean value);

	/**
	 * Binds an {@link Enumeration} parameter by name.
	 * The enumeration is stored using its {@link Enumeration#toCode()} value.
	 *
	 * @param name  the parameter name
	 * @param value the enumeration value; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Enumeration value);

	/**
	 * Binds an arbitrary parameter by name, specifying the Skyve attribute type for correct
	 * JDBC type mapping.
	 *
	 * @param name  the parameter name
	 * @param value the value to bind; may be {@code null}
	 * @param type  the Skyve attribute type that governs JDBC type conversion; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Object value, @Nonnull AttributeType type);
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull SQL noTimeout();
	
	/**
	 * Returns all rows as {@link org.apache.commons.beanutils.DynaBean} instances.
	 * Useful when the column shape is not known at compile time.
	 *
	 * @return a mutable list of rows; never {@code null}, may be empty
	 */
	@Nonnull List<DynaBean> dynaResults();

	/**
	 * Returns the first row as a {@link org.apache.commons.beanutils.DynaBean},
	 * or {@code null} if there are no results.
	 *
	 * @return the first row, or {@code null}
	 */
	@Nullable DynaBean dynaResult();

	/**
	 * Returns exactly one row; throws {@link org.skyve.domain.messages.NoResultsException}
	 * or {@link org.skyve.domain.messages.ManyResultsException} if there are not exactly
	 * one row.
	 *
	 * @return the single row; never {@code null}
	 * @throws org.skyve.domain.messages.NoResultsException   if no rows are returned
	 * @throws org.skyve.domain.messages.ManyResultsException if more than one row is returned
	 */
	@Nonnull DynaBean retrieveDyna();

	/**
	 * Returns a streaming {@link AutoClosingIterable} over all rows.
	 * Must be used in a try-with-resources block.
	 *
	 * @return an iterable cursor; never {@code null}
	 */
	@Nonnull AutoClosingIterable<DynaBean> dynaIterable();
}

package org.skyve.impl.persistence;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLTimeoutException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.LazyDynaMap;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.beanutils.ResultSetDynaClass;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.SQL;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract implementation of {@link org.skyve.persistence.SQL} providing parameter
 * binding, projection, timeout management, and execution scaffolding for raw SQL
 * queries against Skyve's configured data store.
 *
 * <p>Subclasses supply the JDBC/Hibernate execution logic by implementing
 * {@link #toQueryString()} and the underlying execution methods.
 *
 * @see org.skyve.persistence.SQL
 */
public abstract class AbstractSQL extends AbstractQuery implements SQL {
    private static final Logger QUERY_LOGGER = Category.QUERY.logger();

    private String query = null;
	private @Nullable String moduleName;
	private @Nullable String documentName;

	private Map<String, AttributeType> parametersTypes = new TreeMap<>();

	/**
	 * Creates a raw SQL query without a driving document context.
	 *
	 * @param query SQL text to execute
	 */
	public AbstractSQL(@Nonnull String query) {
		this.query = query;
	}

	/**
	 * Creates a raw SQL query with explicit driving module/document names.
	 *
	 * @param moduleName the driving module name
	 * @param documentName the driving document name
	 * @param query SQL text to execute
	 */
	public AbstractSQL(@Nonnull String moduleName,
						@Nonnull String documentName,
						@Nonnull String query) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.query = query;
	}

	/**
	 * Creates a raw SQL query using the supplied document as driving metadata.
	 *
	 * @param document driving document metadata
	 * @param query SQL text to execute
	 */
	public AbstractSQL(@Nonnull Document document,
						@Nonnull String query) {
		this.moduleName = document.getOwningModuleName();
		this.documentName = document.getName();
		this.query = query;
	}
 
	/**
	 * Disables execution timeout enforcement for this SQL query instance.
	 *
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL noTimeout() {
		this.timeoutInSeconds = Integer.MIN_VALUE;
		return this;
	}

	/**
	 * Binds a date-only parameter.
	 *
	 * @param name parameter name
	 * @param value date-only value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, DateOnly value) {
		return putParameter(name, value, AttributeType.date);
	}

	/**
	 * Binds a date-time parameter.
	 *
	 * @param name parameter name
	 * @param value date-time value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, DateTime value) {
		return putParameter(name, value, AttributeType.dateTime);
	}

	/**
	 * Binds a time-only parameter.
	 *
	 * @param name parameter name
	 * @param value time-only value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, TimeOnly value) {
		return putParameter(name, value, AttributeType.time);
	}

	/**
	 * Binds a timestamp parameter.
	 *
	 * @param name parameter name
	 * @param value timestamp value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Timestamp value) {
		return putParameter(name, value, AttributeType.timestamp);
	}

	/**
	 * Binds a decimal parameter.
	 *
	 * @param name parameter name
	 * @param value decimal value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Decimal value) {
		return putParameter(name, value, AttributeType.decimal10);
	}

	/**
	 * Binds an integer parameter.
	 *
	 * @param name parameter name
	 * @param value integer value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Integer value) {
		return putParameter(name, value, AttributeType.integer);
	}

	/**
	 * Binds a long-integer parameter.
	 *
	 * @param name parameter name
	 * @param value long value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Long value) {
		return putParameter(name, value, AttributeType.longInteger);
	}

	/**
	 * Binds a string parameter, optionally flagged as memo text.
	 *
	 * @param name parameter name
	 * @param value string value
	 * @param memo whether the value should be treated as memo/long text
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, String value, boolean memo) {
		return putParameter(name, value, memo ? AttributeType.memo : AttributeType.text);
	}

	/**
	 * Binds a bean parameter by using its bizId.
	 *
	 * @param name parameter name
	 * @param value bean value, may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Bean value) {
		return putParameter(name, (value == null) ? null : value.getBizId(), AttributeType.id);
	}

	/**
	 * Binds a geometry parameter.
	 *
	 * @param name parameter name
	 * @param value geometry value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Geometry value) {
		return putParameter(name, value, AttributeType.geometry);
	}

	/**
	 * Binds a boolean parameter.
	 *
	 * @param name parameter name
	 * @param value boolean value
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Boolean value) {
		return putParameter(name, value, AttributeType.bool);
	}

	/**
	 * Binds an enumeration parameter using its persisted code.
	 *
	 * @param name parameter name
	 * @param value enumeration value, may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Override
	public SQL putParameter(String name, Enumeration value) {
		return putParameter(name, (value == null) ? null : value.toCode(), AttributeType.enumeration);
	}

	/**
	 * Binds a parameter value with an explicit Skyve attribute type.
	 *
	 * @param name parameter name
	 * @param value parameter value
	 * @param type declared Skyve attribute type
	 * @return this query for fluent chaining
	 */
	@Override
	public AbstractSQL putParameter(String name, Object value, AttributeType type) {
		parameters.put(name, value);
		parametersTypes.put(name, type);
		if (UtilImpl.QUERY_TRACE) {
		    QUERY_LOGGER.info("    SET PARAM {} = {}", name, value);
		}
		return this;
	}
	
	/**
	 * Returns the declared Skyve attribute type for a named SQL parameter.
	 *
	 * @param name the parameter name
	 * @return the declared parameter type, or {@code null} when unknown
	 */
	public final @Nullable AttributeType getParameterType(String name) {
		return parametersTypes.get(name);
	}
	
	/**
	 * Returns the executable SQL text for the current query state.
	 *
	 * @return SQL query string
	 */
	@Override
	public String toQueryString() {
		return query;
	}
	
	/**
	 * Returns the driving module name associated with this SQL query.
	 *
	 * @return the module name, or {@code null} when no driving document is set
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}
	
	/**
	 * Returns the driving document name associated with this SQL query.
	 *
	 * @return the document name, or {@code null} when no driving document is set
	 */
	public @Nullable String getDocumentName() {
		return documentName;
	}
	
	/**
	 * Returns the first bean result, or {@code null} when no rows match.
	 *
	 * @param <T> bean type
	 * @return first result bean or {@code null}
	 */
	@Override
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one bean result.
	 *
	 * @param <T> bean type
	 * @return single result bean
	 */
	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the first scalar result, or {@code null} when no rows match.
	 *
	 * @param <T> scalar type
	 * @param type expected scalar class
	 * @return first scalar result or {@code null}
	 */
	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one scalar result.
	 *
	 * @param <T> scalar type
	 * @param type expected scalar class
	 * @return single scalar result
	 */
	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the first tuple result, or {@code null} when no rows match.
	 *
	 * @return first tuple result or {@code null}
	 */
	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one tuple result.
	 *
	 * @return single tuple result
	 */
	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
	
	/**
	 * Returns the first dynamic-row result or {@code null} when no results exist.
	 */
	@Override
	public final DynaBean dynaResult() {
		List<DynaBean> results = dynaResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one dynamic-row result.
	 *
	 * @return single dynamic-row result
	 */
	@Override
	public final DynaBean retrieveDyna() {
		List<DynaBean> results = dynaResults();
		return AbstractQuery.assertOneResult(results);
	}
	
	/**
	 * Applies timeout and parameter bindings to a named-parameter statement.
	 *
	 * <p>Side effects: mutates the supplied prepared statement by setting query
	 * timeout and all currently bound parameters, including JDBC type mappings.
	 *
	 * @param ps the prepared statement wrapper to configure
	 * @param dataStore the active data-store configuration
	 * @param dialect SQL dialect used for geometry conversion/type mapping
	 * @throws TimeoutException if query timeout configuration fails
	 * @throws DomainException if statement preparation fails for other reasons
	 */
	public void prepareStatement(@Nonnull NamedParameterPreparedStatement ps, @Nonnull DataStore dataStore, @Nonnull SkyveDialect dialect) {
		try {
			// negative timeout values means no timeout
			if (timeoutInSeconds == 0) {
				AbstractPersistence p = AbstractPersistence.get();
				if (p.isAsyncThread()) {
					int timeout = dataStore.getAsyncConnectionTimeoutInSeconds();
					if (timeout > 0) {
						ps.setQueryTimeout(timeout);
					}
				}
				else {
					int timeout = dataStore.getOltpConnectionTimeoutInSeconds();
					if (timeout > 0) {
						ps.setQueryTimeout(timeout);
					}
				}
			}
			else if (timeoutInSeconds > 0) {
				ps.setQueryTimeout(timeoutInSeconds);
			}
			
			for (String name : getParameterNames()) {
				Object value = getParameter(name);
				AttributeType type = getParameterType(name);

				if (AttributeType.bool.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.BOOLEAN);
					}
					else {
						ps.setBoolean(name, ((Boolean) value).booleanValue());
					}
				}
				else if (AttributeType.colour.equals(type) ||
							AttributeType.content.equals(type) ||
							AttributeType.image.equals(type) ||
							AttributeType.enumeration.equals(type) ||
							AttributeType.text.equals(type) ||
							AttributeType.id.equals(type) ||
							AttributeType.association.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.VARCHAR);
					}
					else {
						if (value instanceof Bean bean) {
							ps.setString(name, bean.getBizId());
						}
						else if (value instanceof Enumeration enumeration) {
							ps.setString(name, enumeration.toCode());
						}
						else {
							ps.setString(name, (String) value);
						}
					}
				}
				else if (AttributeType.markup.equals(type) ||
							AttributeType.memo.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.LONGVARCHAR);
					}
					else {
						ps.setString(name, (String) value);
					}
				}
				else if (AttributeType.date.equals(type)) {
					if (value == null) {
						ps.setNull(name,Types.DATE);
					}
					else {
						ps.setDate(name, new java.sql.Date(((Date) value).getTime()));
					}
				}
				else if (AttributeType.dateTime.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.TIMESTAMP);
					}
					else {
						ps.setTimestamp(name, new java.sql.Timestamp(((Date) value).getTime()));
					}
				}
				else if (AttributeType.decimal10.equals(type) ||
							AttributeType.decimal2.equals(type) ||
							AttributeType.decimal5.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.DECIMAL);
					}
					else {
						if (value instanceof BigDecimal bigDecimal) {
							ps.setBigDecimal(name, bigDecimal);
						}
						else if (value instanceof Decimal decimal) {
							ps.setBigDecimal(name, decimal.bigDecimalValue());
						}
						else {
							ps.setBigDecimal(name, new BigDecimal(value.toString()));
						}
					}
				}
				else if (AttributeType.geometry.equals(type)) {
					if (value == null) {
						ps.setNull(name, dialect.getGeometrySqlType());
					}
					else {
						if (dialect.getGeometrySqlType() == Types.ARRAY) {
							ps.setBytes(name, (byte[]) dialect.convertToPersistedValue((Geometry) value));
						}
						else {
							ps.setObject(name, dialect.convertToPersistedValue((Geometry) value));
						}
					}
				}
				else if (AttributeType.integer.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.INTEGER);
					}
					else {
						ps.setInt(name, ((Number) value).intValue());
					}
				}
				else if (AttributeType.longInteger.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.NUMERIC);
					}
					else {
						ps.setLong(name,  ((Number) value).longValue());
					}
				}
				else if (AttributeType.time.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.TIME);
					}
					else {
						ps.setTime(name, new java.sql.Time(((Date) value).getTime()));
					}
				}
				else if (AttributeType.timestamp.equals(type)) {
					if (value == null) {
						ps.setNull(name, Types.TIMESTAMP);
					}
					else {
						ps.setTimestamp(name, new java.sql.Timestamp(((Date) value).getTime()));
					}
				}
				else {
					ps.setObject(name, value);
				}
			}
		}
		catch (SQLTimeoutException e) {
			throw new TimeoutException(e);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException("Cannot prepare a statement", e);
		}
	}
	
	/**
	 * Materialises a JDBC {@link ResultSet} into detached {@link DynaBean} rows.
	 *
	 * <p>Side effects: consumes the result-set cursor.
	 *
	 * @param rs the JDBC result set to read
	 * @return a list of detached dynamic beans representing all rows
	 */
	protected static @Nonnull List<DynaBean> dynaList(@Nonnull ResultSet rs) {
		final ArrayList<DynaBean> result = new ArrayList<>(100);

		try {
			final ResultSetDynaClass rsdc = new ResultSetDynaClass(rs, true, true);
			final LazyDynaMap dynaMap = new LazyDynaMap(rsdc.getDynaProperties());
			final Iterator<DynaBean> rows = rsdc.iterator();
			while (rows.hasNext()) {
				final DynaBean row = rows.next();
				final DynaBean newRow = dynaMap.newInstance();
				PropertyUtils.copyProperties(newRow, row);
				result.add(newRow);
			}
		}
		catch (Exception e) {
			throw new DomainException("Cannot extract DynaBeans from the result set", e);
		}

		return result;
	}
}

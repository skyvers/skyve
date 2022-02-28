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

public abstract class AbstractSQL extends AbstractQuery implements SQL {
	private String query = null;
	private String moduleName;
	private String documentName;

	private Map<String, AttributeType> parametersTypes = new TreeMap<>();

	public AbstractSQL(String query) {
		this.query = query;
	}

	public AbstractSQL(String moduleName,
							String documentName,
							String query) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.query = query;
	}

	public AbstractSQL(Document document,
							String query) {
		this.moduleName = document.getOwningModuleName();
		this.documentName = document.getName();
		this.query = query;
	}
 
	@Override
	public SQL noTimeout() {
		this.timeoutInSeconds = 0;
		return this;
	}

	@Override
	public SQL putParameter(String name, DateOnly value) {
		return putParameter(name, value, AttributeType.date);
	}

	@Override
	public SQL putParameter(String name, DateTime value) {
		return putParameter(name, value, AttributeType.dateTime);
	}

	@Override
	public SQL putParameter(String name, TimeOnly value) {
		return putParameter(name, value, AttributeType.time);
	}

	@Override
	public SQL putParameter(String name, Timestamp value) {
		return putParameter(name, value, AttributeType.timestamp);
	}

	@Override
	public SQL putParameter(String name, Decimal value) {
		return putParameter(name, value, AttributeType.decimal10);
	}

	@Override
	public SQL putParameter(String name, Integer value) {
		return putParameter(name, value, AttributeType.integer);
	}

	@Override
	public SQL putParameter(String name, Long value) {
		return putParameter(name, value, AttributeType.longInteger);
	}

	@Override
	public SQL putParameter(String name, String value, boolean memo) {
		return putParameter(name, value, memo ? AttributeType.memo : AttributeType.text);
	}

	@Override
	public SQL putParameter(String name, Bean value) {
		return putParameter(name, (value == null) ? null : value.getBizId(), AttributeType.id);
	}

	@Override
	public SQL putParameter(String name, Geometry value) {
		return putParameter(name, value, AttributeType.geometry);
	}

	@Override
	public SQL putParameter(String name, Boolean value) {
		return putParameter(name, value, AttributeType.bool);
	}

	@Override
	public SQL putParameter(String name, Enumeration value) {
		return putParameter(name, (value == null) ? null : value.toCode(), AttributeType.enumeration);
	}

	@Override
	public AbstractSQL putParameter(String name, Object value, AttributeType type) {
		parameters.put(name, value);
		parametersTypes.put(name, type);
		if (UtilImpl.QUERY_TRACE) {
			UtilImpl.LOGGER.info("    SET PARAM " + name + " = " + value);
		}
		return this;
	}
	
	public final AttributeType getParameterType(String name) {
		return parametersTypes.get(name);
	}
	
	@Override
	public String toQueryString() {
		return query;
	}
	
	public String getModuleName() {
		return moduleName;
	}
	
	public String getDocumentName() {
		return documentName;
	}
	
	@Override
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
	
	@Override
	public final DynaBean dynaResult() {
		List<DynaBean> results = dynaResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final DynaBean retrieveDyna() {
		List<DynaBean> results = dynaResults();
		return AbstractQuery.assertOneResult(results);
	}
	
	public void prepareStatement(NamedParameterPreparedStatement ps, DataStore dataStore, SkyveDialect dialect) {
		try {
			if (timeoutInSeconds > 0) {
				ps.setQueryTimeout(timeoutInSeconds);
			}
			else {
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
						if (value instanceof Bean) {
							ps.setString(name, ((Bean) value).getBizId());
						}
						else if (value instanceof Enumeration) {
							ps.setString(name, ((Enumeration) value).toCode());
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
						if (value instanceof BigDecimal) {
							ps.setBigDecimal(name, (BigDecimal) value);
						}
						else if (value instanceof Decimal) {
							ps.setBigDecimal(name, ((Decimal) value).bigDecimalValue());
						}
						else {
							ps.setBigDecimal(name, new BigDecimal(((Number) value).toString()));
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
	
	protected static List<DynaBean> dynaList(ResultSet rs) {
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

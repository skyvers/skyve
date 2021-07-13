package org.skyve.impl.dataaccess.sql;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLTimeoutException;
import java.sql.Time;
import java.sql.Types;
import java.util.Date;
import java.util.Iterator;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.NamedParameterPreparedStatement;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;

class SQLIterable<T> implements AutoClosingIterable<T> {
	private Document document;
	private SQLDataAccessImpl dataAccess;
	private Class<?> scalarType;
	private NamedParameterPreparedStatement ps = null;
	private ResultSet rs = null;
	
	SQLIterable(Document document,
					SQLDataAccessImpl dataAccess,
					SQLDataAccessSQL sql,
					Class<T> scalarType) {
		this.document = document;
		this.dataAccess = dataAccess;
		this.scalarType = scalarType;
		try {
			@SuppressWarnings("resource")
			Connection c = dataAccess.getConnection();
			ps = new NamedParameterPreparedStatement(c, sql.toQueryString());
			sql.prepareStatement(ps, dataAccess.dataStore, dataAccess.getDialect());
			rs = ps.executeQuery();
		}
		catch (TimeoutException e) {
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException("Cannot setup an SQLIterable", e);
		}
	}
	
	@Override
	public Iterator<T> iterator() {
		return new SQLIterator<>();
	}

	@Override
	public void close() {
		try {
			try {
				if ((rs != null) && (! rs.isClosed())) {
					rs.close();
					rs = null;
				}
			}
			finally {
				if (ps != null) {
					ps.close();
					ps = null;
				}
			}
		}
		catch (SQLException e) {
			throw new DomainException("Could not close resources from SQLIterable", e);
		}
	}
	
	private class SQLIterator<Z> implements Iterator<Z> {
		// This isn't exactly right because this hasNext() implementation has the side effect of moving on a record
		// This shouldn't matter because it should be used in an iterator for loop
		@Override
		public boolean hasNext() {
			boolean hasNext = false;
			
			try {
				if ((rs != null) && (! rs.isClosed())) {
					hasNext = rs.next();
		
					if (! hasNext) {
						rs.close();
						rs = null;
					}
				}
			}
			catch (SQLTimeoutException e) {
				throw new TimeoutException(e);
			}
			catch (SQLException e) {
				throw new DomainException(e);
			}
			
			return hasNext;
		}

		@Override
		public Z next() {
			if (document != null) { // bean select
				return nextBean();
			}
			if (scalarType != null) {
				return nextScalar();
			}
			return nextTuple();
		}
		
		@Override
		public void remove() {
			throw new IllegalStateException("Cannot remove from SQLIterator");
		}

		private Z nextBean() {
			try {
				Z result = document.newInstance(CORE.getUser());
	
				for (Attribute attribute : document.getAllAttributes()) {
					String name = attribute.getName();
					AttributeType type = attribute.getAttributeType();
					if (AttributeType.bool.equals(type)) {
						boolean value = rs.getBoolean(name);
						BindUtil.set(result, name, rs.wasNull() ? null : Boolean.valueOf(value));
					}
					else if (AttributeType.colour.equals(type) ||
								AttributeType.content.equals(type) ||
								AttributeType.image.equals(type) ||
								AttributeType.enumeration.equals(type) ||
								AttributeType.markup.equals(type) ||
								AttributeType.memo.equals(type) ||
								AttributeType.text.equals(type)) {
						BindUtil.convertAndSet(result, name, rs.getString(name));
					}
					else if (AttributeType.date.equals(type)) {
						Date value = rs.getDate(name);
						if (value != null) {
							BindUtil.convertAndSet(result, name, value);
						}
					}
					else if (AttributeType.dateTime.equals(type)) {
						Time value = rs.getTime(name);
						if (value != null) {
							BindUtil.convertAndSet(result, name, value);
						}
					}
					else if (AttributeType.decimal10.equals(type)) {
						double value = rs.getDouble(name);
						BindUtil.set(result, name, rs.wasNull() ? null : new Decimal10(value));
					}
					else if (AttributeType.decimal2.equals(type)) {
						double value = rs.getDouble(name);
						BindUtil.set(result, name, rs.wasNull() ? null : new Decimal2(value));
					}
					else if (AttributeType.decimal5.equals(type)) {
						double value = rs.getDouble(name);
						BindUtil.set(result, name, rs.wasNull() ? null : new Decimal5(value));
					}
					else if (AttributeType.geometry.equals(type)) {
						SkyveDialect dialect = dataAccess.getDialect();
						int geometrySqlType = dialect.getGeometrySqlType();
						Object value = null;
						if (geometrySqlType == Types.ARRAY) {
							value = rs.getBytes(name);
						}
						else {
							value = rs.getObject(name);
						}
						if (value != null) {
							Geometry geometry = dialect.convertFromPersistedValue(value);
							BindUtil.set(result, name, geometry);
						}
					}
					else if (AttributeType.integer.equals(type)) {
						int value = rs.getInt(name);
						BindUtil.set(result, name, rs.wasNull() ? null : Integer.valueOf(value));
					}
					else if (AttributeType.longInteger.equals(type)) {
						long value = rs.getLong(name);
						BindUtil.set(result, name, rs.wasNull() ? null : Long.valueOf(value));
					}
					else if (AttributeType.time.equals(type)) {
						Time value = rs.getTime(name);
						if (value != null) {
							BindUtil.convertAndSet(result, name, value);
						}
					}
					else if (AttributeType.timestamp.equals(type)) {
						Date value = rs.getDate(name);
						if (value != null) {
							BindUtil.convertAndSet(result, name, value);
						}
					}
				}
				
				return result;
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		}
		
		@SuppressWarnings("unchecked")
		Z nextScalar() {
			try {
				return (Z) BindUtil.convert(scalarType, rs.getObject(1));
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		}
		
		@SuppressWarnings("unchecked")
		Z nextTuple() {
			try {
				int columnCount = rs.getMetaData().getColumnCount();
				Object[] result = new Object[columnCount];
				for (int i = 0; i < columnCount; i++) {
					result[i] = rs.getObject(i + 1);
				}
				return (Z) result;
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		}
	}
}

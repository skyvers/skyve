package org.skyve.wildcat.dataaccess.sql;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.util.Iterator;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.bind.BindUtil;

import com.vividsolutions.jts.geom.Geometry;

class SQLIterable<T> implements AutoClosingIterable<T> {
	private Document document;
	private SQLDataAccess dataAccess;
	private Class<?> scalarType;
	private NamedParameterPreparedStatement ps = null;
	private ResultSet rs = null;
	
	SQLIterable(Document document, SQLDataAccess dataAccess, SQL sql, Class<T> scalarType) throws DomainException {
		this.document = document;
		this.dataAccess = dataAccess;
		this.scalarType = scalarType;
		try {
			ps = new NamedParameterPreparedStatement(dataAccess.getConnection(), sql.toQueryString());
			for (String parameterName : sql.getParameterNames()) {
				ps.setObject(parameterName, sql.getParameter(parameterName));
			}
			rs = ps.executeQuery();
		}
		catch (Exception e) {
			throw new DomainException("Cannot setup an SQLIterable", e);
		}
	}
	
	@Override
	@SuppressWarnings("synthetic-access")
	public Iterator<T> iterator() {
		return new SQLIterator<>();
	}

	@Override
	public void close() throws DomainException {
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
		@SuppressWarnings("synthetic-access")
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
			catch (SQLException e) {
				throw new RuntimeException(e);
			}
			
			return hasNext;
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public Z next() {
			if (document != null) { // bean select
				return nextBean();
			}
			if (scalarType != null) {
				return nextScalar();
			}
			return nextTuple();
		}
		
		@SuppressWarnings("synthetic-access")
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
						Geometry geometry = (Geometry) dataAccess.getGeometryUserType().nullSafeGet(rs, new String[] {name}, null);
						BindUtil.set(result, name, geometry);
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
				throw new RuntimeException(e);
			}
		}
		
		@SuppressWarnings({"synthetic-access", "unchecked"})
		Z nextScalar() {
			try {
				return (Z) BindUtil.convert(scalarType, rs.getObject(1));
			}
			catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		
		@SuppressWarnings({"synthetic-access", "unchecked"})
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
				throw new RuntimeException(e);
			}
		}
	}
}

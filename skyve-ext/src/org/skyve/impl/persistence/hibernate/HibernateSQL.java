package org.skyve.impl.persistence.hibernate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.hibernate.Hibernate;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernatespatial.AbstractDBGeometryType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractSQL;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.HibernateAutoClosingIterable;

import com.vividsolutions.jts.geom.Geometry;

class HibernateSQL extends AbstractSQL {
	private AbstractHibernatePersistence persistence;
	
	HibernateSQL(String moduleName,
					String documentName,
					String query,
					AbstractHibernatePersistence persistence) {
		super(moduleName, documentName, query);
		this.persistence = persistence;
	}

	HibernateSQL(Document document,
					String query,
					AbstractHibernatePersistence persistence) {
		super(document, query);
		this.persistence = persistence;
	}

	HibernateSQL(String query,
					AbstractHibernatePersistence persistence) {
		super(query);
		this.persistence = persistence;
	}

	@Override
	public <T extends Bean> List<T> beanResults() 
	throws DomainException {
		String moduleName = getModuleName();
		String documentName = getDocumentName();
		
		if ((moduleName == null) || (documentName == null)) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		try {
			String entityName = persistence.getDocumentEntityName(moduleName, documentName);
			return createQueryFromSQL().addEntity(entityName).list();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable()
	throws DomainException {
		String moduleName = getModuleName();
		String documentName = getDocumentName();

		if ((moduleName == null) || (documentName == null)) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		try {
			String entityName = persistence.getDocumentEntityName(moduleName, documentName);
			return new HibernateAutoClosingIterable<>(createQueryFromSQL().addEntity(entityName).scroll(), false, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type)
	throws DomainException {
		try {
			List<T> results = createQueryFromSQL().list();
			if ((! results.isEmpty()) && (results.get(0) instanceof Object[])) {
				throw new DomainException("There should be only 1 projected value in the query");
			}
			return results;
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type)
	throws DomainException {
		try {
			return new HibernateAutoClosingIterable<>(createQueryFromSQL().scroll(), true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public List<Object[]> tupleResults()
	throws DomainException {
		try {
			List<?> results = createQueryFromSQL().list();
			if ((! results.isEmpty()) && (! (results.get(0) instanceof Object[]))) {
				throw new DomainException("There should be more than 1 projected value in the query");
			}
			return (List<Object[]>) results;
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable()
	throws DomainException {
		try {
			return new HibernateAutoClosingIterable<>(createQueryFromSQL().scroll(), false, true);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public int execute()
	throws DomainException {
		try {
			SQLQuery query = createQueryFromSQL();
			return query.executeUpdate();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}
	
	private SQLQuery createQueryFromSQL() throws Exception {
		Session session = persistence.getSession();
		SQLQuery result = session.createSQLQuery(toQueryString());

		for (String name : getParameterNames()) {
			Object value = getParameter(name);
			
			if (value instanceof Decimal) {
				result.setBigDecimal(name, ((Decimal) value).bigDecimalValue());
				continue;
			}
			else if (value instanceof TimeOnly) {
				result.setTime(name,  new java.sql.Time(((Date) value).getTime()));
				continue;
			}
			else if ((value instanceof Timestamp) || (value instanceof DateTime)) {
				result.setTimestamp(name, new java.sql.Timestamp(((Date) value).getTime()));
				continue;
			}
			else if ((! (value instanceof java.sql.Date)) && (value instanceof Date)) {
				result.setDate(name, new java.sql.Date(((Date) value).getTime()));
				continue;
			}
			else if (value instanceof OptimisticLock) {
				result.setString(name, ((OptimisticLock) value).toString());
				continue;
			}
			else if (value instanceof Enumeration) {
				result.setString(name, ((Enumeration) value).toCode());
				continue;
			}
			
			AttributeType type = getParameterType(name);

			if (AttributeType.bool.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.BOOLEAN);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.BOOLEAN);
				}
				else {
					result.setParameter(name, value, Hibernate.BOOLEAN);
				}
			}
			else if (AttributeType.colour.equals(type) ||
						AttributeType.content.equals(type) ||
						AttributeType.text.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.STRING);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.STRING);
				}
				else {
					result.setParameter(name, value, Hibernate.STRING);
				}
			}
			else if (AttributeType.enumeration.equals(type)) {
				if (value instanceof Collection) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Collection<?>) value) {
						param.add((object instanceof Enumeration) ? ((Enumeration) object).toCode() : object);
					}
					result.setParameterList(name, param, Hibernate.STRING);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Enumeration) ? ((Enumeration) object).toCode() : object);
					}
					result.setParameterList(name, param, Hibernate.STRING);
				}
				else {
					result.setParameter(name, (value instanceof Enumeration) ? ((Enumeration) value).toCode() : value, Hibernate.STRING);
				}
			}
			else if (AttributeType.markup.equals(type) ||
						AttributeType.memo.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.TEXT);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.TEXT);
				}
				else {
					result.setParameter(name, value, Hibernate.TEXT);
				}
			}
			else if (AttributeType.date.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.DATE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.DATE);
				}
				else {
					result.setParameter(name, value, Hibernate.DATE);
				}
			}
			else if (AttributeType.dateTime.equals(type) ||
						AttributeType.timestamp.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.TIMESTAMP);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.TIMESTAMP);
				}
				else {
					result.setParameter(name, value, Hibernate.TIMESTAMP);
				}
			}
			else if (AttributeType.decimal10.equals(type) ||
						AttributeType.decimal2.equals(type) ||
						AttributeType.decimal5.equals(type)) {
				if (value instanceof Collection) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Collection<?>) value) {
						param.add((object instanceof Decimal) ? ((Decimal) object).bigDecimalValue() : object);
					}
					result.setParameterList(name, param, Hibernate.BIG_DECIMAL);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Decimal) ? ((Decimal) object).bigDecimalValue() : object);
					}
					result.setParameterList(name, param, Hibernate.BIG_DECIMAL);
				}
				else {
					result.setParameter(name, (value instanceof Decimal) ? ((Decimal) value).bigDecimalValue() : value, Hibernate.BIG_DECIMAL);
				}
			}
			else if (AttributeType.geometry.equals(type)) {
				if (value == null) {
					result.setParameter(name, value, Hibernate.BINARY);
				}
				else {
					// The SpatialDialect.getGeometryUseType() subclasses all give values of JDBC Types.ARRAY
					AbstractDBGeometryType geometryType = AbstractHibernatePersistence.getGeometryUserType();
					if (value instanceof Collection) {
						List<Object> param = new ArrayList<>();
						for (Object object : (Collection<?>) value) {
							if (object instanceof Geometry) {
								param.add(geometryType.conv2DBGeometry((Geometry) object, persistence.getConnection()));
							}
							else {
								param.add(object);
							}
						}
						result.setParameterList(name, param, Hibernate.BINARY);
					}
					else if (value.getClass().isArray()) {
						List<Object> param = new ArrayList<>();
						for (Object object : (Object[]) value) {
							if (object instanceof Geometry) {
								param.add(geometryType.conv2DBGeometry((Geometry) object, persistence.getConnection()));
							}
							else {
								param.add(object);
							}
						}
						result.setParameterList(name, param, Hibernate.BINARY);
					}
					else {
						if (value instanceof Geometry) {
							result.setParameter(name, AbstractHibernatePersistence.getGeometryUserType().conv2DBGeometry((Geometry) value, persistence.getConnection()), Hibernate.BINARY);
						}
						else {
							result.setParameter(name, value, Hibernate.BINARY);
						}
					}
				}
			}
			else if (AttributeType.integer.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.INTEGER);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.INTEGER);
				}
				else {
					result.setParameter(name, value, Hibernate.INTEGER);
				}
			}
			else if (AttributeType.longInteger.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.LONG);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.LONG);
				}
				else {
					result.setParameter(name, value, Hibernate.LONG);
				}
			}
			else if (AttributeType.time.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, Hibernate.TIME);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, Hibernate.TIME);
				}
				else {
					result.setParameter(name, value, Hibernate.TIME);
				}
			}
			else if (AttributeType.association.equals(type) ||
						AttributeType.id.equals(type)) {
				if (value instanceof Collection) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Collection<?>) value) {
						param.add((object instanceof Bean) ? ((Bean) object).getBizId() : object);
					}
					result.setParameterList(name, param, Hibernate.STRING);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Bean) ? ((Bean) object).getBizId() : object);
					}
					result.setParameterList(name, param, Hibernate.STRING);
				}
				else {
					result.setParameter(name, (value instanceof Bean) ? ((Bean) value).getBizId() : value, Hibernate.STRING);
				}
			}
			else {
				result.setParameter(name, value);
			}
		}
		
		return result;
	}
}

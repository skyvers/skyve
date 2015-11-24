package org.skyve.wildcat.persistence.hibernate;

import java.math.BigDecimal;
import java.sql.Time;
import java.util.Date;
import java.util.List;

import org.hibernate.Hibernate;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernatespatial.AbstractDBGeometryType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.persistence.AbstractSQL;
import org.skyve.wildcat.util.UtilImpl;

import com.vividsolutions.jts.geom.Geometry;

class HibernateSQL extends AbstractSQL {
	private AbstractHibernatePersistence persistence;
	private AbstractDBGeometryType geometryUserType = null; // this is only created when we come across a geometry
	
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
				result.setParameter(name, value, Hibernate.BOOLEAN);
			}
			else if (AttributeType.colour.equals(type) ||
						AttributeType.content.equals(type) ||
						AttributeType.enumeration.equals(type) ||
						AttributeType.text.equals(type) ||
						AttributeType.id.equals(type)) {
				result.setString(name, (String) value);
			}
			else if (AttributeType.markup.equals(type) ||
						AttributeType.memo.equals(type)) {
				result.setText(name, (String) value);
			}
			else if (AttributeType.date.equals(type)) {
				result.setDate(name, (Date) value);
			}
			else if (AttributeType.dateTime.equals(type)) {
				result.setTimestamp(name, (Date) value);
			}
			else if (AttributeType.decimal10.equals(type) ||
						AttributeType.decimal2.equals(type) ||
						AttributeType.decimal5.equals(type)) {
				result.setBigDecimal(name, (BigDecimal) value);
			}
			else if (AttributeType.geometry.equals(type)) {
				if (geometryUserType == null) {
					SpatialDialect dialect = (SpatialDialect) Class.forName(UtilImpl.DIALECT).newInstance();
					geometryUserType = (AbstractDBGeometryType) dialect.getGeometryUserType();
				}
				// The SpatialDialect.getGeometryUseType() subclasses all give values of JDBC Types.ARRAY
				result.setParameter(name, geometryUserType.conv2DBGeometry((Geometry) value, persistence.getConnection()), Hibernate.CHAR_ARRAY);
			}
			else if (AttributeType.integer.equals(type)) {
				result.setParameter(name, value, Hibernate.INTEGER);
			}
			else if (AttributeType.longInteger.equals(type)) {
				result.setParameter(name,  value, Hibernate.LONG);
			}
			else if (AttributeType.time.equals(type)) {
				result.setTime(name, (Time) value);
			}
			else if (AttributeType.timestamp.equals(type)) {
				result.setTimestamp(name, (Date) value);
			}
			else {
				result.setParameter(name, value);
			}
		}
		
		return result;
	}
}

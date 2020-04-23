package org.skyve.impl.persistence.hibernate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.hibernate.query.NativeQuery;
import org.hibernate.Session;
import org.hibernate.type.BigDecimalType;
import org.hibernate.type.BooleanType;
import org.hibernate.type.DateType;
import org.hibernate.type.IntegerType;
import org.hibernate.type.LongType;
import org.hibernate.type.StringType;
import org.hibernate.type.TextType;
import org.hibernate.type.TimeType;
import org.hibernate.type.TimestampType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractSQL;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;

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
	public <T extends Bean> List<T> beanResults() {
		String moduleName = getModuleName();
		String documentName = getDocumentName();
		
		if ((moduleName == null) || (documentName == null)) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		try {
			String entityName = persistence.getDocumentEntityName(moduleName, documentName);
			NativeQuery<T> query = createQueryFromSQL();
			return query.addEntity(entityName).list();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
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
	public <T> List<T> scalarResults(Class<T> type) {
		try {
			NativeQuery<T> query = createQueryFromSQL();
			List<T> results = query.list();
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
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		try {
			return new HibernateAutoClosingIterable<>(createQueryFromSQL().scroll(), true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public List<Object[]> tupleResults() {
		try {
			List<?> results = createQueryFromSQL().list();
			if ((! results.isEmpty()) && (! (results.get(0) instanceof Object[]))) {
				throw new DomainException("There should be more than 1 projected value in the query when using tupleResults()");
			}
			return (List<Object[]>) results;
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		try {
			return new HibernateAutoClosingIterable<>(createQueryFromSQL().scroll(), false, true);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public int execute() {
		try {
			NativeQuery<?> query = createQueryFromSQL();
			return query.executeUpdate();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}
	
	@SuppressWarnings("resource")
	private <T> NativeQuery<T> createQueryFromSQL() throws Exception {
		Session session = persistence.getSession();
		NativeQuery<T> result = session.createNativeQuery(toQueryString());
		// This ensures that the second level (shared) cache is not invalidated.
		// It means that first and second level caches have to be managed manually in code.
		// NB The "" is a table name that cannot exist.
		result.addSynchronizedQuerySpace("");
		
		for (String name : getParameterNames()) {
			Object value = getParameter(name);
			
			if (value instanceof Decimal) {
				result.setParameter(name, ((Decimal) value).bigDecimalValue(), BigDecimalType.INSTANCE);
				continue;
			}
			else if (value instanceof TimeOnly) {
				result.setParameter(name,  new java.sql.Time(((Date) value).getTime()), TimeType.INSTANCE);
				continue;
			}
			else if ((value instanceof Timestamp) || (value instanceof DateTime)) {
				result.setParameter(name, new java.sql.Timestamp(((Date) value).getTime()), TimestampType.INSTANCE);
				continue;
			}
			else if ((! (value instanceof java.sql.Date)) && (value instanceof Date)) {
				result.setParameter(name, new java.sql.Date(((Date) value).getTime()), DateType.INSTANCE);
				continue;
			}
			else if (value instanceof OptimisticLock) {
				result.setParameter(name, ((OptimisticLock) value).toString(), StringType.INSTANCE);
				continue;
			}
			else if (value instanceof Enumeration) {
				result.setParameter(name, ((Enumeration) value).toCode(), StringType.INSTANCE);
				continue;
			}
			
			AttributeType type = getParameterType(name);

			if (AttributeType.bool.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, BooleanType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, BooleanType.INSTANCE);
				}
				else {
					result.setParameter(name, value, BooleanType.INSTANCE);
				}
			}
			else if (AttributeType.colour.equals(type) ||
						AttributeType.content.equals(type) ||
						AttributeType.image.equals(type) ||
						AttributeType.text.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, StringType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, StringType.INSTANCE);
				}
				else {
					result.setParameter(name, value, StringType.INSTANCE);
				}
			}
			else if (AttributeType.enumeration.equals(type)) {
				if (value instanceof Collection) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Collection<?>) value) {
						param.add((object instanceof Enumeration) ? ((Enumeration) object).toCode() : object);
					}
					result.setParameterList(name, param, StringType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Enumeration) ? ((Enumeration) object).toCode() : object);
					}
					result.setParameterList(name, param, StringType.INSTANCE);
				}
				else {
					result.setParameter(name, (value instanceof Enumeration) ? ((Enumeration) value).toCode() : value, StringType.INSTANCE);
				}
			}
			else if (AttributeType.markup.equals(type) ||
						AttributeType.memo.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, TextType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, TextType.INSTANCE);
				}
				else {
					result.setParameter(name, value, TextType.INSTANCE);
				}
			}
			else if (AttributeType.date.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, DateType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, DateType.INSTANCE);
				}
				else {
					result.setParameter(name, value, DateType.INSTANCE);
				}
			}
			else if (AttributeType.dateTime.equals(type) ||
						AttributeType.timestamp.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, TimestampType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, TimestampType.INSTANCE);
				}
				else {
					result.setParameter(name, value, TimestampType.INSTANCE);
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
					result.setParameterList(name, param, BigDecimalType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Decimal) ? ((Decimal) object).bigDecimalValue() : object);
					}
					result.setParameterList(name, param, BigDecimalType.INSTANCE);
				}
				else {
					result.setParameter(name, (value instanceof Decimal) ? ((Decimal) value).bigDecimalValue() : value, BigDecimalType.INSTANCE);
				}
			}
			else if (AttributeType.geometry.equals(type)) {
				SkyveDialect dialect = AbstractHibernatePersistence.getDialect();
				if (value == null) {
					result.setParameter(name, value, dialect.getGeometryType());
				}
				else {
					if (value instanceof Collection) {
						List<Object> param = new ArrayList<>();
						for (Object object : (Collection<?>) value) {
							param.add(object);
						}
						result.setParameterList(name, param, dialect.getGeometryType());
					}
					else if (value.getClass().isArray()) {
						List<Object> param = new ArrayList<>();
						for (Object object : (Object[]) value) {
							param.add(object);
						}
						result.setParameterList(name, param, dialect.getGeometryType());
					}
					else {
						result.setParameter(name, value, dialect.getGeometryType());
					}
				}
			}
			else if (AttributeType.integer.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, IntegerType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, IntegerType.INSTANCE);
				}
				else {
					result.setParameter(name, value, IntegerType.INSTANCE);
				}
			}
			else if (AttributeType.longInteger.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, LongType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, LongType.INSTANCE);
				}
				else {
					result.setParameter(name, value, LongType.INSTANCE);
				}
			}
			else if (AttributeType.time.equals(type)) {
				if (value instanceof Collection) {
					result.setParameterList(name, (Collection<?>) value, TimeType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					result.setParameterList(name, (Object[]) value, TimeType.INSTANCE);
				}
				else {
					result.setParameter(name, value, TimeType.INSTANCE);
				}
			}
			else if (AttributeType.association.equals(type) ||
						AttributeType.id.equals(type)) {
				if (value instanceof Collection) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Collection<?>) value) {
						param.add((object instanceof Bean) ? ((Bean) object).getBizId() : object);
					}
					result.setParameterList(name, param, StringType.INSTANCE);
				}
				else if ((value != null) && value.getClass().isArray()) {
					List<Object> param = new ArrayList<>();
					for (Object object : (Object[]) value) {
						param.add((object instanceof Bean) ? ((Bean) object).getBizId() : object);
					}
					result.setParameterList(name, param, StringType.INSTANCE);
				}
				else {
					result.setParameter(name, (value instanceof Bean) ? ((Bean) value).getBizId() : value, StringType.INSTANCE);
				}
			}
			else {
				result.setParameter(name, value);
			}
		}
		
		return result;
	}
}

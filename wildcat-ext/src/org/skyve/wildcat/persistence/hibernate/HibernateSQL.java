package org.skyve.wildcat.persistence.hibernate;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.persistence.AbstractSQL;

public class HibernateSQL extends AbstractSQL {
	private AbstractHibernatePersistence persistence;
	
	public HibernateSQL(String moduleName,
							String documentName,
							String query,
							AbstractHibernatePersistence persistence) {
		super(moduleName, documentName, query);
		this.persistence = persistence;
	}

	public HibernateSQL(Document document,
							String query,
							AbstractHibernatePersistence persistence) {
		super(document, query);
		this.persistence = persistence;
	}

	public HibernateSQL(String query,
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
	
	private SQLQuery createQueryFromSQL() {
		Session session = persistence.getSession();
		SQLQuery result = session.createSQLQuery(toQueryString());

		Map<String, Object> parameters = getParameters();
		if (parameters != null) {
			for (Entry<String, Object> entry : parameters.entrySet()) {
				result.setParameter(entry.getKey(), entry.getValue());
			}
		}
		
		return result;
	}
}

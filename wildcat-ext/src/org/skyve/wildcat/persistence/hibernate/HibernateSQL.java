package org.skyve.wildcat.persistence.hibernate;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.persistence.AbstractSQL;

public class HibernateSQL extends AbstractSQL {
	private AbstractHibernatePersistence persistence;
	private String moduleName;
	private String documentName;
	
	public HibernateSQL(String moduleName,
							String documentName,
							String query,
							AbstractHibernatePersistence persistence) {
		super(query);
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.persistence = persistence;
	}

	public HibernateSQL(Document document,
							String query,
							AbstractHibernatePersistence persistence) {
		super(query);
		this.moduleName = document.getOwningModuleName();
		this.documentName = document.getName();
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
		if ((moduleName == null) || (documentName == null)) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		try {
			String entityName = persistence.getDocumentEntityName(moduleName, documentName);
			return new HibernateAutoClosingIterable<T>(createQueryFromSQL().addEntity(entityName).scroll());
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> List<T> projectedResults()
	throws DomainException {
		try {
			Query q = createQueryFromSQL();
			return new HibernateQueryDelegate(persistence).list(q, false, false, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable()
	throws DomainException {
		try {
			SQLQuery query = createQueryFromSQL();
			return new HibernateAutoClosingIterable<T>(getDrivingModuleName(), 
														getDrivingDocumentName(), 
														query.scroll(),
														query.getReturnAliases());
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type)
	throws DomainException {
		try {
			if (assertSingle && (query.getReturnAliases().length != 1)) {
				throw new DomainException("There should be only 1 projected value in the query");
			}
			else if (assertMultiple && (query.getReturnAliases().length <= 1)) {
				throw new DomainException("There should be more than 1 projected value in the query");
			}

			return createQueryFromSQL().list();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type)
	throws DomainException {
		try {
			return new HibernateAutoClosingIterable<T>(createQueryFromSQL().scroll());
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public List<Object[]> tupleResults()
	throws DomainException {
		try {
			return createQueryFromSQL().list();
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable()
	throws DomainException {
		try {
			return new HibernateAutoClosingIterable<Object[]>(createQueryFromSQL().scroll());
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
	
	private SQLQuery createQueryFromSQL()
	throws DomainException {
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

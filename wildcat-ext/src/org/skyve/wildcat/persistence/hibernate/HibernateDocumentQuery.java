package org.skyve.wildcat.persistence.hibernate;

import java.util.List;

import org.hibernate.Query;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.wildcat.persistence.AbstractDocumentQuery;

public class HibernateDocumentQuery extends AbstractDocumentQuery {
	private HibernateQueryDelegate delegate;
	
	public HibernateDocumentQuery(Bean queryByExampleBean, AbstractHibernatePersistence persistence)
	throws Exception {
		super(queryByExampleBean);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(Document document,
									String fromClause,
									String filterClause,
									AbstractHibernatePersistence persistence) {
		super(document, fromClause, filterClause);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(Document document, AbstractHibernatePersistence persistence) {
		super(document);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(String moduleName, 
									String documentName,
									AbstractHibernatePersistence persistence)
	throws MetaDataException {
		super(moduleName, documentName);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	@Override
	public DocumentQuery setFirstResult(int first) {
		delegate.setFirstResult(first);
		return this;
	}

	@Override
	public DocumentQuery setMaxResults(int max) {
		delegate.setMaxResults(max);
		return this;
	}

	@Override
	public <T extends Bean> List<T> beanResults()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.list(query, true, true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.iterate(query, true, true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> List<T> projectedResults()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.list(query, false, false, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.iterate(query, false, false, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type)
	throws DomainException {
		try {
		Query query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type)
	throws DomainException {
		try {
		Query query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, true, false);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public List<Object[]> tupleResults()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.list(query, true, false, true);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable()
	throws DomainException {
		try {
			Query query = delegate.createHibernateQuery(this);
			return delegate.iterate(query, true, false, true);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
	}
}

package org.skyve.impl.persistence.hibernate;

import java.util.List;

import org.hibernate.query.Query;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

public class HibernateDocumentQuery extends AbstractDocumentQuery {
	private HibernateQueryDelegate delegate;
	
	public HibernateDocumentQuery(Bean queryByExampleBean, AbstractHibernatePersistence persistence)
	throws Exception {
		super(queryByExampleBean, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(Document document,
									String fromClause,
									String filterClause,
									AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS(), fromClause, filterClause);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(Document document, AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(String moduleName, 
									String documentName,
									AbstractHibernatePersistence persistence) {
		super(moduleName, documentName, AbstractHibernatePersistence.getDialect().getRDBMS());
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
	public <T extends Bean> List<T> beanResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, true, false);
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, false, false, false);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, false, false, false);
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, true, false);
	}

	@Override
	public List<Object[]> tupleResults() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, false, true);
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, false, true);
	}
}

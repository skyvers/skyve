package org.skyve.impl.persistence.hibernate;

import java.util.List;

import org.hibernate.query.Query;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class HibernateDocumentQuery extends AbstractDocumentQuery {
	private HibernateQueryDelegate delegate;
	
	public HibernateDocumentQuery(@Nonnull Bean queryByExampleBean, @Nonnull AbstractHibernatePersistence persistence)
	throws Exception {
		super(queryByExampleBean, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(@Nonnull Document document,
									@Nullable String fromClause,
									@Nullable String filterClause,
									@Nullable String groupClause,
									@Nullable String orderClause,
									@Nonnull AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS(), fromClause, filterClause, groupClause, orderClause);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(@Nonnull Document document, @Nonnull AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	public HibernateDocumentQuery(@Nonnull String moduleName, 
									@Nonnull String documentName,
									@Nonnull AbstractHibernatePersistence persistence) {
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

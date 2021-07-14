package org.skyve.impl.persistence.hibernate;

import java.util.List;

import org.hibernate.query.Query;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractBizQL;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;

public class HibernateBizQL extends AbstractBizQL {
	private HibernateQueryDelegate delegate;
	
	public HibernateBizQL(String query, 
							AbstractHibernatePersistence persistence) {
		super(query);
		delegate = new HibernateQueryDelegate(persistence);
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

	@Override
	public int execute() {
		return delegate.execute(this);
	}

	@Override
	public BizQL setFirstResult(int first) {
		delegate.setFirstResult(first);
		return this;
	}

	@Override
	public BizQL setMaxResults(int max) {
		delegate.setMaxResults(max);
		return this;
	}
}

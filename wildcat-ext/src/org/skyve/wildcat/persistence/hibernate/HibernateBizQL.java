package org.skyve.wildcat.persistence.hibernate;

import java.util.List;

import org.hibernate.Query;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;
import org.skyve.wildcat.persistence.AbstractBizQL;

public class HibernateBizQL extends AbstractBizQL {
	private HibernateQueryDelegate delegate;
	
	public HibernateBizQL(String query, 
							AbstractHibernatePersistence persistence) {
		super(query);
		delegate = new HibernateQueryDelegate(persistence);
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

	@Override
	public int execute()
	throws DomainException {
		try {
			return delegate.execute(this);
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}
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

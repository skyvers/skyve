package org.skyve.impl.persistence.hibernate;

import java.util.List;

import org.hibernate.query.Query;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractBizQL;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;

import jakarta.annotation.Nonnull;

/**
 * Hibernate implementation of {@link org.skyve.impl.persistence.AbstractBizQL}
 * that executes BizQL (JPQL-dialect) queries via the current Hibernate session.
 */
public class HibernateBizQL extends AbstractBizQL {
	private HibernateQueryDelegate delegate;
	
	/**
	 * Creates a Hibernate-backed BizQL query wrapper.
	 *
	 * @param query the BizQL statement to execute
	 * @param persistence the Hibernate persistence context that will execute the query
	 */
	public HibernateBizQL(@Nonnull String query, 
							@Nonnull AbstractHibernatePersistence persistence) {
		super(query);
		delegate = new HibernateQueryDelegate(persistence);
	}

	/**
	 * Executes the BizQL query and returns document-backed bean results.
	 *
	 * @param <T> the bean type returned by the query
	 * @return the matching bean results
	 */
	@Override
	public <T extends Bean> List<T> beanResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
	}

	/**
	 * Executes the BizQL query and streams document-backed bean results.
	 *
	 * @param <T> the bean type returned by the query
	 * @return an auto-closing iterable of matching bean results
	 */
	@Override
	@SuppressWarnings("resource") // Ownership of the returned AutoClosingIterable passes to the caller.
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, true, false);
	}

	/**
	 * Executes the BizQL query and returns projected bean results.
	 *
	 * @param <T> the projected bean type returned by the query
	 * @return the projected query results
	 */
	@Override
	public <T extends Bean> List<T> projectedResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, false, false, false);
	}

	/**
	 * Executes the BizQL query and streams projected bean results.
	 *
	 * @param <T> the projected bean type returned by the query
	 * @return an auto-closing iterable of projected results
	 */
	@Override
	@SuppressWarnings("resource") // Ownership of the returned AutoClosingIterable passes to the caller.
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, false, false, false);
	}

	/**
	 * Executes the BizQL query and returns scalar results.
	 *
	 * @param <T> the scalar element type
	 * @param type the scalar type expected by callers
	 * @return scalar query results
	 */
	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
	}

	/**
	 * Executes the BizQL query and streams scalar results.
	 *
	 * @param <T> the scalar element type
	 * @param type the scalar type expected by callers
	 * @return an auto-closing iterable of scalar results
	 */
	@Override
	@SuppressWarnings("resource") // Ownership of the returned AutoClosingIterable passes to the caller.
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, true, false);
	}

	/**
	 * Executes the BizQL query and returns tuple projection rows.
	 *
	 * @return tuple rows where each element represents a projected column value
	 */
	@Override
	public List<Object[]> tupleResults() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, false, true);
	}

	/**
	 * Executes the BizQL query and streams tuple projection rows.
	 *
	 * @return an auto-closing iterable of tuple projection rows
	 */
	@Override
	@SuppressWarnings("resource") // Ownership of the returned AutoClosingIterable passes to the caller.
	public AutoClosingIterable<Object[]> tupleIterable() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, false, true);
	}

	/**
	 * Executes the BizQL statement as an update or delete operation.
	 *
	 * @return the number of affected rows
	 */
	@Override
	public int execute() {
		return delegate.execute(this);
	}

	/**
	 * Sets the first row offset for paged query execution.
	 *
	 * @param first the zero-based first result index
	 * @return this query for fluent chaining
	 */
	@Override
	public BizQL setFirstResult(int first) {
		delegate.setFirstResult(first);
		return this;
	}

	/**
	 * Sets the maximum number of rows returned by query execution.
	 *
	 * @param max the maximum number of rows to return
	 * @return this query for fluent chaining
	 */
	@Override
	public BizQL setMaxResults(int max) {
		delegate.setMaxResults(max);
		return this;
	}
}

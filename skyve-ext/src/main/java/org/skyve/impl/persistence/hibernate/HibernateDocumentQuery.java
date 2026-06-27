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

/**
 * Hibernate implementation of {@link org.skyve.impl.persistence.AbstractDocumentQuery}
 * that builds and executes JPA Criteria or JPQL queries for Skyve document retrieval.
 */
public class HibernateDocumentQuery extends AbstractDocumentQuery {
	private HibernateQueryDelegate delegate;
	
	/**
	 * Creates a document query using a query-by-example bean.
	 *
	 * @param queryByExampleBean the example bean whose non-null state seeds the query criteria
	 * @param persistence the Hibernate persistence context that will execute the query
	 */
	public HibernateDocumentQuery(@Nonnull Bean queryByExampleBean, @Nonnull AbstractHibernatePersistence persistence) {
		super(queryByExampleBean, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	/**
	 * Creates a document query from explicit clause fragments.
	 *
	 * @param document the document being queried
	 * @param fromClause optional additional {@code from} clause content
	 * @param filterClause optional filter clause content
	 * @param groupClause optional group-by clause content
	 * @param orderClause optional order-by clause content
	 * @param persistence the Hibernate persistence context that will execute the query
	 */
	public HibernateDocumentQuery(@Nonnull Document document,
									@Nullable String fromClause,
									@Nullable String filterClause,
									@Nullable String groupClause,
									@Nullable String orderClause,
									@Nonnull AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS(), fromClause, filterClause, groupClause, orderClause);
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	/**
	 * Creates a document query for the supplied document.
	 *
	 * @param document the document being queried
	 * @param persistence the Hibernate persistence context that will execute the query
	 */
	public HibernateDocumentQuery(@Nonnull Document document, @Nonnull AbstractHibernatePersistence persistence) {
		super(document, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	/**
	 * Creates a document query by module and document name.
	 *
	 * @param moduleName the owning module name
	 * @param documentName the document name within {@code moduleName}
	 * @param persistence the Hibernate persistence context that will execute the query
	 */
	public HibernateDocumentQuery(@Nonnull String moduleName, 
									@Nonnull String documentName,
									@Nonnull AbstractHibernatePersistence persistence) {
		super(moduleName, documentName, AbstractHibernatePersistence.getDialect().getRDBMS());
		this.delegate = new HibernateQueryDelegate(persistence);
	}

	/**
	 * Sets the first row offset used when executing this query.
	 *
	 * @param first the zero-based first result index
	 * @return this query for fluent chaining
	 */
	@Override
	public DocumentQuery setFirstResult(int first) {
		delegate.setFirstResult(first);
		return this;
	}

	/**
	 * Sets the maximum number of rows returned by this query.
	 *
	 * @param max the maximum number of results to return
	 * @return this query for fluent chaining
	 */
	@Override
	public DocumentQuery setMaxResults(int max) {
		delegate.setMaxResults(max);
		return this;
	}

	/**
	 * Executes the document query and returns document-backed bean results.
	 *
	 * @param <T> the bean type returned by the query
	 * @return matching bean results
	 */
	@Override
	public <T extends Bean> List<T> beanResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, true, false);
	}

	/**
	 * Executes the document query and streams document-backed bean results.
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
	 * Executes the document query and returns projected bean results.
	 *
	 * @param <T> the projected bean type returned by the query
	 * @return projected query results
	 */
	@Override
	public <T extends Bean> List<T> projectedResults() {
		Query<T> query = delegate.createHibernateQuery(this);
		return delegate.list(query, false, false, false);
	}

	/**
	 * Executes the document query and streams projected bean results.
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
	 * Executes the document query and returns scalar results.
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
	 * Executes the document query and streams scalar results.
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
	 * Executes the document query and returns tuple projection rows.
	 *
	 * @return tuple rows where each element represents a projected column value
	 */
	@Override
	public List<Object[]> tupleResults() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.list(query, true, false, true);
	}

	/**
	 * Executes the document query and streams tuple projection rows.
	 *
	 * @return an auto-closing iterable of tuple projection rows
	 */
	@Override
	@SuppressWarnings("resource") // Ownership of the returned AutoClosingIterable passes to the caller.
	public AutoClosingIterable<Object[]> tupleIterable() {
		Query<Object[]> query = delegate.createHibernateQuery(this);
		return delegate.iterate(query, true, false, true);
	}
}

package org.skyve.persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Skyve's object-oriented query language for retrieving domain beans.
 *
 * <p>BizQL resembles JPQL / HQL and operates directly over domain bean types
 * (e.g. {@code FROM admin.User AS u WHERE u.active = true}). Because it references
 * domain types rather than SQL tables, BizQL queries are portable across supported
 * databases and automatically receive Skyve row-level security filtering.
 *
 * <p>BizQL supports all result shapes: bean retrieval ({@link BeanQuery}),
 * projected beans ({@link ProjectedQuery}), scalars ({@link ScalarQuery}),
 * tuples ({@link TupleQuery}), pagination ({@link PagedQuery}), and
 * data modification ({@link DMLQuery} — for UPDATE/DELETE statements).
 *
 * <p>Named parameters are bound with {@link #putParameter(String, Object)}.
 * A query with a parameter {@code :userId} is bound as
 * {@code query.putParameter("userId", someId)}.
 *
 * <p>Instances are created via {@link Persistence#newBizQL(String)} and its overloads.
 * Not thread-safe; use within a single request thread.
 *
 * @see Persistence#newBizQL(String)
 * @see Persistence#newNamedBizQL(String, String)
 */
public interface BizQL extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery, DMLQuery {
	/**
	 * Binds a named query parameter.
	 *
	 * @param name  the parameter name as it appears in the query string (without the colon)
	 * @param value the value to bind; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull BizQL putParameter(@Nonnull String name, @Nullable Object value);
	
	@Override
	@Nonnull BizQL setFirstResult(int first);
	
	@Override
	@Nonnull BizQL setMaxResults(int max);
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull BizQL noTimeout();
}

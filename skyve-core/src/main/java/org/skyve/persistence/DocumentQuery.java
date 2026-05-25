package org.skyve.persistence;

import org.skyve.domain.DynamicBean;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Metadata-driven query builder for retrieving {@link org.skyve.domain.Bean} instances.
 *
 * <p>A {@code DocumentQuery} is constructed from a driving {@link Document} and
 * builds a JPQL SELECT statement under the hood. It supports:
 * <ul>
 *   <li><b>Projections</b> — individual bindings ({@link #addBoundProjection}), raw JPQL
 *       expressions ({@link #addExpressionProjection}), aggregate functions
 *       ({@link #addAggregateProjection}), and a {@code THIS} projection that returns the
 *       full entity ({@link #addThisProjection}) for polymorphic beans.
 *   <li><b>Joins</b> — inner, left outer, and right outer joins on associations and
 *       collections, with optional fetch semantics.
 *   <li><b>Filtering</b> — via {@link #getFilter()} which returns a {@link DocumentFilter}
 *       for type-safe predicate construction.
 *   <li><b>Ordering and grouping</b> — via bound bindings or raw expressions.
 *   <li><b>Result shaping</b> — implements {@link BeanQuery}, {@link ProjectedQuery},
 *       {@link ScalarQuery}, {@link TupleQuery}, and {@link PagedQuery}.
 * </ul>
 *
 * <p>All builder methods return {@code this} for fluent chaining.
 *
 * <p>Created via {@link Persistence#newDocumentQuery(Document)} and its overloads.
 *
 * <p>Projected query results are returned as {@link org.skyve.domain.DynamicBean}
 * instances keyed by the projection alias. The special alias {@link #THIS_ALIAS}
 * ({@value #THIS_ALIAS}) is used when the full entity bean is projected.
 *
 * <p>Threading: not thread-safe; use within a single request thread.
 *
 * @see DocumentFilter
 * @see Persistence#newDocumentQuery(Document)
 */
public interface DocumentQuery extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery {
	/** The alias used for the root entity in projections and the {@link org.skyve.domain.DynamicBean} key for the full entity bean. */
	public static final String THIS_ALIAS = DynamicBean.BEAN_PROPERTY_KEY;

	/** Aggregate functions supported in {@link DocumentQuery#addAggregateProjection}. */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum AggregateFunction {
		Min,
		Max, 
		Sum, 
		Count,
		Avg
	}

	/**
	 * Binds a named query parameter.
	 *
	 * @param name  the parameter name as it appears in the query string (without the colon prefix)
	 * @param value the parameter value; may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery putParameter(@Nonnull String name, @Nullable Object value);
	@Override
	@Nonnull DocumentQuery setFirstResult(int first);
	@Override
	@Nonnull DocumentQuery setMaxResults(int max);
	
	/**
	 * Returns whether the query uses {@code SELECT DISTINCT}.
	 *
	 * @return {@code true} if distinct rows are requested
	 */
	boolean isDistinct();
	
	/**
	 * Sets whether the query uses {@code SELECT DISTINCT}.
	 *
	 * @param distinct {@code true} to add DISTINCT to the SELECT clause
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery setDistinct(boolean distinct);
	
	/**
	 * Adds a {@code THIS} projection that returns the full root entity bean.
	 *
	 * <p>Required for polymorphic queries where the returned beans may be subtype
	 * instances. The projected result is placed in the {@link DynamicBean} under
	 * the {@link #THIS_ALIAS} key.
	 *
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addThisProjection();
	
	/**
	 * Adds a projection for the given binding, using the binding as the column alias.
	 *
	 * @param binding the attribute binding path; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String binding);
	
	/**
	 * Adds a projection for the given binding with an explicit column alias.
	 *
	 * @param binding         the attribute binding path; must not be {@code null}
	 * @param projectedAlias  the alias to use in the result {@link org.skyve.domain.DynamicBean};
	 *                        must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * Adds a projection for the given binding from the specified entity alias with an explicit column alias.
	 *
	 * @param entityAlias    the JPQL alias of the entity to project from; must not be {@code null}
	 * @param binding        the attribute binding path; must not be {@code null}
	 * @param projectedAlias the alias to use in the result; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * Adds a raw JPQL expression as a projection with an explicit alias.
	 *
	 * @param expression    a JPQL expression (e.g. {@code "UPPER(bean.name)"}); must not be {@code null}
	 * @param projectedAlias the alias to use in the result; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addExpressionProjection(@Nonnull String expression, @Nonnull String projectedAlias);
	
	/**
	 * Adds an aggregate function projection on the given binding.
	 *
	 * @param function       the aggregate function to apply; must not be {@code null}
	 * @param binding        the attribute binding path; must not be {@code null}
	 * @param projectedAlias the alias to use in the result; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addAggregateProjection(@Nonnull AggregateFunction function, @Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * Adds an aggregate function projection on the given binding from the specified entity alias.
	 *
	 * @param function       the aggregate function to apply; must not be {@code null}
	 * @param entityAlias    the JPQL alias of the entity; must not be {@code null}
	 * @param binding        the attribute binding path; must not be {@code null}
	 * @param projectedAlias the alias to use in the result; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addAggregateProjection(@Nonnull AggregateFunction function,
													@Nonnull String entityAlias,
													@Nonnull String binding,
													@Nonnull String projectedAlias);

	/**
	 * Returns the {@link DocumentFilter} for this query.
	 *
	 * <p>The filter is populated by calling its {@code add*} methods and is included
	 * in the query's WHERE clause. The filter is an AND of all added conditions by default.
	 *
	 * @return the filter; never {@code null}
	 */
	@Nonnull DocumentFilter getFilter();
	
	/**
	 * Appends an ORDER BY clause for the given binding in ascending order.
	 *
	 * @param binding the attribute binding path; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String binding);

	/**
	 * Appends an ORDER BY clause for the given binding from the specified entity alias,
	 * in ascending order.
	 *
	 * @param entityAlias the JPQL alias of the entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * Appends an ORDER BY clause for the given binding with explicit sort direction.
	 *
	 * @param binding the attribute binding path; must not be {@code null}
	 * @param order   the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String binding, @Nonnull SortDirection order);
	
	/**
	 * Appends an ORDER BY clause for the given binding from the specified entity alias
	 * with explicit sort direction.
	 *
	 * @param entityAlias the JPQL alias of the entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param order       the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * Inserts an ORDER BY clause at the beginning of the ORDER BY list for the given
	 * binding. Use this to prepend a sort ahead of any already-added orderings.
	 *
	 * @param binding the attribute binding path; must not be {@code null}
	 * @param order   the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery insertBoundOrdering(@Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * Inserts an ORDER BY clause at the beginning of the ORDER BY list for the given
	 * binding from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @param order       the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery insertBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * Appends a GROUP BY clause for the given binding.
	 *
	 * @param binding the attribute binding path; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundGrouping(@Nonnull String binding);

	/**
	 * Appends a GROUP BY clause for the given binding from the specified entity alias.
	 *
	 * @param entityAlias the JPQL alias of the entity; must not be {@code null}
	 * @param binding     the attribute binding path; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addBoundGrouping(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * Appends an ORDER BY clause using a raw JPQL expression in ascending order.
	 *
	 * @param expression a JPQL expression; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addExpressionOrdering(@Nonnull String expression);
	
	/**
	 * Appends an ORDER BY clause using a raw JPQL expression with explicit sort direction.
	 *
	 * @param expression a JPQL expression; must not be {@code null}
	 * @param order      the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addExpressionOrdering(@Nonnull String expression, @Nonnull SortDirection order);
	
	/**
	 * Inserts an ORDER BY clause at the beginning of the ORDER BY list using a raw JPQL
	 * expression.
	 *
	 * @param expression a JPQL expression; must not be {@code null}
	 * @param order      the sort direction; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery insertExpressionOrdering(@Nonnull String expression, @Nonnull SortDirection order);
	
	/**
	 * Appends a GROUP BY clause using a raw JPQL expression.
	 *
	 * @param expression a JPQL expression; must not be {@code null}
	 * @return this query for fluent chaining
	 */
	@Nonnull DocumentQuery addExpressionGrouping(@Nonnull String expression);
	
	/**
	 * Inner join an association or collection.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addInnerJoin(@Nonnull String referenceBinding);

	/**
	 * Inner join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addInnerJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding);

	/**
	 * Left Outer join an association or collection.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addLeftOuterJoin(@Nonnull String referenceBinding);
	
	/**
	 * Left Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addLeftOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding);

	/**
	 * Right Outer join an association or collection.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addRightOuterJoin(@Nonnull String referenceBinding);

	/**
	 * Right Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addRightOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedInnerJoin(@Nonnull String referenceBinding);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedInnerJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedLeftOuterJoin(@Nonnull String referenceBinding);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedLeftOuterJoinFromEntity(@Nonnull String entityName, @Nonnull String referenceBinding);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedRightOuterJoin(@Nonnull String referenceBinding);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	@Nonnull DocumentQuery addFetchedRightOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding);

	/**
	 * Inner join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addInnerJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Inner join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addInnerJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Left Outer join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addLeftOuterJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);
	
	/**
	 * Left Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addLeftOuterJoinFromEntity(@Nonnull String entityName, @Nonnull String referenceBinding, @Nonnull String joinAlias);
	
	/**
	 * Right Outer join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addRightOuterJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Right Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addRightOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedInnerJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);
	
	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedInnerJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedLeftOuterJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);
	
	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedLeftOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedRightOuterJoin(@Nonnull String referenceBinding, @Nonnull String joinAlias);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	@Nonnull DocumentQuery addFetchedRightOuterJoinFromEntity(@Nonnull String entityAlias, @Nonnull String referenceBinding, @Nonnull String joinAlias);
	
	/**
	 * Returns the driving {@link Document} for this query.
	 *
	 * @return the document; never {@code null}
	 */
	@Nonnull Document getDrivingDocument();
	
	/**
	 * Creates a new {@link DocumentFilter} that can be composed with the current
	 * filter using boolean logic.
	 *
	 * @return a new empty filter; never {@code null}
	 */
	@Nonnull DocumentFilter newDocumentFilter();
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull DocumentQuery noTimeout();
}

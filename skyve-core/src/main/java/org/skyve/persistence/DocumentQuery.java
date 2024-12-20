package org.skyve.persistence;

import org.skyve.domain.DynamicBean;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
public interface DocumentQuery extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery {
	/**
	 * 
	 */
	public static final String THIS_ALIAS = DynamicBean.BEAN_PROPERTY_KEY;

	/**
	 * 
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum AggregateFunction {
		Min,
		Max, 
		Sum, 
		Count,
		Avg
	}

	@Nonnull DocumentQuery putParameter(@Nonnull String name, @Nullable Object value);
	@Override
	@Nonnull DocumentQuery setFirstResult(int first);
	@Override
	@Nonnull DocumentQuery setMaxResults(int max);
	
	/**
	 * 
	 * @return
	 */
	boolean isDistinct();
	
	/**
	 * 
	 * @param distinct
	 */
	@Nonnull DocumentQuery setDistinct(boolean distinct);
	
	/**
	 * 
	 */
	@Nonnull DocumentQuery addThisProjection();
	
	/**
	 * 
	 * @param binding
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String binding);
	
	/**
	 * 
	 * @param binding
	 * @param projectedAlias
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param projectedAlias
	 */
	@Nonnull DocumentQuery addBoundProjection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * 
	 * @param expression
	 * @param projectedAlias
	 */
	@Nonnull DocumentQuery addExpressionProjection(@Nonnull String expression, @Nonnull String projectedAlias);
	
	/**
	 * 
	 * @param function
	 * @param binding
	 * @param projectedAlias
	 */
	@Nonnull DocumentQuery addAggregateProjection(@Nonnull AggregateFunction function, @Nonnull String binding, @Nonnull String projectedAlias);

	/**
	 * 
	 * @param function
	 * @param entityAlias
	 * @param binding
	 * @param projectedAlias
	 */
	@Nonnull DocumentQuery addAggregateProjection(@Nonnull AggregateFunction function,
													@Nonnull String entityAlias,
													@Nonnull String binding,
													@Nonnull String projectedAlias);

	/**
	 * 
	 * @return
	 */
	@Nonnull DocumentFilter getFilter();
	
	/**
	 * 
	 * @param binding
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String binding);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * 
	 * @param binding
	 * @param order
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String binding, @Nonnull SortDirection order);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param order
	 */
	@Nonnull DocumentQuery addBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * 
	 * @param binding
	 * @param order
	 */
	@Nonnull DocumentQuery insertBoundOrdering(@Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param order
	 */
	@Nonnull DocumentQuery insertBoundOrdering(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull SortDirection order);

	/**
	 * 
	 * @param binding
	 */
	@Nonnull DocumentQuery addBoundGrouping(@Nonnull String binding);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	@Nonnull DocumentQuery addBoundGrouping(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * 
	 * @param expression
	 */
	@Nonnull DocumentQuery addExpressionOrdering(@Nonnull String expression);
	
	/**
	 * 
	 * @param expression
	 * @param order
	 */
	@Nonnull DocumentQuery addExpressionOrdering(@Nonnull String expression, @Nonnull SortDirection order);
	
	/**
	 * 
	 * @param expression
	 * @param order
	 */
	@Nonnull DocumentQuery insertExpressionOrdering(@Nonnull String expression, @Nonnull SortDirection order);
	
	/**
	 * 
	 * @param expression
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
	 * 
	 * @return
	 */
	@Nonnull Document getDrivingDocument();
	
	/**
	 * 
	 * @return
	 */
	@Nonnull DocumentFilter newDocumentFilter();
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull DocumentQuery noTimeout();
}

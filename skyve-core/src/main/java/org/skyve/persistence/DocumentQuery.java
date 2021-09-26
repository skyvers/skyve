package org.skyve.persistence;

import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.MapBean;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;

/**
 * 
 */
public interface DocumentQuery extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery {
	/**
	 * 
	 */
	public static final String THIS_ALIAS = MapBean.BEAN_PROPERTY_KEY;

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

	public DocumentQuery putParameter(String name, Object value);
	@Override
	public DocumentQuery setFirstResult(int first);
	@Override
	public DocumentQuery setMaxResults(int max);
	
	/**
	 * 
	 * @return
	 */
	public boolean isDistinct();
	
	/**
	 * 
	 * @param distinct
	 */
	public DocumentQuery setDistinct(boolean distinct);
	
	/**
	 * 
	 */
	public DocumentQuery addThisProjection();
	
	/**
	 * 
	 * @param binding
	 */
	public DocumentQuery addBoundProjection(String binding);
	
	/**
	 * 
	 * @param binding
	 * @param projectedAlias
	 */
	public DocumentQuery addBoundProjection(String binding, String projectedAlias);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param projectedAlias
	 */
	public DocumentQuery addBoundProjection(String entityAlias, String binding, String projectedAlias);

	/**
	 * 
	 * @param expression
	 * @param projectedAlias
	 */
	public DocumentQuery addExpressionProjection(String expression, String projectedAlias);
	
	/**
	 * 
	 * @param function
	 * @param binding
	 * @param projectedAlias
	 */
	public DocumentQuery addAggregateProjection(AggregateFunction function, String binding, String projectedAlias);

	/**
	 * 
	 * @param function
	 * @param entityAlias
	 * @param binding
	 * @param projectedAlias
	 */
	public DocumentQuery addAggregateProjection(AggregateFunction function, String entityAlias, String binding, String projectedAlias);

	/**
	 * 
	 * @return
	 */
	public DocumentFilter getFilter();
	
	/**
	 * 
	 * @param binding
	 */
	public DocumentQuery addBoundOrdering(String binding);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	public DocumentQuery addBoundOrdering(String entityAlias, String binding);

	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public DocumentQuery addBoundOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param order
	 */
	public DocumentQuery addBoundOrdering(String entityAlias, String binding, SortDirection order);

	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public DocumentQuery insertBoundOrdering(String binding, SortDirection order);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param order
	 */
	public DocumentQuery insertBoundOrdering(String entityAlias, String binding, SortDirection order);

	/**
	 * 
	 * @param binding
	 */
	public DocumentQuery addBoundGrouping(String binding);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	public DocumentQuery addBoundGrouping(String entityAlias, String binding);

	/**
	 * 
	 * @param expression
	 */
	public DocumentQuery addExpressionOrdering(String expression);
	
	/**
	 * 
	 * @param expression
	 * @param order
	 */
	public DocumentQuery addExpressionOrdering(String expression, SortDirection order);
	
	/**
	 * 
	 * @param expression
	 * @param order
	 */
	public DocumentQuery insertExpressionOrdering(String expression, SortDirection order);
	
	/**
	 * 
	 * @param expression
	 */
	public DocumentQuery addExpressionGrouping(String expression);
	
	/**
	 * Inner join an association or collection.
	 * @param referenceBinding
	 */
	public DocumentQuery addInnerJoin(String referenceBinding);

	/**
	 * Inner join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding);

	/**
	 * Left Outer join an association or collection.
	 * @param referenceBinding
	 */
	public DocumentQuery addLeftOuterJoin(String referenceBinding);
	
	/**
	 * Left Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addLeftOuterJoinFromEntity(String entityAlias, String referenceBinding);

	/**
	 * Right Outer join an association or collection.
	 * @param referenceBinding
	 */
	public DocumentQuery addRightOuterJoin(String referenceBinding);

	/**
	 * Right Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedInnerJoin(String referenceBinding);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityName, String referenceBinding);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding);

	/**
	 * Inner join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addInnerJoin(String referenceBinding, String joinAlias);

	/**
	 * Inner join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias);

	/**
	 * Left Outer join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addLeftOuterJoin(String referenceBinding, String joinAlias);
	
	/**
	 * Left Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addLeftOuterJoinFromEntity(String entityName, String referenceBinding, String joinAlias);
	
	/**
	 * Right Outer join an association or collection.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addRightOuterJoin(String referenceBinding, String joinAlias);

	/**
	 * Right Outer join an association or collection.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedInnerJoin(String referenceBinding, String joinAlias);
	
	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias);

	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding, String joinAlias);
	
	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding, String joinAlias);

	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param entityAlias
	 * @param referenceBinding
	 * @param joinAlias	The join alias
	 */
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias);
	
	/**
	 * 
	 * @return
	 */
	public Document getDrivingDocument();
	
	/**
	 * 
	 * @return
	 */
	public DocumentFilter newDocumentFilter();
	
	public int getTimeoutInSeconds();
	public void setTimeoutInSeconds(int timeoutInSeconds);
}

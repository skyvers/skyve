package org.skyve.persistence;

import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;

/**
 * 
 */
public interface DocumentQuery extends BeanQuery, ProjectedQuery, ScalarQuery, TupleQuery, PagedQuery {
	/**
	 * 
	 */
	public static final String THIS_ALIAS = "bean";

	/**
	 * 
	 */
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
	 * @param alias
	 */
	public DocumentQuery addBoundProjection(String binding, String alias);
	
	/**
	 * 
	 * @param expression
	 * @param alias
	 */
	public DocumentQuery addExpressionProjection(String expression, String alias);
	
	/**
	 * 
	 * @param function
	 * @param binding
	 * @param alias
	 */
	public DocumentQuery addAggregateProjection(AggregateFunction function, String binding, String alias);
	
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
	 * @param binding
	 * @param order
	 */
	public DocumentQuery addBoundOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public DocumentQuery insertBoundOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 */
	public DocumentQuery addBoundGrouping(String binding);
	
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
	 * Left Outer join an association or collection.
	 * @param referenceBinding
	 */
	public DocumentQuery addLeftOuterJoin(String referenceBinding);
	
	/**
	 * Right Outer join an association or collection.
	 * @param referenceBinding
	 */
	public DocumentQuery addRightOuterJoin(String referenceBinding);

	/**
	 * Inner join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedInnerJoin(String referenceBinding);
	
	/**
	 * Left Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding);
	
	/**
	 * Right Outer join an association or collection and fetch the domain bean(s) at the same time.
	 * @param referenceBinding
	 */
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding);

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
}

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
	public DocumentQuery addOrdering(String binding);
	
	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public DocumentQuery addOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public DocumentQuery insertOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 */
	public DocumentQuery addGrouping(String binding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public DocumentQuery addInnerJoin(String associationBinding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public DocumentQuery addLeftOuterJoin(String associationBinding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public DocumentQuery addRightOuterJoin(String associationBinding);

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

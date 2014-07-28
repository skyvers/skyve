package org.skyve.persistence;

import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Document;

/**
 * 
 */
public interface DocumentQuery extends Query {
	/**
	 * 
	 */
	public static final String THIS_ALIAS = "bean";

	/**
	 * 
	 */
	public static enum AggregateFunction {
		/**
		 * 
		 */
		Min,
		
		/**
		 * 
		 */
		Max, 
		
		/**
		 * 
		 */
		Sum, 
		
		/**
		 * 
		 */
		Count,
		
		/**
		 * 
		 */
		Avg
	}

	/**
	 * 
	 * @return
	 */
	public boolean isDistinct();
	
	/**
	 * 
	 * @param distinct
	 */
	public void setDistinct(boolean distinct);
	
	/**
	 * 
	 */
	public void addThisProjection();
	
	/**
	 * 
	 * @param binding
	 */
	public void addBoundProjection(String binding);
	
	/**
	 * 
	 * @param binding
	 * @param alias
	 */
	public void addBoundProjection(String binding, String alias);
	
	/**
	 * 
	 * @param expression
	 * @param alias
	 */
	public void addExpressionProjection(String expression, String alias);
	
	/**
	 * 
	 * @param function
	 * @param binding
	 * @param alias
	 */
	public void addAggregateProjection(AggregateFunction function, String binding, String alias);
	
	/**
	 * 
	 * @return
	 */
	public DocumentFilter getFilter();
	
	/**
	 * 
	 * @param binding
	 */
	public void addOrdering(String binding);
	
	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public void addOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 * @param order
	 */
	public void insertOrdering(String binding, SortDirection order);
	
	/**
	 * 
	 * @param binding
	 */
	public void addGrouping(String binding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public void addInnerJoin(String associationBinding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public void addLeftOuterJoin(String associationBinding);
	
	/**
	 * 
	 * @param associationBinding
	 */
	public void addRightOuterJoin(String associationBinding);

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

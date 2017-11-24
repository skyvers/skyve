package org.skyve.persistence;

import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 */
public interface DocumentFilter {
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	public void addIn(String binding, Object...operands);
	
	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	public void addNotIn(String binding, Object... operands);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNotEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addGreaterThan(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addGreaterThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addLessThan(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addLessThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addLike(String binding, String operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNotLike(String binding, String operand);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addEquals(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addDisjoint(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addIntersects(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addTouches(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addCrosses(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addWithin(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addContains(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addOverlaps(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrNotEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrGreaterThan(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrGreaterThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrLessThan(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrLessThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrLike(String binding, String operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addNullOrNotLike(String binding, String operand);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrEquals(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrDisjoint(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrIntersects(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrTouches(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrCrosses(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrWithin(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrContains(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public void addNullOrOverlaps(String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 */
	public void addNull(String binding);
	
	/**
	 * 
	 * @param binding
	 */
	public void addNotNull(String binding);
	
	/**
	 * 
	 * @param binding
	 * @param minOperand
	 * @param maxOperand
	 */
	public void addBetween(String binding, Object minOperand, Object maxOperand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeEquals(String binding, int operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeNotEquals(String binding, int operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeGreaterThan(String binding, int operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeGreaterThanOrEqualTo(String binding, int operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeLessThan(String binding, int operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public void addCollectionSizeLessThanOrEqualTo(String binding, int operand);

	/**
	 * 
	 * @param filter
	 */
	public void addAnd(DocumentFilter filter);
	
	/**
	 * 
	 * @param filter
	 */
	public void addOr(DocumentFilter filter);
	
	/**
	 * 
	 * @param expression
	 */
	public void addExpression(String expression);
	
	public boolean isEmpty();
}

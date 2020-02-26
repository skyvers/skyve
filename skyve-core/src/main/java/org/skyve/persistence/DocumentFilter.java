package org.skyve.persistence;

import org.locationtech.jts.geom.Geometry;

/**
 * 
 */
public interface DocumentFilter {
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addEquals(String binding, Object operand);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addEquals(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	public DocumentFilter addIn(String binding, Object...operands);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operands
	 */
	public DocumentFilter addIn(String entityAlias, String binding, Object...operands);

	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	public DocumentFilter addNotIn(String binding, Object... operands);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operands
	 */
	public DocumentFilter addNotIn(String entityAlias, String binding, Object... operands);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNotEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNotEquals(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addGreaterThan(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addGreaterThan(String entityAlias, String binding, Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addGreaterThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addGreaterThanOrEqualTo(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLessThan(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLessThan(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLessThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLessThanOrEqualTo(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLike(String binding, String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addLike(String entityAlias, String binding, String operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNotLike(String binding, String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNotLike(String entityAlias, String binding, String operand);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addEquals(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addEquals(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addDisjoint(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addDisjoint(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addIntersects(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addIntersects(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addTouches(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addTouches(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addCrosses(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addCrosses(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addWithin(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addWithin(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addContains(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addContains(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addOverlaps(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addOverlaps(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrEquals(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrNotEquals(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrNotEquals(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrGreaterThan(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrGreaterThan(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrGreaterThanOrEqualTo(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLessThan(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLessThan(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLessThanOrEqualTo(String binding, Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLessThanOrEqualTo(String entityAlias, String binding, Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLike(String binding, String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrLike(String entityAlias, String binding, String operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrNotLike(String binding, String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addNullOrNotLike(String entityAlias, String binding, String operand);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrEquals(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrEquals(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrDisjoint(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrDisjoint(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrIntersects(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrIntersects(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrTouches(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrTouches(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrCrosses(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrCrosses(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrWithin(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrWithin(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrContains(String binding, Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrContains(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrOverlaps(String binding, Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	public DocumentFilter addNullOrOverlaps(String entityAlias, String binding, Geometry geometry);

	/**
	 * 
	 * @param binding
	 */
	public DocumentFilter addNull(String binding);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	public DocumentFilter addNull(String entityAlias, String binding);

	/**
	 * 
	 * @param binding
	 */
	public DocumentFilter addNotNull(String binding);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	public DocumentFilter addNotNull(String entityAlias, String binding);

	/**
	 * 
	 * @param binding
	 * @param minOperand
	 * @param maxOperand
	 */
	public DocumentFilter addBetween(String binding, Object minOperand, Object maxOperand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param minOperand
	 * @param maxOperand
	 */
	public DocumentFilter addBetween(String entityAlias, String binding, Object minOperand, Object maxOperand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeEquals(String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeEquals(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeNotEquals(String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeNotEquals(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeGreaterThan(String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeGreaterThan(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeGreaterThanOrEqualTo(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeLessThan(String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeLessThan(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String binding, int operand);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	public DocumentFilter addCollectionSizeLessThanOrEqualTo(String entityAlias, String binding, int operand);

	/**
	 * 
	 * @param filter
	 */
	public DocumentFilter addAnd(DocumentFilter filter);
	
	/**
	 * 
	 * @param filter
	 */
	public DocumentFilter addOr(DocumentFilter filter);
	
	/**
	 * 
	 * @param expression
	 */
	public DocumentFilter addExpression(String expression);
	
	public boolean isEmpty();
}

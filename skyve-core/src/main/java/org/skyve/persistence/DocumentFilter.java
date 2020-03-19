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
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedIn(String entityAlias, String binding, Object...operands);

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
	public DocumentFilter addAliasedNotIn(String entityAlias, String binding, Object... operands);

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
	public DocumentFilter addAliasedNotEquals(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedGreaterThan(String entityAlias, String binding, Object operand);
	
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
	public DocumentFilter addAliasedGreaterThanOrEqualTo(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedLessThan(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedLessThanOrEqualTo(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedLike(String entityAlias, String binding, String operand);

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
	public DocumentFilter addAliasedNotLike(String entityAlias, String binding, String operand);

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
	public DocumentFilter addAliasedEquals(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedDisjoint(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedIntersects(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedTouches(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedCrosses(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedWithin(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedContains(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedOverlaps(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrNotEquals(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrGreaterThan(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrGreaterThanOrEqualTo(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrLessThan(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrLessThanOrEqualTo(String entityAlias, String binding, Object operand);

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
	public DocumentFilter addAliasedNullOrLike(String entityAlias, String binding, String operand);

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
	public DocumentFilter addAliasedNullOrNotLike(String entityAlias, String binding, String operand);

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
	public DocumentFilter addAliasedNullOrEquals(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrDisjoint(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrIntersects(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrTouches(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrCrosses(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrWithin(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrContains(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNullOrOverlaps(String entityAlias, String binding, Geometry geometry);

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
	public DocumentFilter addAliasedNull(String entityAlias, String binding);

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
	public DocumentFilter addAliasedNotNull(String entityAlias, String binding);

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
	public DocumentFilter addAliasedBetween(String entityAlias, String binding, Object minOperand, Object maxOperand);

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
	public DocumentFilter addAliasedCollectionSizeEquals(String entityAlias, String binding, int operand);

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
	public DocumentFilter addAliasedCollectionSizeNotEquals(String entityAlias, String binding, int operand);

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
	public DocumentFilter addAliasedCollectionSizeGreaterThan(String entityAlias, String binding, int operand);

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
	public DocumentFilter addAliasedCollectionSizeGreaterThanOrEqualTo(String entityAlias, String binding, int operand);

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
	public DocumentFilter addAliasedCollectionSizeLessThan(String entityAlias, String binding, int operand);

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
	public DocumentFilter addAliasedCollectionSizeLessThanOrEqualTo(String entityAlias, String binding, int operand);

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

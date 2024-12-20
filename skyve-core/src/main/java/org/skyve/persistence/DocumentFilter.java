package org.skyve.persistence;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;

/**
 * 
 */
public interface DocumentFilter {
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addEquals(@Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	@Nonnull DocumentFilter addIn(@Nonnull String binding, Object...operands);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operands
	 */
	@Nonnull DocumentFilter addAliasedIn (@Nonnull String entityAlias, @Nonnull String binding, Object...operands);

	/**
	 * 
	 * @param binding
	 * @param operands
	 */
	@Nonnull DocumentFilter addNotIn(@Nonnull String binding, Object... operands);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operands
	 */
	@Nonnull DocumentFilter addAliasedNotIn(@Nonnull String entityAlias, @Nonnull String binding, Object... operands);

	/** 
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNotEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNotEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addGreaterThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addLessThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedLessThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNotLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNotLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addEquals(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addDisjoint(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedDisjoint(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addIntersects(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedIntersects(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addTouches(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedTouches(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addCrosses(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedCrosses(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addWithin(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedWithin(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addContains(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedContains(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addOverlaps(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedOverlaps(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrNotEquals(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrNotEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrGreaterThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrLessThan(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrLessThan(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrLessThanOrEqualTo(@Nonnull String binding, @Nonnull Object operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addNullOrNotLike(@Nonnull String binding, @Nonnull String operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedNullOrNotLike(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull String operand);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrEquals(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrEquals(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrDisjoint(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrDisjoint(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrIntersects(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrIntersects(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrTouches(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrTouches(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrCrosses(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrCrosses(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrWithin(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrWithin(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrContains(@Nonnull String binding, @Nonnull Geometry geometry);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrContains(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addNullOrOverlaps(@Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param geometry
	 */
	@Nonnull DocumentFilter addAliasedNullOrOverlaps(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Geometry geometry);

	/**
	 * 
	 * @param binding
	 */
	@Nonnull DocumentFilter addNull(@Nonnull String binding);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	@Nonnull DocumentFilter addAliasedNull(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * 
	 * @param binding
	 */
	@Nonnull DocumentFilter addNotNull(@Nonnull String binding);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 */
	@Nonnull DocumentFilter addAliasedNotNull(@Nonnull String entityAlias, @Nonnull String binding);

	/**
	 * 
	 * @param binding
	 * @param minOperand
	 * @param maxOperand
	 */
	@Nonnull DocumentFilter addBetween(@Nonnull String binding, @Nonnull Object minOperand, @Nonnull Object maxOperand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param minOperand
	 * @param maxOperand
	 */
	@Nonnull DocumentFilter addAliasedBetween(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Object minOperand, @Nonnull Object maxOperand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeEquals(@Nonnull String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeEquals(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeNotEquals(@Nonnull String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeNotEquals(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeGreaterThan(@Nonnull String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeGreaterThan(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeGreaterThanOrEqualTo(@Nonnull String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeGreaterThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeLessThan(@Nonnull String binding, int operand);
	
	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeLessThan(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	/**
	 * 
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addCollectionSizeLessThanOrEqualTo(@Nonnull String binding, int operand);

	/**
	 * 
	 * @param entityAlias
	 * @param binding
	 * @param operand
	 */
	@Nonnull DocumentFilter addAliasedCollectionSizeLessThanOrEqualTo(@Nonnull String entityAlias, @Nonnull String binding, int operand);

	@Nonnull DocumentFilter addMemberOfCollection(@Nonnull String binding, @Nonnull Bean operand);
	
	@Nonnull DocumentFilter addNotMemberOfCollection(@Nonnull String binding, @Nonnull Bean operand);
	
	@Nonnull DocumentFilter addAliasedMemberOfCollection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Bean operand);
	
	@Nonnull DocumentFilter addAliasedNotMemberOfCollection(@Nonnull String entityAlias, @Nonnull String binding, @Nonnull Bean operand);
	
	/**
	 * 
	 * @param filter
	 */
	@Nonnull DocumentFilter addAnd(@Nonnull DocumentFilter filter);
	
	/**
	 * 
	 * @param filter
	 */
	@Nonnull DocumentFilter addOr(@Nonnull DocumentFilter filter);
	
	/**
	 * 
	 * @param expression
	 */
	@Nonnull DocumentFilter addExpression(@Nonnull String expression);
	
	boolean isEmpty();
}

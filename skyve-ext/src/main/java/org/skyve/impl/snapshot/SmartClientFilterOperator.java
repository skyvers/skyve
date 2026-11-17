package org.skyve.impl.snapshot;

import org.skyve.metadata.FilterOperator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Enumerates SmartClient-compatible filter operators used by snapshot serialization.
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum SmartClientFilterOperator {
	// text match style for filtering
	substring, startsWith, exact, // all case insensitive

	// advanced filter criteria operators
	equals, // exactly equal to
	notEqual, // not equal to
	iEquals, // exactly equal to, if case is disregarded
	iNotEqual, // not equal to, if case is disregarded
	greaterThan, // Greater than
	lessThan, // Less than
	greaterOrEqual, // Greater than or equal to
	lessOrEqual, // Less than or equal to
	contains, // Contains as sub-string (match case)
	endsWith, // Ends with (match case)
	iContains, // Contains as sub-string (case insensitive)
	iStartsWith, // Starts with (case insensitive)
	iEndsWith, // Ends with (case insensitive)
	notContains, // Does not contain as sub-string (match case)
	notStartsWith, // Does not start with (match case)
	notEndsWith, // Does not end with (match case)
	iNotContains, // Does not contain as sub-string (case insensitive)
	iNotStartsWith, // Does not start with (case insensitive)
	iNotEndsWith, // Does not end with (case insensitive)
	iBetween, // shortcut for "greaterThan" + "and" + "lessThan" (case insensitive)
	between, // shortcut for "greaterThan" + "lessThan" + "and". Specify criterion.start and criterion.end
	matchesPattern, // Basic GLOB matching using wildcards (see DataSource.translatePatternOperators for more information on available patterns)
	iMatchesPattern, // Basic GLOB matching using wildcards (case insensitive) (see DataSource.translatePatternOperators for more information on available patterns)
	containsPattern, // GLOB matching using wildcards. Value is considered to meet the criterion if it contains the pattern. See DataSource.translatePatternOperators for more information on available patterns)
	startsWithPattern, // GLOB mathcing using wildcards. Value is considered to meet the criterion if it starts with the pattern.See DataSource.translatePatternOperators for more information on available patterns)
	endsWithPattern, // GLOB mathcing using wildcards. Value is considered to meet the criterion if it starts with the pattern.See DataSource.translatePatternOperators for more information on available patterns)
	iContainsPattern, // GLOB matching using wildcards. Value is considered to meet the criterion if it contains the pattern. Matching is case insensitive. See DataSource.translatePatternOperators for more information on available patterns)
	iStartsWithPattern, // GLOB matching using wildcards. Value is considered to meet the criterion if it starts with the pattern. Matching is case insensitive.See DataSource.translatePatternOperators for more information on available patterns)
	iEndsWithPattern, // GLOB matching using wildcards.Value is considered to meet the criterion if it ends with the pattern. Matching is case insensitive. See DataSource.translatePatternOperators for more information on available patterns)
	regexp, // Regular expression match
	iregexp, // Regular expression match (case insensitive)
	isNull, // value is null
	notNull, // value is non-null. Note empty string ("") is non-null
	isBlank, // value is either null or the empty string. For numeric fields it behaves as isNull
	notBlank, // value is neither null nor the empty string ("")
	inSet, // value is in a set of values. Specify criterion.value as an Array
	notInSet, // value is not in a set of values. Specify criterion.value as an Array
	equalsField, // matches another field (specify fieldName as criterion.value)
	notEqualField, // does not match another field (specified fieldName as criterion.value)
	iEqualsField, // matches another field (case insensitive, specify fieldName as criterion.value)
	iNotEqualField, // does not match another field (case insensitive, specify fieldName as criterion.value)
	greaterThanField, // greater than another field (specify fieldName as criterion.value)
	lessThanField, // less than another field (specify fieldName as criterion.value)
	greaterOrEqualField, // greater than or equal to another field (specify fieldName as criterion.value)
	lessOrEqualField, // less than or equal to another field (specify fieldName as criterion.value)
	containsField, // Contains as sub-string (match case) another field value (specify fieldName as criterion.value)
	startsWithField, // Starts with (match case) another field value (specify fieldName as criterion.value)
	endsWithField, // Ends with (match case) another field value (specify fieldName as criterion.value)
	iContainsField, // Contains as sub-string (case insensitive) another field value (specify fieldName as criterion.value)
	iStartsWithField, // Starts with (case insensitive) another field value (specify fieldName as criterion.value)
	iEndsWithField, // Ends with (case insensitive) another field value (specify fieldName as criterion.value)
	notContainsField, // Does not contain as sub-string (match case) another field value (specify fieldName as criterion.value)
	notStartsWithField, // Does not start with (match case) another field value (specify fieldName as criterion.value)
	notEndsWithField, // Does not end with (match case) another field value (specify fieldName as criterion.value)
	iNotContainsField, // Does not contain as sub-string (case insensitive) another field value (specify fieldName as criterion.value)
	iNotStartsWithField, // Does not start with (case insensitive) another field value (specify fieldName as criterion.value)
	iNotEndsWithField, // Does not end with (case insensitive) another field value (specify fieldName as criterion.value)
	and, // all subcriteria (criterion.criteria) are true
	not, // all subcriteria (criterion.criteria) are false
	or, // at least one subcriteria (criterion.criteria) is true
	betweenInclusive, // shortcut for "greaterOrEqual" + "lessOrEqual" + "and". Specify criterion.start and criterion.end
	iBetweenInclusive, // shortcut for "greaterOrEqual" + "lessOrEqual" + "and". Specify criterion.start and criterion.end
	geoWithin, // the geometry field is within (contained by) the operand geometry
	geoContains, // the operand geometry is within (contained by) the geometry field
	geoOverlaps, // the geometry field overlaps the operand geometry
	geoDisjoint, // the geometry field does not touch or intersect the operand geometry
	geoIntersects, // the geometry field intersects the operand geometry
	geoTouches, // the geometry field touches (but does not intersect) the operand geometry
	geoCrosses, // not sure???
	geoEquals; // the geometry field is equivalent to the operand geometry
	
	/**
	 * Maps a framework {@link FilterOperator} to the closest SmartClient operator.
	 *
	 * @param operator The framework operator.
	 * @return The mapped SmartClient operator.
	 */
	public static @Nonnull SmartClientFilterOperator fromFilterOperator(@Nonnull FilterOperator operator) {
		return switch (operator) {
			case equal -> equals;
			case greater -> greaterThan;
			case greaterEqual -> greaterOrEqual;
			case isNull -> isNull;
			case less -> lessThan;
			case lessEqual -> lessOrEqual;
			case like -> iContains;
			case notEqual -> iNotEqual;
			case notLike -> iNotContains;
			case notNull -> notNull;
			case nullOrEqual -> equals;
			case nullOrGreater -> greaterThan;
			case nullOrGreaterEqual -> greaterOrEqual;
			case nullOrLess -> lessThan;
			case nullOrLessEqual -> lessOrEqual;
			case nullOrLike -> iContains;
			case nullOrNotEqual -> notEqual;
			case nullOrNotLike -> iNotContains;
		};
	}

	/**
	 * Indicates if this operator negates a condition on a value (notEqual, the not contains, starts with and ends with
	 * family, notInSet). SmartClient evaluates such a criterion as "not the condition", so it also matches a row with
	 * no value.
	 *
	 * @return true for a negative operator
	 */
	public boolean isNegative() {
		return switch (this) {
			case notEqual, iNotEqual, notContains, iNotContains, notStartsWith, iNotStartsWith, notEndsWith, iNotEndsWith,
					notInSet -> true;
			default -> false;
		};
	}

	/**
	 * Indicates if this operator tests for no value (isNull, notNull, isBlank, notBlank).
	 *
	 * @return true for a null or blank check
	 */
	public boolean isNullCheck() {
		return switch (this) {
			case isNull, notNull, isBlank, notBlank -> true;
			default -> false;
		};
	}

	/**
	 * Returns the operator that negates this one for a criterion under Match None (not): contains becomes notContains,
	 * greaterThan lessOrEqual, isNull notNull and so on. Ranges (between and the rest) are negated by their outside,
	 * not by one operator; patterns, geometries, field comparisons and the logical operators have no negation here.
	 *
	 * @return the negating operator, or null if there is none
	 */
	public @Nullable SmartClientFilterOperator negation() {
		return switch (this) {
			case substring, contains -> notContains;
			case iContains -> iNotContains;
			case notContains -> contains;
			case iNotContains -> iContains;
			case startsWith -> notStartsWith;
			case iStartsWith -> iNotStartsWith;
			case notStartsWith -> startsWith;
			case iNotStartsWith -> iStartsWith;
			case endsWith -> notEndsWith;
			case iEndsWith -> iNotEndsWith;
			case notEndsWith -> endsWith;
			case iNotEndsWith -> iEndsWith;
			case exact, equals -> notEqual;
			case iEquals -> iNotEqual;
			case notEqual -> equals;
			case iNotEqual -> iEquals;
			case greaterThan -> lessOrEqual;
			case greaterOrEqual -> lessThan;
			case lessThan -> greaterOrEqual;
			case lessOrEqual -> greaterThan;
			case isNull -> notNull;
			case notNull -> isNull;
			case isBlank -> notBlank;
			case notBlank -> isBlank;
			case inSet -> notInSet;
			case notInSet -> inSet;
			default -> null;
		};
	}
}

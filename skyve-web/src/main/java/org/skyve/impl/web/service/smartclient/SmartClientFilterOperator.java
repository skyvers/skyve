package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.FilterOperator;

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
	betweenInclusive, // shortcut for "greaterThan" + "lessThan" + "and". Specify criterion.start and criterion.end
	iBetweenInclusive, // shortcut for "greaterOrEqual" + "lessOrEqual" + "and". Specify criterion.start and criterion.end
	geoWithin, // the geometry field is within (contained by) the operand geometry
	geoContains, // the operand geometry is within (contained by) the geometry field
	geoOverlaps, // the geometry field overlaps the operand geometry
	geoDisjoint, // the geometry field does not touch or intersect the operand geometry
	geoIntersects, // the geometry field intersects the operand geometry
	geoTouches, // the geometry field touches (but does not intersect) the operand geometry
	geoCrosses, // not sure???
	geoEquals; // the geometry field is equivalent to the operand geometry
	
	public static SmartClientFilterOperator fromFilterOperator(FilterOperator operator) {
		SmartClientFilterOperator result = null;
		
		switch (operator) {
		case equal:
			result = equals;
			break;
		case greater:
			result = greaterThan;
			break;
		case greaterEqual:
			result = greaterOrEqual;
			break;
		case isNull:
			result = isNull;
			break;
		case less:
			result = lessThan;
			break;
		case lessEqual:
			result = lessOrEqual;
			break;
		case like:
			result = iContains;
			break;
		case notEqual:
			result = iNotEqual;
			break;
		case notLike:
			result = iNotContains;
			break;
		case notNull:
			result = notNull;
			break;
		case nullOrEqual:
			result = equals;
			break;
		case nullOrGreater:
			result = greaterThan;
			break;
		case nullOrGreaterEqual:
			result = greaterOrEqual;
			break;
		case nullOrLess:
			result = lessThan;
			break;
		case nullOrLessEqual:
			result = lessOrEqual;
			break;
		case nullOrLike:
			result = iContains;
			break;
		case nullOrNotEqual:
			result = notEqual;
			break;
		case nullOrNotLike:
			result = iNotContains;
			break;
		default:
		}
		
		return result;
	}
}

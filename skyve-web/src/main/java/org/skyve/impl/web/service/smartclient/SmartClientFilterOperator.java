package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.FilterOperator;

public enum SmartClientFilterOperator {
	// text match style for filtering
	substring, startsWith, exact, // both case insensitive
	
	// advanced filter criteria operators
	equals, // exactly equal to
	iEquals, // exactly equal to (ignore case)
	notEqual, // not equal to
	iNotEqual, // not equal to (ignore case)
	greaterThan, // Greater than
	lessThan, // Less than
	greaterOrEqual, // Greater than or equal to
	lessOrEqual, // Less than or equal to
	iContains, // Contains as sub-string (case insensitive)
	iStartsWith, // Starts with (case insensitive)
	iEndsWith, // Ends with (case insensitive)
	iNotContains, // Does not contain as sub-string (case insensitive)
	iNotStartsWith, // Does not start with (case insensitive)
	iNotEndsWith, // Does not end with (case insensitive)
	regexp, // Regular expression match
	iregexp, // Regular expression match (case insensitive)
	isNull, // value is null
	notNull, // value is non-null. Note empty string ("") is non-null
	inSet, // value is in a set of values. Specify criterion.value as an Array
	notInSet, // value is not in a set of values. Specify criterion.value as an Array
	equalsField, // matches another field (specify fieldName as criterion.value)
	notEqualField, // does not match another field (specified fieldName as criterion.value)
	greaterThanField, // greater than another field (specify fieldName as criterion.value)
	lessThanField, // less than another field (specify fieldName as criterion.value)
	greaterOrEqualField, // greater than or equal to another field (specify fieldName as criterion.value)
	lessOrEqualField, // less than or equal to another field (specify fieldName as criterion.value)
	and, // all subcriteria (criterion.criteria) are true
	not, // all subcriteria (criterion.criteria) are false
	or, // at least one subcriteria (criterion.criteria) is true
	betweenInclusive, // shortcut for "greaterThan" + "lessThan" + "and". Specify criterion.start and criterion.end
	iBetweenInclusive, // shortcut for "greaterOrEqual" + "lessOrEqual" + "and". Specify criterion.start and criterion.end
	gWithin, // the geometry field is within (contained by) the operand geometry
	gContains, // the operand geometry is within (contained by) the geometry field
	gOverlaps, // the geometry field overlaps the operand geometry
	gDisjoint, // the geometry field does not touch or intersect the operand geometry
	gIntersects, // the geometry field intersects the operand geometry
	gTouches, // the geometry field touches (but does not intersect) the operand geometry
	gCrosses, // not sure???
	gEquals; // the geometry field is equivalent to the operand geometry
	
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

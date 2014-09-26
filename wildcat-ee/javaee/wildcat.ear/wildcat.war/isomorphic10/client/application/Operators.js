/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */






//> @class Operators
// The Operators class provides a collection of class-level properties that can be used to
// modify the descriptions associated with the logical +link{operator}s used in 
// +link{AdvancedCriteria}. This is primarily intended for internationalization.
// <p>
// To add or override an operator's description, use
// Operators.+link{Class.addClassProperties,addClassProperties()}.  For example:
// <p>
// <code>Operators.addClassProperties({lessOrEqualTitle: "Less than or equal to"});</code>
// @treeLocation Client Reference/Data Binding/DataSource
// @visibility external
//<
isc.defineClass("Operators", "Class").addClassProperties({

    //> @classAttr Operators.equalsTitle (String : "equals" : IR)
    // Title for the "equals" operator
    // @group i18nMessages
    // @visibility external
    //<
    equalsTitle: "equals",

    //> @classAttr Operators.notEqualTitle (String : "not equal" : IR)
    // Title for the "notEqual" operator
    // @group i18nMessages
    // @visibility external
    //<
    notEqualTitle: "not equal",

    //> @classAttr Operators.iEqualsTitle (String : "equals (ignore case)" : IR)
    // Title for the "iEquals" operator
    // @group i18nMessages
    // @visibility external
    //<
    iEqualsTitle: "equals (ignore case)",

    //> @classAttr Operators.iNotEqualTitle (String : "not equal (ignore case)" : IR)
    // Title for the "iNotEqual" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotEqualTitle: "not equal (ignore case)",

    //> @classAttr Operators.greaterThanTitle (String : "greater than" : IR)
    // Title for the "greaterThan" operator
    // @group i18nMessages
    // @visibility external
    //<
    greaterThanTitle: "greater than",

    //> @classAttr Operators.lessThanTitle (String : "less than" : IR)
    // Title for the "lessThan" operator
    // @group i18nMessages
    // @visibility external
    //<
    lessThanTitle: "less than",

    //> @classAttr Operators.greaterOrEqualTitle (String : "greater than or equal to" : IR)
    // Title for the "greaterOrEqual" operator
    // @group i18nMessages
    // @visibility external
    //<
    greaterOrEqualTitle: "greater than or equal to",

    //> @classAttr Operators.lessOrEqualTitle (String : "less than or equal to" : IR)
    // Title for the "lessOrEqual" operator
    // @group i18nMessages
    // @visibility external
    //<
    lessOrEqualTitle: "less than or equal to",

    //> @classAttr Operators.betweenTitle (String : "between (match case)" : IR)
    // Title for the "between" operator
    // @group i18nMessages
    // @visibility external
    //<
    betweenTitle: "between (match case)",

    //> @classAttr Operators.iBetweenTitle (String : "between" : IR)
    // Title for the "iBetween" operator
    // @group i18nMessages
    // @visibility external
    //<
    iBetweenTitle: "between",

    //> @classAttr Operators.betweenInclusiveTitle (String : "between (inclusive, match case)" : IR)
    // Title for the "betweenInclusive" operator
    // @group i18nMessages
    // @visibility external
    //<
    betweenInclusiveTitle: "between (inclusive, match case)",

    //> @classAttr Operators.iBetweenInclusiveTitle (String : "between (inclusive)" : IR)
    // Title for the "iBetweenInclusive" operator
    // @group i18nMessages
    // @visibility external
    //<
    iBetweenInclusiveTitle: "between (inclusive)",

    //> @classAttr Operators.iContainsTitle (String : "contains" : IR)
    // Title for the "iContains" operator
    // @group i18nMessages
    // @visibility external
    //<
    iContainsTitle: "contains",

    //> @classAttr Operators.iStartsWithTitle (String : "starts with" : IR)
    // Title for the "iStartsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    iStartsWithTitle: "starts with",

    //> @classAttr Operators.iEndsWithTitle (String : "ends with" : IR)
    // Title for the "iEndsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    iEndsWithTitle: "ends with",

    //> @classAttr Operators.containsTitle (String : "contains (match case)" : IR)
    // Title for the "contains" operator
    // @group i18nMessages
    // @visibility external
    //<
    containsTitle: "contains (match case)",

    //> @classAttr Operators.startsWithTitle (String : "starts with (match case)" : IR)
    // Title for the "startsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    startsWithTitle: "starts with (match case)",

    //> @classAttr Operators.endsWithTitle (String : "ends with (match case)" : IR)
    // Title for the "endsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    endsWithTitle: "ends with (match case)",

    //> @classAttr Operators.iNotContainsTitle (String : "does not contain" : IR)
    // Title for the "iNotContains" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotContainsTitle: "does not contain",

    //> @classAttr Operators.iNotStartsWithTitle (String : "does not start with" : IR)
    // Title for the "iNotStartsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotStartsWithTitle: "does not start with",

    //> @classAttr Operators.iNotEndsWithTitle (String : "does not end with" : IR)
    // Title for the "iNotEndsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotEndsWithTitle: "does not end with",

    //> @classAttr Operators.notContainsTitle (String : "does not contain (match case)" : IR)
    // Title for the "notContains" operator
    // @group i18nMessages
    // @visibility external
    //<
    notContainsTitle: "does not contain (match case)",

    //> @classAttr Operators.notStartsWithTitle (String : "does not start with (match case)" : IR)
    // Title for the "notStartsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    notStartsWithTitle: "does not start with (match case)",

    //> @classAttr Operators.notEndsWithTitle (String : "does not end with (match case)" : IR)
    // Title for the "notEndsWith" operator
    // @group i18nMessages
    // @visibility external
    //<
    notEndsWithTitle: "does not end with (match case)",

    //> @classAttr Operators.isNullTitle (String : "is null" : IR)
    // Title for the "isNull" operator
    // @group i18nMessages
    // @visibility external
    //<
    isNullTitle: "is null",

    //> @classAttr Operators.notNullTitle (String : "is not null" : IR)
    // Title for the "notNull" operator
    // @group i18nMessages
    // @visibility external
    //<
    notNullTitle: "not null",

    //> @classAttr Operators.regexpTitle (String : "matches expression (exact case)" : IR)
    // Title for the "regexp" operator
    // @group i18nMessages
    // @visibility external
    //<
    regexpTitle: "matches expression (exact case)",

    //> @classAttr Operators.iregexpTitle (String : "matches expression" : IR)
    // Title for the "iregexp" operator
    // @group i18nMessages
    // @visibility external
    //<
    iregexpTitle: "matches expression",

    //> @classAttr Operators.matchesPatternTitle (String : "matches pattern (exact case)" : IR)
    // Title for the "matchesPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    matchesPatternTitle: "matches pattern (exact case)",

    //> @classAttr Operators.iMatchesPatternTitle (String : "matches pattern" : IR)
    // Title for the "iMatchesPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    iMatchesPatternTitle: "matches pattern",

    //> @classAttr Operators.containsPatternTitle (String : "contains pattern (exact case)" : IR)
    // Title for the "containsPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    containsPatternTitle: "contains pattern (exact case)",

    //> @classAttr Operators.iContainsPatternTitle (String : "contains pattern" : IR)
    // Title for the "iContainsPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    iContainsPatternTitle: "contains pattern",

    //> @classAttr Operators.inSetTitle (String : "is one of" : IR)
    // Title for the "inSet" operator
    // @group i18nMessages
    // @visibility external
    //<
    inSetTitle: "is one of",

    //> @classAttr Operators.notInSetTitle (String : "is not one of" : IR)
    // Title for the "notInSet" operator
    // @group i18nMessages
    // @visibility external
    //<
    notInSetTitle: "is not one of",

    //> @classAttr Operators.equalsFieldTitle (String : "matches other field (match case)" : IR)
    // Title for the "equalsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    equalsFieldTitle: "matches other field (match case)",

    //> @classAttr Operators.notEqualFieldTitle (String : "differs from field (match case)" : IR)
    // Title for the "notEqualField" operator
    // @group i18nMessages
    // @visibility external
    //<
    notEqualFieldTitle: "differs from field (match case)",

    //> @classAttr Operators.iEqualsFieldTitle (String : "matches other field (case insensitive)" : IR)
    // Title for the "iEqualsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iEqualsFieldTitle: "matches other field (case insensitive)",

    //> @classAttr Operators.iNotEqualFieldTitle (String : "differs from field (case insensitive)" : IR)
    // Title for the "iNotEqualField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotEqualFieldTitle: "differs from field (case insensitive)",

    //> @classAttr Operators.greaterThanFieldTitle (String : "greater than field" : IR)
    // Title for the "greaterThanField" operator
    // @group i18nMessages
    // @visibility external
    //<
    greaterThanFieldTitle: "greater than field",

    //> @classAttr Operators.lessThanFieldTitle (String : "less than field" : IR)
    // Title for the "lessThanField" operator
    // @group i18nMessages
    // @visibility external
    //<
    lessThanFieldTitle: "less than field",

    //> @classAttr Operators.greaterOrEqualFieldTitle (String : "greater than or equal to field" : IR)
    // Title for the "greaterOrEqualField" operator
    // @group i18nMessages
    // @visibility external
    //<
    greaterOrEqualFieldTitle: "greater than or equal to field",

    //> @classAttr Operators.lessOrEqualFieldTitle (String : "less than or equal to field" : IR)
    // Title for the "lessOrEqualField" operator
    // @group i18nMessages
    // @visibility external
    //<
    lessOrEqualFieldTitle: "less than or equal to field",

    //> @classAttr Operators.containsFieldTitle (String : "contains (match case) another field value" : IR)
    // Title for the "containsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    containsFieldTitle: "contains (match case) another field value",

    //> @classAttr Operators.startsWithFieldTitle (String : "starts with (match case) another field value" : IR)
    // Title for the "startsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    startsWithFieldTitle: "starts with (match case) another field value",

    //> @classAttr Operators.endsWithFieldTitle (String : "ends with (match case) another field value" : IR)
    // Title for the "endsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    endsWithFieldTitle: "ends with (match case) another field value",

    //> @classAttr Operators.iContainsFieldTitle (String : "contains (case insensitive) another field value" : IR)
    // Title for the "iContainsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iContainsFieldTitle: "contains (case insensitive) another field value",

    //> @classAttr Operators.iStartsWithFieldTitle (String : "starts with (case insensitive) another field value" : IR)
    // Title for the "iStartsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iStartsWithFieldTitle: "starts with (case insensitive) another field value",

    //> @classAttr Operators.iEndsWithFieldTitle (String : "ends with (case insensitive) another field value" : IR)
    // Title for the "iEndsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iEndsWithFieldTitle: "ends with (case insensitive) another field value",

    //> @classAttr Operators.notContainsFieldTitle (String : "does not contain (match case) another field value" : IR)
    // Title for the "notContainsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    notContainsFieldTitle: "does not contain (match case) another field value",

    //> @classAttr Operators.notStartsWithFieldTitle (String : "does not start with (match case) another field value" : IR)
    // Title for the "notStartsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    notStartsWithFieldTitle: "does not start with (match case) another field value",

    //> @classAttr Operators.notEndsWithFieldTitle (String : "does not end with (match case) another field value" : IR)
    // Title for the "notEndsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    notEndsWithFieldTitle: "does not end with (match case) another field value",

    //> @classAttr Operators.iNotContainsFieldTitle (String : "does not contain (case insensitive) another field value" : IR)
    // Title for the "iNotContainsField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotContainsFieldTitle: "does not contain (case insensitive) another field value",

    //> @classAttr Operators.iNotStartsWithFieldTitle (String : "does not start with (case insensitive) another field value" : IR)
    // Title for the "iNotStartsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotStartsWithFieldTitle: "does not start with (case insensitive) another field value",

    //> @classAttr Operators.iNotEndsWithFieldTitle (String : "does not end with (case insensitive) another field value" : IR)
    // Title for the "iNotEndsWithField" operator
    // @group i18nMessages
    // @visibility external
    //<
    iNotEndsWithFieldTitle: "does not end with (case insensitive) another field value",

    //> @classAttr Operators.andTitle (String : "Match All" : IR)
    // Title for the "and" operator
    // @group i18nMessages
    // @visibility external
    //<
    andTitle: "and",

    //> @classAttr Operators.notTitle (String : "Match None" : IR)
    // Title for the "not" operator
    // @group i18nMessages
    // @visibility external
    //<
    notTitle: "not",

    //> @classAttr Operators.orTitle (String : "Match Any" : IR)
    // Title for the "or" operator
    // @group i18nMessages
    // @visibility external
    //<
    orTitle: "or",

    //> @classAttr Operators.startsWithPatternTitle (String : "starts with pattern (exact case)" : IR)
    // Title for the "startsWithPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    startsWithPatternTitle: "starts with pattern (exact case)",

    //> @classAttr Operators.iStartsWithPatternTitle (String : "starts with pattern" : IR)
    // Title for the "iStartsWithPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    iStartsWithPatternTitle: "starts with pattern",

    //> @classAttr Operators.endsWithPatternTitle (String : "ends with pattern (exact case)" : IR)
    // Title for the "endsWithPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    startsWithPatternTitle: "ends with pattern (exact case)",

    //> @classAttr Operators.iEndsWithPatternTitle (String : "ends with pattern" : IR)
    // Title for the "iEndsWithPattern" operator
    // @group i18nMessages
    // @visibility external
    //<
    iStartsWithPatternTitle: "starts with pattern"
    
});

package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.Filter;
import org.locationtech.jts.geom.Geometry;
import org.skyve.metadata.view.model.list.ListModel;

import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class SmartClientListServletHelperTest {

	@Test
	void transformWildcardFilterOperatorMapsContainsStylesToEquals() throws Exception {
		SmartClientFilterOperator mapped = invokeTransformWildcardFilterOperator(SmartClientFilterOperator.iContains);
		assertEquals(SmartClientFilterOperator.equals, mapped);
	}

	@Test
	void transformWildcardFilterOperatorMapsNotContainsStylesToNotEqual() throws Exception {
		SmartClientFilterOperator mapped = invokeTransformWildcardFilterOperator(SmartClientFilterOperator.iNotContains);
		assertEquals(SmartClientFilterOperator.notEqual, mapped);
	}

	@Test
	void transformWildcardFilterOperatorLeavesUnrelatedOperatorsUnchanged() throws Exception {
		SmartClientFilterOperator mapped = invokeTransformWildcardFilterOperator(SmartClientFilterOperator.greaterThan);
		assertEquals(SmartClientFilterOperator.greaterThan, mapped);
	}

	@Test
	void transformWildcardFilterOperatorMapsAdditionalPositiveWildcardOperatorsToEquals() throws Exception {
		assertEquals(SmartClientFilterOperator.equals,
				invokeTransformWildcardFilterOperator(SmartClientFilterOperator.startsWith));
		assertEquals(SmartClientFilterOperator.equals,
				invokeTransformWildcardFilterOperator(SmartClientFilterOperator.substring));
		assertEquals(SmartClientFilterOperator.equals,
				invokeTransformWildcardFilterOperator(SmartClientFilterOperator.regexp));
	}

	@Test
	void transformWildcardFilterOperatorMapsAdditionalNegativeWildcardOperatorsToNotEqual() throws Exception {
		assertEquals(SmartClientFilterOperator.notEqual,
				invokeTransformWildcardFilterOperator(SmartClientFilterOperator.iNotEndsWith));
		assertEquals(SmartClientFilterOperator.notEqual,
				invokeTransformWildcardFilterOperator(SmartClientFilterOperator.iNotStartsWith));
	}

	@Test
	void addCriterionToFilterHandlesTaggedTrueAndFalse() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter(PersistentBean.TAGGED_NAME, SmartClientFilterOperator.equals, "true", null, null, "tag-1", filter);
		invokeAddCriterionToFilter(PersistentBean.TAGGED_NAME, SmartClientFilterOperator.equals, "false", null, null, "tag-2", filter);

		verify(filter).addTagged("tag-1", true);
		verify(filter).addTagged("tag-2", false);
	}

	@Test
	void addCriterionToFilterHandlesParentIdNullAndRootAndChildBranches() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, null, null, null, null, null, filter);
		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, null, "_root123", null, null, null, filter);
		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, null, "child123", null, null, null, filter);

		verify(filter).addNull(HierarchicalBean.PARENT_ID);
		verify(filter).addEquals(Bean.DOCUMENT_ID, "root123");
		verify(filter).addEquals(HierarchicalBean.PARENT_ID, "child123");
	}

	@Test
	void addCriterionToFilterAddsEqualsForNonParentWhenFilterOperatorIsNull() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", null, "alice", null, null, null, filter);

		verify(filter).addEquals("customer.name", "alice");
	}

	@Test
	void addCriterionToFilterAppliesContainsFamilyOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.contains, "ali", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.notContains, "ali", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.startsWith, "al", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.notStartsWith, "al", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.endsWith, "ce", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.notEndsWith, "ce", null, null, null, filter);

		verify(filter).addContains("customer.name", "ali");
		verify(filter).addNotContains("customer.name", "ali");
		verify(filter).addStartsWith("customer.name", "al");
		verify(filter).addNotStartsWith("customer.name", "al");
		verify(filter).addEndsWith("customer.name", "ce");
		verify(filter).addNotEndsWith("customer.name", "ce");
	}

	@Test
	void addCriterionToFilterAppliesContainsAliasOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.substring, "ali", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iContains, "ali", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iNotContains, "ali", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iStartsWith, "al", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iNotStartsWith, "al", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iEndsWith, "ce", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iNotEndsWith, "ce", null, null, null, filter);

		verify(filter, org.mockito.Mockito.times(2)).addContains("customer.name", "ali");
		verify(filter).addNotContains("customer.name", "ali");
		verify(filter).addStartsWith("customer.name", "al");
		verify(filter).addNotStartsWith("customer.name", "al");
		verify(filter).addEndsWith("customer.name", "ce");
		verify(filter).addNotEndsWith("customer.name", "ce");
	}

	@Test
	void addCriterionToFilterAppliesCaseInsensitiveEqualityOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iEquals, "Alice", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iNotEqual, "Alice", null, null, null, filter);

		verify(filter).addEqualsIgnoreCase("customer.name", "Alice");
		verify(filter).addNotEqualsIgnoreCase("customer.name", "Alice");
	}

	@Test
	void addCriterionToFilterHandlesNullAndBlankOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.isNull, "ignored", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.isBlank, "ignored", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.notNull, "ignored", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.notBlank, "ignored", null, null, null, filter);

		verify(filter, org.mockito.Mockito.times(2)).addNull("customer.name");
		verify(filter, org.mockito.Mockito.times(2)).addNotNull("customer.name");
	}

	@Test
	void addCriterionToFilterHandlesInSetAndNotInSetForArrayAndList() throws Exception {
		Filter filter = mock(Filter.class);

		Object[] inValues = new Object[] {"A", "B"};
		Object[] notInValues = new Object[] {"X", "Y"};

		invokeAddCriterionToFilter("status", SmartClientFilterOperator.inSet, inValues, null, null, null, filter);
		invokeAddCriterionToFilter("status", SmartClientFilterOperator.notInSet, notInValues, null, null, null, filter);
		invokeAddCriterionToFilter("status", SmartClientFilterOperator.inSet, java.util.List.of("L1", "L2"), null, null, null, filter);
		invokeAddCriterionToFilter("status", SmartClientFilterOperator.notInSet, java.util.List.of("M1", "M2"), null, null, null, filter);

		verify(filter).addIn("status", inValues);
		verify(filter).addNotIn("status", notInValues);
		verify(filter).addIn("status", "L1", "L2");
		verify(filter).addNotIn("status", "M1", "M2");
	}

	@Test
	void addCriterionToFilterHandlesGeoOperators() throws Exception {
		Filter filter = mock(Filter.class);
		Geometry geometry = mock(Geometry.class);

		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoWithin, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoContains, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoCrosses, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoDisjoint, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoEquals, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoIntersects, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoOverlaps, geometry, null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoTouches, geometry, null, null, null, filter);

		verify(filter).addWithin("shape", geometry);
		verify(filter).addContains("shape", geometry);
		verify(filter).addCrosses("shape", geometry);
		verify(filter).addDisjoint("shape", geometry);
		verify(filter).addEquals("shape", geometry);
		verify(filter).addIntersects("shape", geometry);
		verify(filter).addOverlaps("shape", geometry);
		verify(filter).addTouches("shape", geometry);
	}

	@Test
	void addCriterionToFilterSkipsStringOperatorsWhenValueIsNull() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.contains, null, null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iEquals, null, null, null, null, filter);

		verify(filter, never()).addContains(anyString(), anyString());
		verify(filter, never()).addEqualsIgnoreCase(anyString(), anyString());
	}

	@Test
	void addCriterionToFilterHandlesRelationalOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.greaterThan, "10", null, null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.greaterOrEqual, "10", null, null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.lessThan, "20", null, null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.lessOrEqual, "20", null, null, null, filter);

		verify(filter).addGreaterThan("amount", "10");
		verify(filter).addGreaterThanOrEqualTo("amount", "10");
		verify(filter).addLessThan("amount", "20");
		verify(filter).addLessThanOrEqualTo("amount", "20");
	}

	@Test
	void addCriterionToFilterHandlesExactAndNotEqualAliases() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.exact, "10", null, null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.notEqual, "20", null, null, null, filter);

		verify(filter).addEquals("amount", "10");
		verify(filter).addNotEquals("amount", "20");
	}

	@Test
	void addCriterionToFilterHandlesBetweenOperatorVariants() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.betweenInclusive, null, "10", "20", null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.betweenInclusive, null, "30", null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.betweenInclusive, null, null, "40", null, filter);

		verify(filter).addBetween("amount", "10", "20");
		verify(filter).addGreaterThanOrEqualTo("amount", "30");
		verify(filter).addLessThanOrEqualTo("amount", "40");
	}

	@Test
	void addCriterionToFilterHandlesIBetweenVariants() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.iBetweenInclusive, null, "10", "20", null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.iBetween, null, "30", null, null, filter);
		invokeAddCriterionToFilter("amount", SmartClientFilterOperator.iBetween, null, null, "40", null, filter);

		verify(filter).addBetween("amount", "10", "20");
		verify(filter).addGreaterThanOrEqualTo("amount", "30");
		verify(filter).addLessThanOrEqualTo("amount", "40");
	}

	@Test
	void addCriterionToFilterHandlesParentIdWhenOperatorPresent() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, SmartClientFilterOperator.equals, null, null, null, null, filter);
		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, SmartClientFilterOperator.equals, "_root999", null, null, null, filter);
		invokeAddCriterionToFilter(HierarchicalBean.PARENT_ID, SmartClientFilterOperator.equals, "child999", null, null, null, filter);

		verify(filter).addNull(HierarchicalBean.PARENT_ID);
		verify(filter).addEquals(Bean.DOCUMENT_ID, "root999");
		verify(filter).addEquals(HierarchicalBean.PARENT_ID, "child999");
	}

	@Test
	void addCriterionToFilterNoOpsForRegexpAndLogicalOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.regexp, "[a-z]+", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iregexp, "[a-z]+", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.and, "x", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.or, "x", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.not, "x", null, null, null, filter);

		verifyNoInteractions(filter);
	}

	@Test
	void addCriterionToFilterNoOpsForFieldOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.equalsField, "fieldA", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.containsField, "fieldB", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.greaterThanField, "fieldC", null, null, null, filter);

		verifyNoInteractions(filter);
	}

	@Test
	void addCriterionToFilterNoOpsForPatternOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.matchesPattern, "ab.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.containsPattern, "bc.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.startsWithPattern, "cd.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.endsWithPattern, "de.*", null, null, null, filter);

		verifyNoInteractions(filter);
	}

	@Test
	void addCriterionToFilterNoOpsForCaseInsensitivePatternOperators() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iMatchesPattern, "ab.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iContainsPattern, "bc.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iStartsWithPattern, "cd.*", null, null, null, filter);
		invokeAddCriterionToFilter("customer.name", SmartClientFilterOperator.iEndsWithPattern, "de.*", null, null, null, filter);

		verifyNoInteractions(filter);
	}

	@Test
	void addCriterionToFilterConvertsEnumerationToCodeForStringOperators() throws Exception {
		Filter filter = mock(Filter.class);
		org.skyve.domain.types.Enumeration enumeration = mock(org.skyve.domain.types.Enumeration.class);
		org.mockito.Mockito.when(enumeration.toCode()).thenReturn("CODE_A");

		invokeAddCriterionToFilter("status", SmartClientFilterOperator.contains, enumeration, null, null, null, filter);
		invokeAddCriterionToFilter("status", SmartClientFilterOperator.iEquals, enumeration, null, null, null, filter);

		verify(filter).addContains("status", "CODE_A");
		verify(filter).addEqualsIgnoreCase("status", "CODE_A");
	}

	@Test
	void addCriterionToFilterNoOpsForUnsupportedInSetOrGeoValues() throws Exception {
		Filter filter = mock(Filter.class);

		invokeAddCriterionToFilter("status", SmartClientFilterOperator.inSet, "not-a-collection", null, null, null, filter);
		invokeAddCriterionToFilter("shape", SmartClientFilterOperator.geoWithin, "not-geometry", null, null, null, filter);

		verifyNoInteractions(filter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaToQueryDoesNotAddEmptyGeneratedFilter() throws Exception {
		ListModel<?> model = mock(ListModel.class);
		Filter outerFilter = mock(Filter.class);
		Filter generatedFilter = mock(Filter.class);

		org.mockito.Mockito.when(model.getFilter()).thenReturn(outerFilter);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(generatedFilter);
		org.mockito.Mockito.doReturn(true).when(generatedFilter).isEmpty();

		invokeAddAdvancedFilterCriteriaToQuery(null,
				null,
				null,
				compoundFilterOperator("and"),
				null,
				null,
				model);

		verify(outerFilter, never()).addAnd(generatedFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaToQueryAddsGeneratedFilterForParameterCriteria() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter outerFilter = mock(Filter.class);
		Filter generatedFilter = mock(Filter.class);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(model.getFilter()).thenReturn(outerFilter);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(generatedFilter);
		org.mockito.Mockito.doReturn(false).when(generatedFilter).isEmpty();

		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", ":param");
		criterion.put("operator", SmartClientFilterOperator.equals.toString());
		criterion.put("value", "abc");

		invokeAddAdvancedFilterCriteriaToQuery(module,
				document,
				user,
				compoundFilterOperator("and"),
				java.util.List.of(criterion),
				null,
				model);

		verify(model).putParameter("param", "abc");
		verify(outerFilter).addAnd(generatedFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalAddsOrForAdvancedSubFilter() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Filter subFilter = mock(Filter.class);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(subFilter);
		org.mockito.Mockito.doReturn(false).when(subFilter).isEmpty();

		java.util.Map<String, Object> subCriterion = new java.util.HashMap<>();
		subCriterion.put("fieldName", ":p");
		subCriterion.put("operator", SmartClientFilterOperator.equals.toString());
		subCriterion.put("value", "abc");

		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", null);
		criterion.put("operator", SmartClientFilterOperator.or.toString());
		criterion.put("criteria", java.util.List.of(subCriterion));

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("or"),
				java.util.List.of(criterion),
				null,
				model,
				filter);

		verify(model).putParameter("p", "abc");
		verify(filter).addOr(subFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalAddsAndForAdvancedSubFilter() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Filter subFilter = mock(Filter.class);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(subFilter);
		org.mockito.Mockito.doReturn(false).when(subFilter).isEmpty();

		java.util.Map<String, Object> subCriterion = new java.util.HashMap<>();
		subCriterion.put("fieldName", ":q");
		subCriterion.put("operator", SmartClientFilterOperator.equals.toString());
		subCriterion.put("value", "xyz");

		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", null);
		criterion.put("operator", SmartClientFilterOperator.and.toString());
		criterion.put("criteria", java.util.List.of(subCriterion));

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("and"),
				java.util.List.of(criterion),
				null,
				model,
				filter);

		verify(model).putParameter("q", "xyz");
		verify(filter).addAnd(subFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalSkipsEmptyAdvancedSubFilter() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Filter subFilter = mock(Filter.class);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(subFilter);
		org.mockito.Mockito.doReturn(true).when(subFilter).isEmpty();

		java.util.Map<String, Object> subCriterion = new java.util.HashMap<>();
		subCriterion.put("fieldName", ":r");
		subCriterion.put("operator", SmartClientFilterOperator.equals.toString());
		subCriterion.put("value", "noop");

		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", null);
		criterion.put("operator", SmartClientFilterOperator.or.toString());
		criterion.put("criteria", java.util.List.of(subCriterion));

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("or"),
				java.util.List.of(criterion),
				null,
				model,
				filter);

		verify(model).putParameter("r", "noop");
		verify(filter, never()).addOr(subFilter);
		verify(filter, never()).addAnd(subFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalAddsAndWhenCompoundIsNot() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Filter subFilter = mock(Filter.class);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(subFilter);
		org.mockito.Mockito.doReturn(false).when(subFilter).isEmpty();

		java.util.Map<String, Object> subCriterion = new java.util.HashMap<>();
		subCriterion.put("fieldName", ":s");
		subCriterion.put("operator", SmartClientFilterOperator.equals.toString());
		subCriterion.put("value", "hit");

		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", null);
		criterion.put("operator", SmartClientFilterOperator.and.toString());
		criterion.put("criteria", java.util.List.of(subCriterion));

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(criterion),
				null,
				model,
				filter);

		verify(model).putParameter("s", "hit");
		verify(filter).addAnd(subFilter);
	}

	@Test
	void fromStringReturnsNullWhenInputIsNull() throws Exception {
		Object result = invokeFromString("name", "value", null, null, null, String.class);
		assertEquals(null, result);
	}

	@Test
	void fromStringEscapesSingleQuotesInStringResult() throws Exception {
		Object result = invokeFromString("name", "value", "O'Brien", null, null, String.class);
		assertEquals("O%Brien", result);
	}

	@Test
	void fromStringDateWithoutTimeSeparatorRaisesValidationWhenUnparseable() {
		Customer customer = mock(Customer.class);
		Assertions.assertThrows(ValidationException.class,
				() -> invokeFromString("created", "value", "2026-05-27", customer, null, java.util.Date.class));
	}

	@Test
	void fromStringThrowsValidationExceptionWithBindingWhenConversionFails() {
		Customer customer = mock(Customer.class);
		ValidationException ex = Assertions.assertThrows(ValidationException.class,
				() -> invokeFromString("customer.age", "value", "not-a-number", customer, null, Integer.class));

		assertTrue(ex.getMessages().stream().anyMatch(m -> {
			for (String binding : m.getBindings()) {
				if ("customer_age".equals(binding)) {
					return true;
				}
			}
			return false;
		}));
	}

	@Test
	void fromStringThrowsValidationExceptionWithoutBindingWhenBindingIsNull() {
		Customer customer = mock(Customer.class);
		ValidationException ex = Assertions.assertThrows(ValidationException.class,
				() -> invokeFromString(null, "start", "not-a-number", customer, null, Integer.class));

		assertTrue(ex.getMessages().stream().anyMatch(m -> ! m.getBindings().iterator().hasNext()));
	}

	@Test
	void isRowTaggedReturnsTrueWhenOldValuesContainTaggedTrue() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		org.mockito.Mockito.when(request.getParameter("_oldValues")).thenReturn("{\"" + PersistentBean.TAGGED_NAME + "\":true}");

		assertTrue(invokeIsRowTagged(request));
	}

	@Test
	void isRowTaggedReturnsFalseWhenOldValuesMissingOrNotTagged() throws Exception {
		HttpServletRequest requestWithoutOldValues = mock(HttpServletRequest.class);
		org.mockito.Mockito.when(requestWithoutOldValues.getParameter("_oldValues")).thenReturn(null);
		assertFalse(invokeIsRowTagged(requestWithoutOldValues));

		HttpServletRequest requestWithFalseTag = mock(HttpServletRequest.class);
		org.mockito.Mockito.when(requestWithFalseTag.getParameter("_oldValues")).thenReturn("{\"" + PersistentBean.TAGGED_NAME + "\":false}");
		assertFalse(invokeIsRowTagged(requestWithFalseTag));
	}

	private static SmartClientFilterOperator invokeTransformWildcardFilterOperator(SmartClientFilterOperator operator)
	throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("transformWildcardFilterOperator", SmartClientFilterOperator.class);
		method.setAccessible(true);
		return (SmartClientFilterOperator) method.invoke(null, operator);
	}

	private static void invokeAddCriterionToFilter(String binding,
									SmartClientFilterOperator filterOperator,
									Object value,
									Object start,
									Object end,
									String tagId,
									Filter filter) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("addCriterionToFilter",
				String.class,
				SmartClientFilterOperator.class,
				Object.class,
				Object.class,
				Object.class,
				String.class,
				Filter.class);
		method.setAccessible(true);
		invokeVoid(method, binding, filterOperator, value, start, end, tagId, filter);
	}

	private static Object invokeFromString(String valueBinding,
									String valueDescription,
									String valueString,
									Customer customer,
									org.skyve.domain.types.converters.Converter<?> converter,
									Class<?> type) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("fromString",
				String.class,
				String.class,
				String.class,
				Customer.class,
				org.skyve.domain.types.converters.Converter.class,
				Class.class);
		method.setAccessible(true);
		try {
			return method.invoke(null, valueBinding, valueDescription, valueString, customer, converter, type);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static boolean invokeIsRowTagged(HttpServletRequest request) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("isRowTagged", HttpServletRequest.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, request)).booleanValue();
	}

	private static void invokeAddAdvancedFilterCriteriaToQuery(Module module,
										Document document,
										User user,
										Object compoundFilterOperator,
										java.util.List<java.util.Map<String, Object>> criteria,
										String tagId,
										ListModel<?> model) throws Exception {
		Class<?> compoundFilterOperatorClass = Class.forName("org.skyve.impl.snapshot.CompoundFilterOperator");
		Method method = SmartClientListServlet.class.getMethod("addAdvancedFilterCriteriaToQuery",
				Module.class,
				Document.class,
				User.class,
				compoundFilterOperatorClass,
				java.util.List.class,
				String.class,
				ListModel.class);
		invokeVoid(method, module, document, user, compoundFilterOperator, criteria, tagId, model);
	}

	private static void invokeAddAdvancedFilterCriteriaToQueryInternal(Module module,
											Document document,
											User user,
											Object compoundFilterOperator,
											java.util.List<java.util.Map<String, Object>> criteria,
											String tagId,
											ListModel<?> model,
											Filter filter) throws Exception {
		Class<?> compoundFilterOperatorClass = Class.forName("org.skyve.impl.snapshot.CompoundFilterOperator");
		Method method = SmartClientListServlet.class.getDeclaredMethod("addAdvancedFilterCriteriaToQueryInternal",
				Module.class,
				Document.class,
				User.class,
				compoundFilterOperatorClass,
				java.util.List.class,
				String.class,
				ListModel.class,
				Filter.class);
		method.setAccessible(true);
		invokeVoid(method, module, document, user, compoundFilterOperator, criteria, tagId, model, filter);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private static Object compoundFilterOperator(String name) throws Exception {
		Class<?> compoundFilterOperatorClass = Class.forName("org.skyve.impl.snapshot.CompoundFilterOperator");
		return Enum.valueOf((Class<? extends Enum>) compoundFilterOperatorClass.asSubclass(Enum.class), name);
	}

	private static void invokeVoid(Method method, Object... args) throws Exception {
		try {
			method.invoke(null, args);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}
}

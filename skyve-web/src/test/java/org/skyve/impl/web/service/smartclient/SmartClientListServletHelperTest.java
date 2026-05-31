package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.Filter;
import org.locationtech.jts.geom.Geometry;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.impl.metadata.model.document.field.Text;

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
	@SuppressWarnings("boxing")
	void addSimpleFilterCriteriaToQueryUsesStringMetadataAndContainsOperator() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Customer customer = mock(Customer.class);
		ListModel<Bean> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(model.getFilter()).thenReturn(filter);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> criteria = new java.util.HashMap<>();
		criteria.put("name", "O'Brien");

		SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
				document,
				customer,
				SmartClientFilterOperator.contains,
				criteria,
				null,
				model);

		verify(filter).addContains("name", "O%Brien");
	}

	@Test
	@SuppressWarnings("boxing")
	void addSimpleFilterCriteriaToQueryForcesEqualsForConstantDomainFields() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Customer customer = mock(Customer.class);
		ListModel<Bean> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text statusField = new Text();
		statusField.setName("status");
		statusField.setLength(20);
		statusField.setDomainType(DomainType.constant);

		org.mockito.Mockito.when(model.getFilter()).thenReturn(filter);
		org.mockito.Mockito.when(document.getAttribute("status")).thenReturn(statusField);

		java.util.Map<String, Object> criteria = new java.util.HashMap<>();
		criteria.put("status", "A'B");

		SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
				document,
				customer,
				SmartClientFilterOperator.contains,
				criteria,
				null,
				model);

		verify(filter).addEquals("status", "A%B");
	}

	@Test
	@SuppressWarnings("boxing")
	void addSimpleFilterCriteriaToQueryForcesEqualsForNumericFields() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Customer customer = mock(Customer.class);
		ListModel<Bean> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		org.skyve.impl.metadata.model.document.field.Integer ageField = new org.skyve.impl.metadata.model.document.field.Integer();
		ageField.setName("age");

		org.mockito.Mockito.when(model.getFilter()).thenReturn(filter);
		org.mockito.Mockito.when(document.getAttribute("age")).thenReturn(ageField);

		java.util.Map<String, Object> criteria = new java.util.HashMap<>();
		criteria.put("age", "42");

		SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
				document,
				customer,
				SmartClientFilterOperator.contains,
				criteria,
				null,
				model);

		verify(filter).addEquals("age", java.lang.Integer.valueOf(42));
	}

	@Test
	void addSimpleFilterCriteriaToQueryStoresParameterizedListsAndArrays() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Customer customer = mock(Customer.class);
		ListModel<Bean> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);

		org.mockito.Mockito.when(model.getFilter()).thenReturn(filter);

		java.util.Map<String, Object> criteria = new java.util.HashMap<>();
		criteria.put(":codes", new java.util.ArrayList<>(java.util.List.of("A", "B")));
		criteria.put(":flags", new Object[] {"X", "Y"});

		SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
				document,
				customer,
				SmartClientFilterOperator.equals,
				criteria,
				null,
				model);

		verify(model).putParameter("codes", java.util.List.of("A", "B"));
		verify(model).putParameter(org.mockito.ArgumentMatchers.eq("flags"), org.mockito.ArgumentMatchers.argThat(value -> {
			if (! (value instanceof Object[] values)) {
				return false;
			}
			return (values.length == 2) && "X".equals(values[0]) && "Y".equals(values[1]);
		}));
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalUsesOrBranchForSecondSimpleCriterion() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Filter orFilter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(50);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);
		org.mockito.Mockito.when(model.newFilter()).thenReturn(orFilter);
		org.mockito.Mockito.doReturn(false).when(orFilter).isEmpty();

		java.util.Map<String, Object> c1 = new java.util.HashMap<>();
		c1.put("fieldName", "name");
		c1.put("operator", SmartClientFilterOperator.equals.toString());
		c1.put("value", "first");

		java.util.Map<String, Object> c2 = new java.util.HashMap<>();
		c2.put("fieldName", "name");
		c2.put("operator", SmartClientFilterOperator.equals.toString());
		c2.put("value", "second");

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("or"),
				java.util.List.of(c1, c2),
				null,
				model,
				filter);

		verify(filter).addEquals("name", "first");
		verify(orFilter).addEquals("name", "second");
		verify(filter).addOr(orFilter);
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalNotInvertsStringAndSetOperators() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> contains = new java.util.HashMap<>();
		contains.put("fieldName", "name");
		contains.put("operator", SmartClientFilterOperator.contains.toString());
		contains.put("value", "abc");

		java.util.Map<String, Object> notContains = new java.util.HashMap<>();
		notContains.put("fieldName", "name");
		notContains.put("operator", SmartClientFilterOperator.notContains.toString());
		notContains.put("value", "def");

		java.util.Map<String, Object> inSet = new java.util.HashMap<>();
		inSet.put("fieldName", "name");
		inSet.put("operator", SmartClientFilterOperator.inSet.toString());
		inSet.put("value", new java.util.ArrayList<>(java.util.List.of("A", "B")));

		java.util.Map<String, Object> notInSet = new java.util.HashMap<>();
		notInSet.put("fieldName", "name");
		notInSet.put("operator", SmartClientFilterOperator.notInSet.toString());
		notInSet.put("value", new java.util.ArrayList<>(java.util.List.of("X", "Y")));

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(contains, notContains, inSet, notInSet),
				null,
				model,
				filter);

		verify(filter).addNotContains("name", "abc");
		verify(filter).addContains("name", "def");
		verify(filter).addNotIn("name", "A", "B");
		verify(filter).addIn("name", "X", "Y");
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalNotInvertsRangeAndNullOperators() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> between = new java.util.HashMap<>();
		between.put("fieldName", "name");
		between.put("operator", SmartClientFilterOperator.betweenInclusive.toString());
		between.put("start", "A");
		between.put("end", "Z");

		java.util.Map<String, Object> isNull = new java.util.HashMap<>();
		isNull.put("fieldName", "name");
		isNull.put("operator", SmartClientFilterOperator.isNull.toString());

		java.util.Map<String, Object> notNull = new java.util.HashMap<>();
		notNull.put("fieldName", "name");
		notNull.put("operator", SmartClientFilterOperator.notNull.toString());

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(between, isNull, notNull),
				null,
				model,
				filter);

		verify(filter).addLessThanOrEqualTo("name", "A");
		verify(filter).addGreaterThan("name", "Z");
		verify(filter).addNotNull("name");
		verify(filter).addNull("name");
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalNotInvertsStartsEndsAndEqualityOperators() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> startsWith = criterion("name", SmartClientFilterOperator.startsWith, "ab");
		java.util.Map<String, Object> iStartsWith = criterion("name", SmartClientFilterOperator.iStartsWith, "cd");
		java.util.Map<String, Object> notStartsWith = criterion("name", SmartClientFilterOperator.notStartsWith, "ef");
		java.util.Map<String, Object> iNotStartsWith = criterion("name", SmartClientFilterOperator.iNotStartsWith, "gh");
		java.util.Map<String, Object> endsWith = criterion("name", SmartClientFilterOperator.endsWith, "ij");
		java.util.Map<String, Object> iEndsWith = criterion("name", SmartClientFilterOperator.iEndsWith, "kl");
		java.util.Map<String, Object> notEndsWith = criterion("name", SmartClientFilterOperator.notEndsWith, "mn");
		java.util.Map<String, Object> iNotEndsWith = criterion("name", SmartClientFilterOperator.iNotEndsWith, "op");
		java.util.Map<String, Object> exact = criterion("name", SmartClientFilterOperator.exact, "q");
		java.util.Map<String, Object> iEquals = criterion("name", SmartClientFilterOperator.iEquals, "r");
		java.util.Map<String, Object> notEqual = criterion("name", SmartClientFilterOperator.notEqual, "s");
		java.util.Map<String, Object> iNotEqual = criterion("name", SmartClientFilterOperator.iNotEqual, "t");

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(startsWith,
						iStartsWith,
						notStartsWith,
						iNotStartsWith,
						endsWith,
						iEndsWith,
						notEndsWith,
						iNotEndsWith,
						exact,
						iEquals,
						notEqual,
						iNotEqual),
				null,
				model,
				filter);

		verify(filter).addNotStartsWith("name", "ab");
		verify(filter).addNotStartsWith("name", "cd");
		verify(filter).addStartsWith("name", "ef");
		verify(filter).addStartsWith("name", "gh");
		verify(filter).addNotEndsWith("name", "ij");
		verify(filter).addNotEndsWith("name", "kl");
		verify(filter).addEndsWith("name", "mn");
		verify(filter).addEndsWith("name", "op");
		verify(filter).addNotEquals("name", "q");
		verify(filter).addNotEqualsIgnoreCase("name", "r");
		verify(filter).addEquals("name", "s");
		verify(filter).addEqualsIgnoreCase("name", "t");
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalNotInvertsRelationalAndBetweenOperators() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> greaterThan = criterion("name", SmartClientFilterOperator.greaterThan, "10");
		java.util.Map<String, Object> greaterOrEqual = criterion("name", SmartClientFilterOperator.greaterOrEqual, "11");
		java.util.Map<String, Object> lessThan = criterion("name", SmartClientFilterOperator.lessThan, "12");
		java.util.Map<String, Object> lessOrEqual = criterion("name", SmartClientFilterOperator.lessOrEqual, "13");
		java.util.Map<String, Object> iBetween = criterion("name", SmartClientFilterOperator.iBetween, null);
		iBetween.put("start", "A");
		iBetween.put("end", "Z");

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(greaterThan, greaterOrEqual, lessThan, lessOrEqual, iBetween),
				null,
				model,
				filter);

		verify(filter).addLessThanOrEqualTo("name", "10");
		verify(filter).addLessThan("name", "11");
		verify(filter).addGreaterThanOrEqualTo("name", "12");
		verify(filter).addGreaterThan("name", "13");
		verify(filter).addLessThanOrEqualTo("name", "A");
		verify(filter).addGreaterThanOrEqualTo("name", "Z");
	}

	@Test
	@SuppressWarnings("boxing")
	void addAdvancedFilterCriteriaInternalNotNoOpsForFieldPatternGeoAndLogicalOperators() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		ListModel<?> model = mock(ListModel.class);
		Filter filter = mock(Filter.class);
		Text nameField = new Text();
		nameField.setName("name");
		nameField.setLength(100);

		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(document.getAttribute("name")).thenReturn(nameField);

		java.util.Map<String, Object> equalsField = criterion("name", SmartClientFilterOperator.equalsField, "x");
		java.util.Map<String, Object> iEqualsField = criterion("name", SmartClientFilterOperator.iEqualsField, "x");
		java.util.Map<String, Object> notEqualField = criterion("name", SmartClientFilterOperator.notEqualField, "x");
		java.util.Map<String, Object> iNotEqualField = criterion("name", SmartClientFilterOperator.iNotEqualField, "x");
		java.util.Map<String, Object> containsField = criterion("name", SmartClientFilterOperator.containsField, "x");
		java.util.Map<String, Object> iContainsField = criterion("name", SmartClientFilterOperator.iContainsField, "x");
		java.util.Map<String, Object> notContainsField = criterion("name", SmartClientFilterOperator.notContainsField, "x");
		java.util.Map<String, Object> iNotContainsField = criterion("name", SmartClientFilterOperator.iNotContainsField, "x");
		java.util.Map<String, Object> startsWithField = criterion("name", SmartClientFilterOperator.startsWithField, "x");
		java.util.Map<String, Object> iStartsWithField = criterion("name", SmartClientFilterOperator.iStartsWithField, "x");
		java.util.Map<String, Object> notStartsWithField = criterion("name", SmartClientFilterOperator.notStartsWithField, "x");
		java.util.Map<String, Object> iNotStartsWithField = criterion("name", SmartClientFilterOperator.iNotStartsWithField, "x");
		java.util.Map<String, Object> endsWithField = criterion("name", SmartClientFilterOperator.endsWithField, "x");
		java.util.Map<String, Object> iEndsWithField = criterion("name", SmartClientFilterOperator.iEndsWithField, "x");
		java.util.Map<String, Object> notEndsWithField = criterion("name", SmartClientFilterOperator.notEndsWithField, "x");
		java.util.Map<String, Object> iNotEndsWithField = criterion("name", SmartClientFilterOperator.iNotEndsWithField, "x");
		java.util.Map<String, Object> greaterThanField = criterion("name", SmartClientFilterOperator.greaterThanField, "x");
		java.util.Map<String, Object> greaterOrEqualField = criterion("name", SmartClientFilterOperator.greaterOrEqualField, "x");
		java.util.Map<String, Object> lessThanField = criterion("name", SmartClientFilterOperator.lessThanField, "x");
		java.util.Map<String, Object> lessOrEqualField = criterion("name", SmartClientFilterOperator.lessOrEqualField, "x");
		java.util.Map<String, Object> regexp = criterion("name", SmartClientFilterOperator.regexp, "x");
		java.util.Map<String, Object> iregexp = criterion("name", SmartClientFilterOperator.iregexp, "x");
		java.util.Map<String, Object> containsPattern = criterion("name", SmartClientFilterOperator.containsPattern, "x");
		java.util.Map<String, Object> iContainsPattern = criterion("name", SmartClientFilterOperator.iContainsPattern, "x");
		java.util.Map<String, Object> matchesPattern = criterion("name", SmartClientFilterOperator.matchesPattern, "x");
		java.util.Map<String, Object> iMatchesPattern = criterion("name", SmartClientFilterOperator.iMatchesPattern, "x");
		java.util.Map<String, Object> startsWithPattern = criterion("name", SmartClientFilterOperator.startsWithPattern, "x");
		java.util.Map<String, Object> iStartsWithPattern = criterion("name", SmartClientFilterOperator.iStartsWithPattern, "x");
		java.util.Map<String, Object> endsWithPattern = criterion("name", SmartClientFilterOperator.endsWithPattern, "x");
		java.util.Map<String, Object> iEndsWithPattern = criterion("name", SmartClientFilterOperator.iEndsWithPattern, "x");
		java.util.Map<String, Object> geoContains = criterion("name", SmartClientFilterOperator.geoContains, "x");
		java.util.Map<String, Object> geoCrosses = criterion("name", SmartClientFilterOperator.geoCrosses, "x");
		java.util.Map<String, Object> geoDisjoint = criterion("name", SmartClientFilterOperator.geoDisjoint, "x");
		java.util.Map<String, Object> geoEquals = criterion("name", SmartClientFilterOperator.geoEquals, "x");
		java.util.Map<String, Object> geoIntersects = criterion("name", SmartClientFilterOperator.geoIntersects, "x");
		java.util.Map<String, Object> geoOverlaps = criterion("name", SmartClientFilterOperator.geoOverlaps, "x");
		java.util.Map<String, Object> geoTouches = criterion("name", SmartClientFilterOperator.geoTouches, "x");
		java.util.Map<String, Object> geoWithin = criterion("name", SmartClientFilterOperator.geoWithin, "x");
		java.util.Map<String, Object> and = criterion("name", SmartClientFilterOperator.and, "x");
		java.util.Map<String, Object> or = criterion("name", SmartClientFilterOperator.or, "x");
		java.util.Map<String, Object> not = criterion("name", SmartClientFilterOperator.not, "x");

		invokeAddAdvancedFilterCriteriaToQueryInternal(module,
				document,
				user,
				compoundFilterOperator("not"),
				java.util.List.of(equalsField,
						iEqualsField,
						notEqualField,
						iNotEqualField,
						containsField,
						iContainsField,
						notContainsField,
						iNotContainsField,
						startsWithField,
						iStartsWithField,
						notStartsWithField,
						iNotStartsWithField,
						endsWithField,
						iEndsWithField,
						notEndsWithField,
						iNotEndsWithField,
						greaterThanField,
						greaterOrEqualField,
						lessThanField,
						lessOrEqualField,
						regexp,
						iregexp,
						containsPattern,
						iContainsPattern,
						matchesPattern,
						iMatchesPattern,
						startsWithPattern,
						iStartsWithPattern,
						endsWithPattern,
						iEndsWithPattern,
						geoContains,
						geoCrosses,
						geoDisjoint,
						geoEquals,
						geoIntersects,
						geoOverlaps,
						geoTouches,
						geoWithin,
						and,
						or,
						not),
				null,
				model,
				filter);

		verifyNoInteractions(filter);
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

	@Test
	void returnTagUpdateMessageNullsFlagForUserWithoutPermissionAndSetsTagState() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);

		org.mockito.Mockito.when(user.canFlag()).thenReturn(false);
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		org.mockito.Mockito.when(document.getName()).thenReturn("Contact");
		org.mockito.Mockito.when(model.getDrivingDocument()).thenReturn(document);
		org.mockito.Mockito.when(model.getProjections()).thenReturn(java.util.Set.of(PersistentBean.TAGGED_NAME,
				PersistentBean.FLAG_COMMENT_NAME,
				"name"));

		java.util.Map<String, Object> parameters = new java.util.HashMap<>();
		parameters.put(PersistentBean.FLAG_COMMENT_NAME, "sensitive");
		parameters.put("name", "Alice");

		StringBuilder message = invokeReturnTagUpdateMessage(user, customer, parameters, module, model, true);

		assertNotNull(message);
		assertTrue(message.toString().contains("\"" + PersistentBean.TAGGED_NAME + "\":true"));
		assertTrue(message.toString().contains("\"" + PersistentBean.FLAG_COMMENT_NAME + "\":null"));
		assertTrue(message.toString().contains("\"name\":\"Alice\""));
	}

	@Test
	void returnTagUpdateMessageKeepsFlagForUserWithPermissionAndClearsTagStateOnUntag() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);

		org.mockito.Mockito.when(user.canFlag()).thenReturn(true);
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		org.mockito.Mockito.when(document.getName()).thenReturn("Contact");
		org.mockito.Mockito.when(model.getDrivingDocument()).thenReturn(document);
		org.mockito.Mockito.when(model.getProjections()).thenReturn(java.util.Set.of(PersistentBean.TAGGED_NAME,
				PersistentBean.FLAG_COMMENT_NAME,
				"name"));

		java.util.Map<String, Object> parameters = new java.util.HashMap<>();
		parameters.put(PersistentBean.FLAG_COMMENT_NAME, "visible-note");
		parameters.put("name", "Alice");

		StringBuilder message = invokeReturnTagUpdateMessage(user, customer, parameters, module, model, false);

		assertNotNull(message);
		assertTrue(message.toString().contains("\"" + PersistentBean.TAGGED_NAME + "\":false"));
		assertTrue(message.toString().contains("\"" + PersistentBean.FLAG_COMMENT_NAME + "\":\"visible-note\""));
		assertTrue(message.toString().contains("\"name\":\"Alice\""));
	}

	@Test
	void returnUpdatedMessageMasksFlagWhenUserCannotFlagAndPreservesTaggedState() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);

		org.mockito.Mockito.when(user.canFlag()).thenReturn(false);
		org.mockito.Mockito.when(model.getColumns()).thenReturn(java.util.Collections.emptyList());
		org.mockito.Mockito.when(model.getProjections()).thenReturn(java.util.Set.of(PersistentBean.TAGGED_NAME,
				PersistentBean.FLAG_COMMENT_NAME,
				"name"));

		java.util.TreeMap<String, Object> beanValues = new java.util.TreeMap<>();
		beanValues.put(PersistentBean.TAGGED_NAME, null);
		beanValues.put(PersistentBean.FLAG_COMMENT_NAME, "private-note");
		beanValues.put("name", "Bob");
		DynamicBean bean = new DynamicBean("admin", "Contact", beanValues);

		StringBuilder message = invokeReturnUpdatedMessage(user, customer, module, document, model, bean, true);

		assertNotNull(message);
		assertTrue(message.toString().contains("\"" + PersistentBean.TAGGED_NAME + "\":true"));
		assertTrue(message.toString().contains("\"" + PersistentBean.FLAG_COMMENT_NAME + "\":null"));
		assertTrue(message.toString().contains("\"name\":\"Bob\""));
	}

	@Test
	void returnUpdatedMessageKeepsFlagWhenUserCanFlagAndDoesNotForceTaggedState() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);

		org.mockito.Mockito.when(user.canFlag()).thenReturn(true);
		org.mockito.Mockito.when(model.getColumns()).thenReturn(java.util.Collections.emptyList());
		org.mockito.Mockito.when(model.getProjections()).thenReturn(java.util.Set.of(PersistentBean.TAGGED_NAME,
				PersistentBean.FLAG_COMMENT_NAME,
				"name"));

		java.util.TreeMap<String, Object> beanValues = new java.util.TreeMap<>();
		beanValues.put(PersistentBean.TAGGED_NAME, Boolean.FALSE);
		beanValues.put(PersistentBean.FLAG_COMMENT_NAME, "visible-note");
		beanValues.put("name", "Carol");
		DynamicBean bean = new DynamicBean("admin", "Contact", beanValues);

		StringBuilder message = invokeReturnUpdatedMessage(user, customer, module, document, model, bean, false);

		assertNotNull(message);
		assertTrue(message.toString().contains("\"" + PersistentBean.TAGGED_NAME + "\":false"));
		assertTrue(message.toString().contains("\"" + PersistentBean.FLAG_COMMENT_NAME + "\":\"visible-note\""));
		assertTrue(message.toString().contains("\"name\":\"Carol\""));
	}

	@Test
	void removeDeletesModelRowAndReturnsStatusOkPayload() throws Exception {
		ListModel<Bean> model = mock(ListModel.class);
		java.io.StringWriter sink = new java.io.StringWriter();
		java.io.PrintWriter pw = new java.io.PrintWriter(sink);
		java.util.Map<String, Object> parameters = new java.util.HashMap<>();
		parameters.put(Bean.DOCUMENT_ID, "biz-123");

		invokeRemove(model, parameters, pw);
		pw.flush();

		verify(model).remove("biz-123");
		assertEquals("{\"response\":{\"status\":0}}", sink.toString());
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

	private static StringBuilder invokeReturnTagUpdateMessage(User user,
											Customer customer,
											java.util.Map<String, Object> parameters,
											Module module,
											ListModel<Bean> model,
											boolean tagging) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("returnTagUpdateMessage",
				User.class,
				Customer.class,
				Map.class,
				Module.class,
				ListModel.class,
				boolean.class);
		method.setAccessible(true);
		return (StringBuilder) method.invoke(null, user, customer, parameters, module, model, Boolean.valueOf(tagging));
	}

	private static StringBuilder invokeReturnUpdatedMessage(User user,
										Customer customer,
										Module module,
										Document document,
										ListModel<Bean> model,
										Bean bean,
										boolean rowIsTagged) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("returnUpdatedMessage",
				User.class,
				Customer.class,
				Module.class,
				Document.class,
				ListModel.class,
				Bean.class,
				boolean.class);
		method.setAccessible(true);
		return (StringBuilder) method.invoke(null,
				user,
				customer,
				module,
				document,
				model,
				bean,
				Boolean.valueOf(rowIsTagged));
	}

	private static void invokeRemove(ListModel<Bean> model,
								java.util.Map<String, Object> parameters,
								java.io.PrintWriter pw) throws Exception {
		Method method = SmartClientListServlet.class.getDeclaredMethod("remove", ListModel.class, Map.class, java.io.PrintWriter.class);
		method.setAccessible(true);
		invokeVoid(method, model, parameters, pw);
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

	private static java.util.Map<String, Object> criterion(String fieldName,
								SmartClientFilterOperator operator,
								Object value) {
		java.util.Map<String, Object> criterion = new java.util.HashMap<>();
		criterion.put("fieldName", fieldName);
		criterion.put("operator", operator.toString());
		criterion.put("value", value);
		return criterion;
	}
}

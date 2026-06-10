package org.skyve.impl.snapshot;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.snapshot.Snapshot.AdvancedSearchType;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;

/**
 * Tests for {@code VueSnapshotAdapter#toClientPayload(Snapshot)}.
 * Uses {@code SnapshotAdapter.VUE} which is the package-private singleton.
 */
@SuppressWarnings("static-method")
class VueSnapshotAdapterTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void toClientPayloadEmptySnapshotContainsDefaultOperatorAndEmptyLists() {
		Snapshot snapshot = new Snapshot();
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"operator\""));
		assertThat(payload, containsString("\"visibleColumns\""));
		assertThat(payload, containsString("\"columnWidths\""));
	}

	@Test
	void toClientPayloadWithColumnNoWidthSetsAllWidthsPresentFalse() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("name"));
		// When a column has no width, allWidthsPresent=false, so columnWidths is NOT cleared
		assertThat(payload, containsString("\"columnWidths\":[]"));
	}

	@Test
	void toClientPayloadWithColumnWithWidthColumnWidthsListIsCleared() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name", 150);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("name"));
		// allWidthsPresent=true → columnWidths cleared → empty list in payload
		assertThat(payload, containsString("\"columnWidths\":[]"));
	}

	@Test
	void toClientPayloadWithSortAscendingContainsColumnName() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");
		snapshot.putSort("name", SortDirection.ascending);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"sortColumns\""));
		assertThat(payload, containsString("name"));
	}

	@Test
	void toClientPayloadWithSortDescendingContainsDashPrefix() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("amount");
		snapshot.putSort("amount", SortDirection.descending);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		// descending sort produces "-amount" in the sort columns list
		assertThat(payload, containsString("-amount"));
	}

	@Test
	void toClientPayloadWithSummaryContainsSummarySelectionValue() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("price");
		snapshot.setSummary(AggregateFunction.Sum);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"summarySelection\":\"Sum\""));
	}

	@Test
	void toClientPayloadNoSummaryContainsEmptySummarySelection() {
		Snapshot snapshot = new Snapshot();
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"summarySelection\":\"\""));
	}

	@Test
	void toClientPayloadWithSimpleCriterionFilterContainsFilters() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("name");
		criterion.setOperator(SmartClientFilterOperator.iContains);
		criterion.setValue("Alice");
		snapshot.setFilter(criterion);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"filters\""));
		assertThat(payload, containsString("name"));
	}

	@Test
	void toClientPayloadWithInconvertibleCriteriaAndNoSmartClientSourceReturnsNull() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");

		// A "not" criteria cannot be restructured for Vue — returns null from filters()
		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.not);
		snapshot.setFilter(criteria);
		// no sourceSmartClientCriteria set → should throw → toClientPayload returns null

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when filter cannot be converted to Vue format");
	}

	@Test
	void toClientPayloadWithInconvertibleCriteriaAndSmartClientSourceContainsSmartClientCriteria() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");

		// A "not" criteria cannot be restructured for Vue — returns null from filters()
		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.not);
		snapshot.setFilter(criteria);

		// Set a fallback SmartClient source criteria
		Map<String, Object> source = new LinkedHashMap<>();
		source.put("_constructor", "AdvancedCriteria");
		snapshot.setSourceSmartClientCriteria(source);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("smartClientCriteria"));
	}

	@Test
	void toClientPayloadWithMultipleSortsIncludesAllSortColumns() {
		Snapshot snapshot = new Snapshot();
		snapshot.putSort("name", SortDirection.ascending);
		snapshot.putSort("age", SortDirection.descending);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"name\""));
		assertThat(payload, containsString("\"-age\""));
	}

	@Test
	void toClientPayloadWithCriteriaHavingNullOperatorCriterionReturnsNull() {
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.and);
		// criterion with null operator inside criteria — not in VUE_ALLOWED_CONSTRAINT_OPERATORS
		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("name");
		setField(criterion, "operator", null); // null operator → not in VUE_ALLOWED_CONSTRAINT_OPERATORS
		criterion.setValue("a");
		criteria.getFilters().add(criterion);
		snapshot.setFilter(criteria);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null for criteria with null criterion operator");
	}

	@Test
	void toClientPayloadWithOrCriteriaContainsOrOperator() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("status");

		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.or);

		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("status");
		c1.setOperator(SmartClientFilterOperator.equals);
		c1.setValue("active");
		criteria.getFilters().add(c1);

		SnapshotCriterion c2 = new SnapshotCriterion();
		c2.setColumn("status");
		c2.setOperator(SmartClientFilterOperator.equals);
		c2.setValue("pending");
		criteria.getFilters().add(c2);

		snapshot.setFilter(criteria);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"filters\""));
	}

	@Test
	void toClientPayloadWithNestedCriteriaWithMixedColumnsReturnsNull() {
		Snapshot snapshot = new Snapshot();

		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		SnapshotCriteria inner = new SnapshotCriteria();
		inner.setOperator(CompoundFilterOperator.or);
		// Two different columns — heterogeneous, should fail restructure
		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("name");
		c1.setOperator(SmartClientFilterOperator.equals);
		c1.setValue("Alice");

		SnapshotCriterion c2 = new SnapshotCriterion();
		c2.setColumn("age"); // different column
		c2.setOperator(SmartClientFilterOperator.equals);
		c2.setValue("30");

		inner.getFilters().add(c1);
		inner.getFilters().add(c2);
		outer.getFilters().add(inner);

		snapshot.setFilter(outer);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when nested criteria have heterogeneous columns");
	}

	@Test
	void toClientPayloadWithNullOperatorCriteriaReturnsNull() {
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria criteria = new SnapshotCriteria();
		setField(criteria, "operator", null);
		snapshot.setFilter(criteria);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload);
	}

	@Test
	void toClientPayloadWithStartEndCriterionReturnsNull() {
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.and);

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("date");
		criterion.setStart("2020-01-01");
		criterion.setEnd("2020-12-31");
		criteria.getFilters().add(criterion);

		snapshot.setFilter(criteria);
		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when criterion has start/end (range)");
	}

	@Test
	void toClientPayloadWithNestedCriteriaContainingStartEndReturnsNull() {
		// Covers the branch: bottomCriterion.getStart() != null inside restructure
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		SnapshotCriteria inner = new SnapshotCriteria();
		inner.setOperator(CompoundFilterOperator.or);

		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("date");
		c.setStart("2020-01-01");
		c.setEnd("2020-12-31");
		inner.getFilters().add(c);
		outer.getFilters().add(inner);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when nested criterion has start/end");
	}

	@Test
	void toClientPayloadWithNestedCriteriaHavingNotOperatorReturnsNull() {
		// Covers: middleCriteriaOperator == not → return null in restructure
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		SnapshotCriteria inner = new SnapshotCriteria();
		inner.setOperator(CompoundFilterOperator.not);

		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("status");
		c.setOperator(SmartClientFilterOperator.equals);
		c.setValue("active");
		inner.getFilters().add(c);
		outer.getFilters().add(inner);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when inner criteria uses 'not' operator");
	}

	@Test
	void toClientPayloadWithDeepNestingReturnsNull() {
		// Covers: bottomFilter instanceof SnapshotCriteria → return null (infinite nesting)
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		SnapshotCriteria inner = new SnapshotCriteria();
		inner.setOperator(CompoundFilterOperator.or);

		// Add another nested SnapshotCriteria (depth > 2 → unsupported)
		SnapshotCriteria deepNested = new SnapshotCriteria();
		deepNested.setOperator(CompoundFilterOperator.and);
		inner.getFilters().add(deepNested);
		outer.getFilters().add(inner);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when filter nesting depth exceeds supported level");
	}

	@Test
	void toClientPayloadWithNestedCriteriaHavingUnsupportedOperatorReturnsNull() {
		// Covers: bottomCriterionOperator not in VUE_ALLOWED_CONSTRAINT_OPERATORS → return null
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		SnapshotCriteria inner = new SnapshotCriteria();
		inner.setOperator(CompoundFilterOperator.or);

		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("status");
		c.setOperator(SmartClientFilterOperator.betweenInclusive); // not in VUE_ALLOWED_CONSTRAINT_OPERATORS
		c.setValue("x");
		inner.getFilters().add(c);
		outer.getFilters().add(inner);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when nested criterion uses unsupported operator");
	}

	@Test
	void toClientPayloadWithNestedCriteriaConflictingOperatorReturnsNull() {
		// Covers: criteria already added to same column but different operator → return null
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		// First nested criteria for column "status" with "and" operator
		SnapshotCriteria inner1 = new SnapshotCriteria();
		inner1.setOperator(CompoundFilterOperator.or);
		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("status");
		c1.setOperator(SmartClientFilterOperator.equals);
		c1.setValue("active");
		inner1.getFilters().add(c1);

		// Second nested criteria for column "status" with different operator "and" → conflicts
		SnapshotCriteria inner2 = new SnapshotCriteria();
		inner2.setOperator(CompoundFilterOperator.and);
		SnapshotCriterion c2 = new SnapshotCriterion();
		c2.setColumn("status");
		c2.setOperator(SmartClientFilterOperator.equals);
		c2.setValue("inactive");
		inner2.getFilters().add(c2);

		outer.getFilters().add(inner1);
		outer.getFilters().add(inner2);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when nested criteria have conflicting operators for same column");
	}

	@Test
	void toClientPayloadWithDirectCriterionHavingStartEndThrowsAndReturnsNull() {
		// Covers: constraint() throws when criterion has start/end
		Snapshot snapshot = new Snapshot();
		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("date");
		criterion.setStart("2020-01-01");
		criterion.setEnd("2020-12-31");
		snapshot.setFilter(criterion);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNull(payload, "Expected null when direct criterion has start/end");
	}

	@Test
	void toClientPayloadWithCriteriaAndExistingAndOperatorMergesFilters() {
		// Covers: criteria already added for same column with same "and" operator (merge)
		Snapshot snapshot = new Snapshot();
		SnapshotCriteria outer = new SnapshotCriteria();
		outer.setOperator(CompoundFilterOperator.and);

		// Two nested criteria for same column "name" both with "and" → should merge
		SnapshotCriteria inner1 = new SnapshotCriteria();
		inner1.setOperator(CompoundFilterOperator.and);
		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("name");
		c1.setOperator(SmartClientFilterOperator.iContains);
		c1.setValue("Alice");
		inner1.getFilters().add(c1);

		SnapshotCriteria inner2 = new SnapshotCriteria();
		inner2.setOperator(CompoundFilterOperator.and);
		SnapshotCriterion c2 = new SnapshotCriterion();
		c2.setColumn("name");
		c2.setOperator(SmartClientFilterOperator.iContains);
		c2.setValue("Smith");
		inner2.getFilters().add(c2);

		outer.getFilters().add(inner1);
		outer.getFilters().add(inner2);
		snapshot.setFilter(outer);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload, "Expected valid payload when nested criteria for same column have compatible operators");
		assertThat(payload, containsString("name"));
	}

	@Test
	void fromClientPayloadParsesColumnsFiltersSortsAndSummary() throws Exception {
		bindMockUser();

		Map<String, Object> statusFilter = filter("and", constraints(
				constraint(SmartClientFilterOperator.equals, "active")));
		Map<String, Object> nameFilter = filter("or", constraints(
				constraint(SmartClientFilterOperator.iStartsWith, "Al"),
				constraint(SmartClientFilterOperator.iContains, null),
				constraint(SmartClientFilterOperator.iContains, "Bob")));
		Map<String, Object> filters = new LinkedHashMap<>();
		filters.put("status", statusFilter);
		filters.put("name", nameFilter);

		Snapshot result = SnapshotAdapter.VUE.fromClientPayload(vuePayloadJson(filters,
				List.of(),
				List.of(Integer.valueOf(120)),
				"and",
				List.of("status", "-createdDate", "", " "),
				AggregateFunction.Count.toString()));

		assertNotNull(result);
		assertEquals(Integer.valueOf(120), result.getColumns().get("status"));
		assertTrue(result.getColumns().containsKey("name"));
		assertNull(result.getColumns().get("name"));
		assertEquals(SortDirection.ascending, result.getSorts().get("status"));
		assertEquals(SortDirection.descending, result.getSorts().get("createdDate"));
		assertEquals(AggregateFunction.Count, result.getSummary());
		assertEquals(AdvancedSearchType.bracket, result.getAdvanced());

		SnapshotCriteria top = assertInstanceOf(SnapshotCriteria.class, result.getFilter());
		assertEquals(CompoundFilterOperator.and, top.getOperator());
		assertEquals(2, top.getFilters().size());
		SnapshotCriterion status = assertInstanceOf(SnapshotCriterion.class, top.getFilters().get(0));
		assertEquals("status", status.getColumn());
		assertEquals(SmartClientFilterOperator.equals, status.getOperator());
		assertEquals("active", status.getValue());
		SnapshotCriteria name = assertInstanceOf(SnapshotCriteria.class, top.getFilters().get(1));
		assertEquals(CompoundFilterOperator.or, name.getOperator());
		assertEquals(2, name.getFilters().size());
	}

	@Test
	void fromClientPayloadReturnsNullForMalformedPayloadSections() throws Exception {
		bindMockUser();

		List<Map<String, Object>> malformedPayloads = List.of(
				vuePayload("notAMap", List.of("name"), List.of(), "and", List.of(), ""),
				vuePayload(new LinkedHashMap<>(), List.of("name"), "notAList", "and", List.of(), ""),
				vuePayload(new LinkedHashMap<>(), "notAList", List.of(), "and", List.of(), ""),
				vuePayload(new LinkedHashMap<>(), List.of("name"), List.of(), Integer.valueOf(42), List.of(), ""),
				vuePayload(Map.of("name", "notAMap"), List.of("name"), List.of(), "and", List.of(), ""),
				vuePayload(Map.of("name", Map.of("constraints", "notAList")), List.of("name"), List.of(), "and", List.of(), ""),
				vuePayload(Map.of("name", filter(Integer.valueOf(42), constraints(
						constraint(SmartClientFilterOperator.equals, "active"),
						constraint(SmartClientFilterOperator.iContains, "pending")))), List.of("name"), List.of(), "and", List.of(), ""),
				vuePayload(Map.of("name", filter("and", constraints(Map.of("value", "active")))), List.of("name"), List.of(), "and", List.of(), ""),
				vuePayload(new LinkedHashMap<>(), List.of("name"), List.of(), "and", "notAList", ""),
				vuePayload(new LinkedHashMap<>(), List.of("name"), List.of(), "and", List.of(), Integer.valueOf(42)));

		for (Map<String, Object> payload : malformedPayloads) {
			assertNull(SnapshotAdapter.VUE.fromClientPayload(JSON.marshall(payload)));
		}
	}

	private static Map<String, Object> vuePayload(Object filters,
													Object visibleColumns,
													Object columnWidths,
													Object operator,
													Object sortColumns,
													Object summarySelection) {
		Map<String, Object> payload = new LinkedHashMap<>();
		payload.put("filters", filters);
		payload.put("visibleColumns", visibleColumns);
		payload.put("columnWidths", columnWidths);
		payload.put("operator", operator);
		payload.put("sortColumns", sortColumns);
		payload.put("summarySelection", summarySelection);
		return payload;
	}

	private static String vuePayloadJson(Map<String, Object> filters,
											List<String> visibleColumns,
											List<Integer> columnWidths,
											String operator,
											List<String> sortColumns,
											String summarySelection)
	{
		return JSON.marshall(vuePayload(filters, visibleColumns, columnWidths, operator, sortColumns, summarySelection));
	}

	private static void setField(Object target, String fieldName, Object value) {
		try {
			Field field = target.getClass().getDeclaredField(fieldName);
			field.setAccessible(true);
			field.set(target, value);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	private static Map<String, Object> filter(Object operator, List<Map<String, Object>> constraints) {
		Map<String, Object> filter = new LinkedHashMap<>();
		filter.put("operator", operator);
		filter.put("constraints", constraints);
		return filter;
	}

	@SafeVarargs
	private static List<Map<String, Object>> constraints(Map<String, Object>... constraints) {
		return Arrays.asList(constraints);
	}

	private static Map<String, Object> constraint(SmartClientFilterOperator matchMode, Object value) {
		Map<String, Object> constraint = new LinkedHashMap<>();
		constraint.put("matchMode", matchMode.toString());
		constraint.put("value", value);
		return constraint;
	}

	private static void bindMockUser() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(mock(User.class));
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}

package org.skyve.impl.snapshot;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.impl.snapshot.CompoundFilterOperator;

/**
 * Tests for {@code VueSnapshotAdapter#toClientPayload(Snapshot)}.
 * Uses {@code SnapshotAdapter.VUE} which is the package-private singleton.
 */
@SuppressWarnings("static-method")
class VueSnapshotAdapterTest {

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
		criterion.setOperator(null); // null operator → not in VUE_ALLOWED_CONSTRAINT_OPERATORS
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
		criteria.setOperator(null);
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
}


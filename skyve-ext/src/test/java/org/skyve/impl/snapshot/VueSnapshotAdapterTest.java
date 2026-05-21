package org.skyve.impl.snapshot;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

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
	void toClientPayloadWithConvertibleCriteriaContainsFiltersAndOperator() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("status");

		// Build an "and" criteria with one criterion — this should be convertible
		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.and);

		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("status");
		c1.setOperator(SmartClientFilterOperator.equals);
		c1.setValue("active");
		criteria.getFilters().add(c1);

		snapshot.setFilter(criteria);

		String payload = SnapshotAdapter.VUE.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"filters\""));
		assertThat(payload, containsString("status"));
		assertThat(payload, containsString("active"));
	}
}

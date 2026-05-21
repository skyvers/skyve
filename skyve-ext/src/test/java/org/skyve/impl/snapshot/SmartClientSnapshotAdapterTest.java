package org.skyve.impl.snapshot;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.snapshot.Snapshot.AdvancedSearchType;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

/**
 * Tests for {@code SmartClientSnapshotAdapter#toClientPayload(Snapshot)}.
 * Uses {@code SnapshotAdapter.SMART_CLIENT} which is the package-private singleton.
 */
@SuppressWarnings("static-method")
class SmartClientSnapshotAdapterTest {

	@Test
	void toClientPayloadEmptySnapshotContainsNullAdvancedCriteriaStyle() {
		Snapshot snapshot = new Snapshot();
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"advancedCriteriaStyle\":null"));
	}

	@Test
	void toClientPayloadWithAdvancedTypeContainsAdvancedCriteriaStyle() {
		Snapshot snapshot = new Snapshot();
		snapshot.setAdvanced(AdvancedSearchType.radio);
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"advancedCriteriaStyle\":\"radio\""));
	}

	@Test
	void toClientPayloadColumnWithNoWidthContainsAutoFitWidth() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("myColumn");
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("myColumn"));
		assertThat(payload, containsString("autoFitWidth:false"));
	}

	@Test
	void toClientPayloadColumnWithWidthContainsWidth() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("myColumn", 150);
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("myColumn"));
		assertThat(payload, containsString("width:150"));
	}

	@Test
	void toClientPayloadWithSortContainsSortState() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("myColumn");
		snapshot.putSort("myColumn", SortDirection.descending);
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("sortSpecifiers"));
		assertThat(payload, containsString("myColumn"));
		assertThat(payload, containsString("descending"));
	}

	@Test
	void toClientPayloadEmptySortContainsNullSortState() {
		Snapshot snapshot = new Snapshot();
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"sortState\":null"));
	}

	@Test
	void toClientPayloadWithGroupContainsGroupState() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");
		snapshot.setGroup("category");
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("category"));
		assertThat(payload, containsString("fieldName"));
	}

	@Test
	void toClientPayloadWithSummaryContainsSummaryType() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("amount");
		snapshot.setSummary(AggregateFunction.Sum);
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("Sum"));
	}

	@Test
	void toClientPayloadWithSimpleCriterionContainsCriteria() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("name");
		criterion.setOperator(SmartClientFilterOperator.iContains);
		criterion.setValue("Alice");
		snapshot.setFilter(criterion);

		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("name"));
		assertThat(payload, containsString("iContains"));
		assertThat(payload, containsString("Alice"));
	}

	@Test
	void toClientPayloadWithNullFilterContainsEmptyCriteria() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");
		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("\"criteria\":{}"));
	}

	@Test
	void toClientPayloadWithCriterionUsingStartEndContainsStartEnd() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("date");

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("date");
		criterion.setOperator(SmartClientFilterOperator.betweenInclusive);
		criterion.setStart("2024-01-01");
		criterion.setEnd("2024-12-31");
		snapshot.setFilter(criterion);

		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("2024-01-01"));
		assertThat(payload, containsString("2024-12-31"));
		assertThat(payload, not(containsString("\"value\"")));
	}

	@Test
	void toClientPayloadWithCompoundCriteriaContainsAdvancedCriteriaConstructor() {
		Snapshot snapshot = new Snapshot();
		snapshot.putColumn("name");

		SnapshotCriteria criteria = new SnapshotCriteria();
		criteria.setOperator(CompoundFilterOperator.and);

		SnapshotCriterion c1 = new SnapshotCriterion();
		c1.setColumn("name");
		c1.setOperator(SmartClientFilterOperator.iStartsWith);
		c1.setValue("Jo");
		criteria.getFilters().add(c1);

		snapshot.setFilter(criteria);

		String payload = SnapshotAdapter.SMART_CLIENT.toClientPayload(snapshot);
		assertNotNull(payload);
		assertThat(payload, containsString("AdvancedCriteria"));
		assertThat(payload, containsString("Jo"));
	}
}

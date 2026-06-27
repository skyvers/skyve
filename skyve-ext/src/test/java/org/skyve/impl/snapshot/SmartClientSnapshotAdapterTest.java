package org.skyve.impl.snapshot;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
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
import org.skyve.domain.PersistentBean;
import org.skyve.impl.snapshot.Snapshot.AdvancedSearchType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;

/**
 * Tests for {@code SmartClientSnapshotAdapter#toClientPayload(Snapshot)}.
 * Uses {@code SnapshotAdapter.SMART_CLIENT} which is the package-private singleton.
 */
@SuppressWarnings("static-method")
class SmartClientSnapshotAdapterTest {

	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

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

	@Test
	void fromClientPayloadReturnsNullForMalformedPayload() throws Exception {
		bindMockUser();

		Snapshot result = SnapshotAdapter.SMART_CLIENT.fromClientPayload("{\"fieldState\":42}");

		assertNull(result);
	}

	@Test
	void fromClientPayloadParsesDisplayStateAndSimpleMapCriteria() throws Exception {
		bindMockUser();

		Map<String, Object> criteria = new LinkedHashMap<>();
		criteria.put("status", Arrays.asList("active", "pending"));
		criteria.put("owner", List.of("mike"));
		criteria.put("description", "alpha");

		Snapshot result = SnapshotAdapter.SMART_CLIENT.fromClientPayload(clientPayload(AdvancedSearchType.inline,
				"[\"" + PersistentBean.TAGGED_NAME + "\",\"" + PersistentBean.FLAG_COMMENT_NAME
						+ "\",\"status\",{name:\"amount\",width:120},{name:\"description\"}]",
				"({sortSpecifiers:[{property:\"name\",direction:\"descending\"},{property:\"created\",direction:\"ascending\"}]})",
				"([{fieldName:\"category\"}])",
				AggregateFunction.Sum,
				criteria));

		assertNotNull(result);
		assertParsedDisplayState(result);
		assertEquals(criteria, result.getSourceSmartClientCriteria());
		assertParsedSimpleMapFilter(result.getFilter());
	}

	private static void assertParsedDisplayState(Snapshot result) {
		assertEquals(AdvancedSearchType.inline, result.getAdvanced());
		assertEquals(Integer.valueOf(120), result.getColumns().get("amount"));
		assertTrue(result.getColumns().containsKey("status"));
		assertNull(result.getColumns().get("status"));
		assertTrue(result.getColumns().containsKey("description"));
		assertNull(result.getColumns().get(PersistentBean.TAGGED_NAME));
		assertNull(result.getColumns().get(PersistentBean.FLAG_COMMENT_NAME));
		assertEquals(SortDirection.descending, result.getSorts().get("name"));
		assertEquals(SortDirection.ascending, result.getSorts().get("created"));
		assertEquals("category", result.getGroup());
		assertEquals(AggregateFunction.Sum, result.getSummary());
	}

	private static void assertParsedSimpleMapFilter(SnapshotFilter resultFilter) {
		SnapshotCriteria filter = assertInstanceOf(SnapshotCriteria.class, resultFilter);
		assertEquals(3, filter.getFilters().size());
		SnapshotCriteria status = assertInstanceOf(SnapshotCriteria.class, filter.getFilters().get(0));
		assertEquals(CompoundFilterOperator.or, status.getOperator());
		assertEquals(2, status.getFilters().size());
		SnapshotCriterion owner = assertInstanceOf(SnapshotCriterion.class, filter.getFilters().get(1));
		assertEquals("owner", owner.getColumn());
		assertEquals(SmartClientFilterOperator.equals, owner.getOperator());
		assertEquals(List.of("mike"), owner.getValue());
		SnapshotCriterion description = assertInstanceOf(SnapshotCriterion.class, filter.getFilters().get(2));
		assertEquals("description", description.getColumn());
		assertEquals(SmartClientFilterOperator.iContains, description.getOperator());
		assertEquals("alpha", description.getValue());
	}

	@Test
	void fromClientPayloadParsesAdvancedCriteriaAndSkipsEmptyChildren() throws Exception {
		bindMockUser();

		Map<String, Object> nameCriterion = new LinkedHashMap<>();
		nameCriterion.put("fieldName", "name");
		nameCriterion.put("operator", "iStartsWith");
		nameCriterion.put("value", "Jo");

		Map<String, Object> dateCriterion = new LinkedHashMap<>();
		dateCriterion.put("fieldName", "createdDate");
		dateCriterion.put("operator", "betweenInclusive");
		dateCriterion.put("start", "2024-01-01");
		dateCriterion.put("end", "2024-12-31");

		Map<String, Object> criteria = new LinkedHashMap<>();
		criteria.put("_constructor", "AdvancedCriteria");
		criteria.put("operator", "and");
		criteria.put("criteria", Arrays.asList(new LinkedHashMap<>(), nameCriterion, dateCriterion));

		Snapshot result = SnapshotAdapter.SMART_CLIENT.fromClientPayload(clientPayload(null,
				"[\"name\"]",
				null,
				"",
				null,
				criteria));

		assertNotNull(result);
		assertNull(result.getAdvanced());
		assertNull(result.getGroup());
		assertNull(result.getSummary());
		SnapshotCriteria filter = assertInstanceOf(SnapshotCriteria.class, result.getFilter());
		assertEquals(CompoundFilterOperator.and, filter.getOperator());
		assertEquals(2, filter.getFilters().size());
		SnapshotCriterion name = assertInstanceOf(SnapshotCriterion.class, filter.getFilters().get(0));
		assertEquals("name", name.getColumn());
		assertEquals(SmartClientFilterOperator.iStartsWith, name.getOperator());
		assertEquals("Jo", name.getValue());
		SnapshotCriterion date = assertInstanceOf(SnapshotCriterion.class, filter.getFilters().get(1));
		assertEquals("createdDate", date.getColumn());
		assertEquals(SmartClientFilterOperator.betweenInclusive, date.getOperator());
		assertEquals("2024-01-01", date.getStart());
		assertEquals("2024-12-31", date.getEnd());
	}

	@Test
	void fromClientPayloadReturnsNullForMalformedStateSections() throws Exception {
		bindMockUser();

		List<Map<String, Object>> malformedPayloads = Arrays.asList(rawPayload(
				"fieldState",
				"[{width:100}]"),
				rawPayload("fieldState", "[42]"),
				rawPayload("fieldState", "{}"),
				rawPayload("sortState", "({sortSpecifiers:[{direction:\"ascending\"}]})"),
				rawPayload("sortState", "({sortSpecifiers:{property:\"name\"}})"),
				rawPayload("sortState", "({bad:[{}]})"),
				rawPayload("sortState", Integer.valueOf(42)),
				rawPayload("groupState", "([{missing:\"category\"}])"),
				rawPayload("groupState", "{}"),
				rawPayload("summaryType", Integer.valueOf(42)),
				rawPayload("criteria", null),
				rawPayload("criteria", "notAMap"));

		for (Map<String, Object> payload : malformedPayloads) {
			assertNull(SnapshotAdapter.SMART_CLIENT.fromClientPayload(JSON.marshall(payload)));
		}
	}

	private static String clientPayload(AdvancedSearchType advanced,
										String fieldState,
										String sortState,
										String groupState,
										AggregateFunction summaryType,
										Map<String, Object> criteria) {
		Map<String, Object> payload = new LinkedHashMap<>();
		payload.put("advancedCriteriaStyle", advanced);
		payload.put("fieldState", fieldState);
		payload.put("sortState", sortState);
		payload.put("groupState", groupState);
		payload.put("summaryType", summaryType == null ? "" : summaryType.toString());
		payload.put("criteria", criteria);
		return JSON.marshall(payload);
	}

	private static Map<String, Object> rawPayload(String key, Object value) {
		Map<String, Object> payload = new LinkedHashMap<>();
		payload.put("advancedCriteriaStyle", null);
		payload.put("fieldState", "[\"name\"]");
		payload.put("sortState", null);
		payload.put("groupState", "");
		payload.put("summaryType", "");
		payload.put("criteria", new LinkedHashMap<>());
		payload.put(key, value);
		return payload;
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

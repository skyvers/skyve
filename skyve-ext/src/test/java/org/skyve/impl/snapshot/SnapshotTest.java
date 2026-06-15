package org.skyve.impl.snapshot;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

@SuppressWarnings("static-method")
public class SnapshotTest {

	// ---- getters / setters ----

	@Test
	public void testDefaultStateIsNull() {
		Snapshot s = new Snapshot();
		assertNull(s.getAdvanced());
		assertNull(s.getSummary());
		assertNotNull(s.getSorts());
		assertTrue(s.getSorts().isEmpty());
		assertNull(s.getGroup());
		assertNotNull(s.getColumns());
		assertTrue(s.getColumns().isEmpty());
		assertNull(s.getFilter());
	}

	@Test
	public void testSetAndGetAdvanced() {
		Snapshot s = new Snapshot();
		s.setAdvanced(Snapshot.AdvancedSearchType.inline);
		assertEquals(Snapshot.AdvancedSearchType.inline, s.getAdvanced());
	}

	@Test
	public void testSetAndGetSummary() {
		Snapshot s = new Snapshot();
		s.setSummary(AggregateFunction.Count);
		assertEquals(AggregateFunction.Count, s.getSummary());
	}

	@Test
	public void testSetAndGetGroup() {
		Snapshot s = new Snapshot();
		s.setGroup("category");
		assertEquals("category", s.getGroup());
	}

	@Test
	public void testPutSortDefaultAscending() {
		Snapshot s = new Snapshot();
		s.putSort("name");
		assertEquals(SortDirection.ascending, s.getSorts().get("name"));
	}

	@Test
	public void testPutSortDescending() {
		Snapshot s = new Snapshot();
		s.putSort("date", SortDirection.descending);
		assertEquals(SortDirection.descending, s.getSorts().get("date"));
	}

	@Test
	public void testPutColumnWithoutWidth() {
		Snapshot s = new Snapshot();
		s.putColumn("status");
		assertTrue(s.getColumns().containsKey("status"));
		assertNull(s.getColumns().get("status"));
	}

	@Test
	public void testPutColumnWithWidth() {
		Snapshot s = new Snapshot();
		s.putColumn("name", 150);
		assertEquals(Integer.valueOf(150), s.getColumns().get("name"));
	}

	@Test
	public void testPutColumnWithIntegerWidth() {
		Snapshot s = new Snapshot();
		s.putColumn("code", Integer.valueOf(80));
		assertEquals(Integer.valueOf(80), s.getColumns().get("code"));
	}

	@Test
	public void testSetFilter() {
		Snapshot s = new Snapshot();
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("name");
		c.setOperator(SmartClientFilterOperator.iContains);
		c.setValue("test");
		s.setFilter(c);
		assertEquals(c, s.getFilter());
	}

	// ---- JSON round-trip ----

	@Test
	public void testToJSONProducesNonEmptyString() {
		Snapshot s = new Snapshot();
		s.putSort("name");
		s.putColumn("name", 100);
		String json = s.toJSON();
		assertNotNull(json);
		assertTrue(!json.isEmpty());
	}

	@Test
	public void testToJSONRoundtripSorts() throws Exception {
		Snapshot s = new Snapshot();
		s.putSort("name", SortDirection.ascending);
		s.putSort("date", SortDirection.descending);

		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
		assertEquals(SortDirection.ascending, restored.getSorts().get("name"));
		assertEquals(SortDirection.descending, restored.getSorts().get("date"));
	}

	@Test
	public void testToJSONRoundtripColumns() throws Exception {
		Snapshot s = new Snapshot();
		s.putColumn("name", 200);
		s.putColumn("status");

		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
		assertTrue(restored.getColumns().containsKey("name"));
	}

	@Test
	public void testToJSONRoundtripGroup() throws Exception {
		Snapshot s = new Snapshot();
		s.setGroup("category");

		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
		assertEquals("category", restored.getGroup());
	}

	@Test
	public void testToJSONRoundtripAdvanced() throws Exception {
		Snapshot s = new Snapshot();
		s.setAdvanced(Snapshot.AdvancedSearchType.radio);

		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
		assertEquals(Snapshot.AdvancedSearchType.radio, restored.getAdvanced());
	}

	@Test
	public void testToJSONEmptySnapshotRoundtrip() throws Exception {
		Snapshot s = new Snapshot();
		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
	}

	@Test
	public void testFromJSONReturnsNullForNullInput() throws Exception {
		// JSON.unmarshall("null") returns null as the value
		Snapshot result = Snapshot.fromJSON("null");
		assertNull(result);
	}

        @Test
        public void testToJSONRoundtripSummary() throws Exception {
                Snapshot s = new Snapshot();
                s.setSummary(AggregateFunction.Sum);

                String json = s.toJSON();
                Snapshot restored = Snapshot.fromJSON(json);
                assertNotNull(restored);
                assertEquals(AggregateFunction.Sum, restored.getSummary());
        }

	@Test
	public void testFromJSONRoundtripWithFilter() throws Exception {
		Snapshot s = new Snapshot();
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("status");
		c.setOperator(SmartClientFilterOperator.equals);
		c.setValue("active");
		s.setFilter(c);

		String json = s.toJSON();
		Snapshot restored = Snapshot.fromJSON(json);
		assertNotNull(restored);
		assertNotNull(restored.getFilter());
		assertTrue(restored.getFilter() instanceof SnapshotCriterion);
		SnapshotCriterion restoredCriterion = (SnapshotCriterion) restored.getFilter();
		assertNotNull(restoredCriterion);
		assertEquals("status", restoredCriterion.getColumn());
	}
}

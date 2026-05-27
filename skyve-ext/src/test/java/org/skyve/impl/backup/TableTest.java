package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

@SuppressWarnings("static-method")
public class TableTest {

	@Test
	public void testConstructorSetsIdentifiers() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals("myTable", t.agnosticIdentifier);
		assertEquals("schema.myTable", t.persistentIdentifier);
	}

	@Test
	public void testEqualsMatchesByAgnosticIdentifier() {
		Table t1 = new Table("myTable", "schema.myTable");
		Table t2 = new Table("myTable", "other.myTable");
		assertEquals(t1, t2);
	}

	@Test
	public void testEqualsReturnsFalseForDifferentIdentifier() {
		Table t1 = new Table("myTable", "schema.myTable");
		Table t2 = new Table("otherTable", "schema.otherTable");
		assertNotEquals(t1, t2);
	}

	@Test
	public void testEqualsReturnsFalseForNull() {
		Table t = new Table("myTable", "schema.myTable");
		assertNotEquals(null, t);
	}

	@Test
	public void testEqualsReturnsFalseForNonTableObject() {
		Table t = new Table("myTable", "schema.myTable");
		assertNotEquals("myTable", t);
	}

	@Test
	public void testEqualsReturnsTrueForSameInstance() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals(t, t);
	}

	@Test
	public void testHashCodeBasedOnAgnosticIdentifier() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals("myTable".hashCode(), t.hashCode());
	}

	@Test
	public void testToJSONProducesValidJson() throws Exception {
		Table t = new Table("myTable", "schema.myTable");
		t.fields.put("bizId", new BackupField(AttributeType.text, Sensitivity.none));
		t.fields.put("bizVersion", new BackupField(AttributeType.integer, Sensitivity.none));
		String json = t.toJSON();
		assertNotNull(json);
		assertTrue(json.contains("myTable"));
		assertTrue(json.contains("bizId"));
	}

	@Test
	public void testFromJSONRoundTripsSimpleTable() throws Exception {
		Table original = new Table("myTable", "myTable");
		original.fields.put("bizId", new BackupField(AttributeType.text, Sensitivity.none));
		original.fields.put("bizVersion", new BackupField(AttributeType.integer, Sensitivity.none));
		String json = original.toJSON();
		Table restored = Table.fromJSON(json);
		assertNotNull(restored);
		assertEquals("myTable", restored.agnosticIdentifier);
		assertTrue(restored.fields.containsKey("bizId"));
		assertTrue(restored.fields.containsKey("bizVersion"));
	}

	@Test
	public void testFromJSONRoundTripsJoinTable() throws Exception {
		JoinTable join = new JoinTable("myJoin", "myJoin", "ownerTable", "ownerTable", true);
		join.fields.put("owner_id", Table.ASSOCIATION);
		String json = join.toJSON();
		assertTrue(json.contains("ownerTable"));
		Table restored = Table.fromJSON(json);
		assertNotNull(restored);
		assertTrue(restored instanceof JoinTable);
		assertEquals("myJoin", restored.agnosticIdentifier);
		JoinTable restoredJoin = (JoinTable) restored;
		assertEquals("ownerTable", restoredJoin.ownerAgnosticIdentifier);
		assertTrue(restoredJoin.ordered);
	}

	@Test
	public void testToJSONWithEmptyFieldsDoesNotThrow() throws Exception {
		Table t = new Table("emptyTable", "emptyTable");
		String json = t.toJSON();
		assertNotNull(json);
	}

}

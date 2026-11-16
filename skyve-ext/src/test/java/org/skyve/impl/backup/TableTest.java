package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings({"static-method", "boxing"})
class TableTest {

	@Test
	void testConstructorSetsIdentifiers() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals("myTable", t.agnosticIdentifier);
		assertEquals("schema.myTable", t.persistentIdentifier);
	}

	@Test
	void testEqualsMatchesByAgnosticIdentifier() {
		Table t1 = new Table("myTable", "schema.myTable");
		Table t2 = new Table("myTable", "other.myTable");
		assertEquals(t1, t2);
	}

	@Test
	void testEqualsReturnsFalseForDifferentIdentifier() {
		Table t1 = new Table("myTable", "schema.myTable");
		Table t2 = new Table("otherTable", "schema.otherTable");
		assertNotEquals(t1, t2);
	}

	@Test
	void testEqualsReturnsFalseForNull() {
		Table t = new Table("myTable", "schema.myTable");
		assertNotEquals(null, t);
	}

	@Test
	void testEqualsReturnsFalseForNonTableObject() {
		Table t = new Table("myTable", "schema.myTable");
		assertNotEquals("myTable", t);
	}

	@Test
	void testEqualsReturnsTrueForSameInstance() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals(t, t);
	}

	@Test
	void testHashCodeBasedOnAgnosticIdentifier() {
		Table t = new Table("myTable", "schema.myTable");
		assertEquals("myTable".hashCode(), t.hashCode());
	}

	@Test
	void testToJSONProducesValidJson() throws Exception {
		Table t = new Table("myTable", "schema.myTable");
		t.fields.put("bizId", new BackupField(AttributeType.text, Sensitivity.none));
		t.fields.put("bizVersion", new BackupField(AttributeType.integer, Sensitivity.none));
		String json = t.toJSON();
		assertNotNull(json);
		assertTrue(json.contains("myTable"));
		assertTrue(json.contains("bizId"));
	}

	@Test
	void testFromJSONRoundTripsSimpleTable() throws Exception {
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
	void testFromJSONRoundTripsJoinTable() throws Exception {
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
	void testToJSONWithEmptyFieldsDoesNotThrow() throws Exception {
		Table t = new Table("emptyTable", "emptyTable");
		String json = t.toJSON();
		assertNotNull(json);
	}

	@Test
	void addFieldsFromDocumentAddsSystemScalarEmbeddedAndAssociationFields() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = document("admin", "Invoice");
		Document embeddedDocument = document("admin", "Address");
		Document referencedDocument = document("admin", "Customer");
		Text code = text("code", AttributeType.text, Sensitivity.none, 20, IndexType.database, false);
		Text secretCode = text("code", AttributeType.text, Sensitivity.confidential, 20, null, false);
		Text dynamicField = text("dynamicValue", AttributeType.text, Sensitivity.none, 10, null, true);
		Text embeddedLine1 = text("line1", AttributeType.text, Sensitivity.none, 50, null, false);
		Association embeddedAddress = association("address", AssociationType.embedded, "addr", "Address");
		Association customerAssociation = association("customer", AssociationType.aggregation, null, "Customer");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Address")).thenReturn(embeddedDocument);
		when(module.getDocument(customer, "Customer")).thenReturn(referencedDocument);
		doReturn(List.<Attribute> of(code, secretCode, dynamicField, embeddedAddress, customerAssociation)).when(document).getAllAttributes(customer);
		doReturn(List.<Attribute> of(embeddedLine1)).when(embeddedDocument).getAllAttributes(customer);
		when(referencedDocument.isDynamic()).thenReturn(false);
		when(embeddedDocument.isDynamic()).thenReturn(false);
		Table table = new Table("Invoice", "ADM_Invoice");

		table.addFieldsFromDocument(customer, document);

		assertTrue(table.fields.containsKey(Bean.DOCUMENT_ID));
		assertTrue(table.fields.containsKey(PersistentBean.VERSION_NAME));
		assertTrue(table.fields.containsKey(PersistentBean.LOCK_NAME));
		assertTrue(table.fields.containsKey(Bean.BIZ_KEY));
		assertTrue(table.fields.containsKey(Bean.CUSTOMER_NAME));
		assertTrue(table.fields.containsKey("code"));
		assertTrue(table.fields.containsKey("addr_line1"));
		assertTrue(table.fields.containsKey("customer_id"));
		assertFalse(table.fields.containsKey("dynamicValue"));
		assertEquals(Sensitivity.confidential, table.fields.get("code").getSensitivity());
		assertEquals(AttributeType.text, table.fields.get("addr_line1").getAttributeType());
		assertEquals(AttributeType.association, table.fields.get("customer_id").getAttributeType());
		assertEquals(IndexType.database, table.indexes.get("code"));
		assertEquals(Integer.valueOf(20), ((BackupLengthField) table.fields.get("code")).getMaxLength());
	}

	@Test
	void addFieldsFromDocumentAddsHierarchyAndChildParentFields() {
		Customer customer = mock(Customer.class);
		Document hierarchy = document("admin", "Node");
		when(hierarchy.getParentDocumentName()).thenReturn("Node");
		when(hierarchy.isOrdered()).thenReturn(true);
		when(hierarchy.getAllAttributes(customer)).thenReturn(List.of());
		Table hierarchyTable = new Table("Node", "ADM_Node");

		hierarchyTable.addFieldsFromDocument(customer, hierarchy);

		assertTrue(hierarchyTable.fields.containsKey(HierarchicalBean.PARENT_ID));
		assertTrue(hierarchyTable.fields.containsKey(Bean.ORDINAL_NAME));

		Document child = document("admin", "Line");
		when(child.getParentDocumentName()).thenReturn("Invoice");
		when(child.isOrdered()).thenReturn(true);
		when(child.getAllAttributes(customer)).thenReturn(List.of());
		Table childTable = new Table("Line", "ADM_Line");

		childTable.addFieldsFromDocument(customer, child);

		assertTrue(childTable.fields.containsKey(ChildBean.PARENT_NAME + "_id"));
		assertTrue(childTable.fields.containsKey(Bean.ORDINAL_NAME));
	}

	@Test
	void addFieldsFromDocumentUpgradesAnExistingFieldWithoutSensitivity() {
		Customer customer = mock(Customer.class);
		Document document = document("admin", "Invoice");
		Text code = text("code", AttributeType.text, Sensitivity.personal, 20, null, false);
		doReturn(List.<Attribute> of(code)).when(document).getAllAttributes(customer);
		Table table = new Table("Invoice", "ADM_Invoice");
		table.fields.put("code", new BackupField(AttributeType.text, null));

		table.addFieldsFromDocument(customer, document);

		assertEquals(Sensitivity.personal, table.fields.get("code").getSensitivity());
	}

	@Test
	void fromJSONThrowsWhenTheJsonDefinesNoTable() {
		assertThrows(IllegalArgumentException.class, () -> Table.fromJSON("null"));
	}

	@Test
	void fromJSONQualifiesUnqualifiedTableAndOwnerNamesWithTheConfiguredSchema() throws Exception {
		JoinTable join = new JoinTable("myJoin", "myJoin", "ownerTable", "ownerTable", false);
		join.fields.put("owner_id", Table.ASSOCIATION);
		String json = join.toJSON();
		String previousCatalog = UtilImpl.CATALOG;
		String previousSchema = UtilImpl.SCHEMA;
		try {
			UtilImpl.CATALOG = null;
			UtilImpl.SCHEMA = "app";

			JoinTable restored = (JoinTable) Table.fromJSON(json);

			assertEquals("myJoin", restored.agnosticIdentifier);
			assertEquals("app.myJoin", restored.persistentIdentifier);
			assertEquals("ownerTable", restored.ownerAgnosticIdentifier);
			assertEquals("app.ownerTable", restored.ownerPersistentIdentifier);
		}
		finally {
			UtilImpl.CATALOG = previousCatalog;
			UtilImpl.SCHEMA = previousSchema;
		}
	}

	@Test
	void qualifyThrowsWhenThereIsNoIdentifierToQualify() throws Exception {
		java.lang.reflect.Method qualify = Table.class.getDeclaredMethod("qualify", String.class);
		qualify.setAccessible(true);

		java.lang.reflect.InvocationTargetException e = assertThrows(java.lang.reflect.InvocationTargetException.class,
																			() -> qualify.invoke(null, (Object) null));

		assertTrue(e.getCause() instanceof IllegalStateException);
		assertEquals("Cannot qualify table identifier null", e.getCause().getMessage());
	}

	private static Document document(String moduleName, String name) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn(moduleName);
		when(document.getName()).thenReturn(name);
		when(document.getPersistent()).thenReturn(null);
		when(document.getBizKeySensitity()).thenReturn(Sensitivity.none);
		return document;
	}

	private static Text text(String name,
								AttributeType attributeType,
								Sensitivity sensitivity,
								int length,
								IndexType index,
								boolean dynamic) {
		Text text = new Text();
		text.setName(name);
		text.setAttributeType(attributeType);
		text.setSensitivity(sensitivity);
		text.setLength(length);
		text.setIndex(index);
		text.setDynamic(dynamic);
		return text;
	}

	private static Association association(String name, AssociationType type, String embeddedColumnsPrefix, String documentName) {
		Association association = mock(Association.class);
		when(association.isPersistent()).thenReturn(true);
		when(association.getName()).thenReturn(name);
		when(association.getAttributeType()).thenReturn(AttributeType.association);
		when(association.getSensitivity()).thenReturn(Sensitivity.none);
		when(association.getType()).thenReturn(type);
		when(association.getEmbeddedColumnsPrefix()).thenReturn(embeddedColumnsPrefix);
		when(association.getDocumentName()).thenReturn(documentName);
		return association;
	}
}

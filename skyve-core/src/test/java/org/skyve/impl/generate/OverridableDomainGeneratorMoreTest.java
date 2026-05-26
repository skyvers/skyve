package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * Additional unit tests for {@link OverridableDomainGenerator} private/package
 * methods that are not already covered by {@link OverridableDomainGeneratorTest}.
 */
@SuppressWarnings("static-method")
class OverridableDomainGeneratorMoreTest {

	// ----- helpers ----------------------------------------------------------

	private static OverridableDomainGenerator generator() {
		return new OverridableDomainGenerator(false, false, false,
				DialectOptions.H2_NO_INDEXES, "", "", "", "", null);
	}

	private static OverridableDomainGenerator generator(DialectOptions opts) {
		return new OverridableDomainGenerator(false, false, false,
				opts, "", "", "", "", null);
	}

	private static Method declaredMethod(String name, Class<?>... params) throws Exception {
		Method m = OverridableDomainGenerator.class.getDeclaredMethod(name, params);
		m.setAccessible(true);
		return m;
	}

	// ----- columnName -------------------------------------------------------

	@Test
	void columnNameWithNoPrefixOrSuffix() throws Exception {
		Set<String> columnNames = new HashSet<>();
		String result = (String) declaredMethod("columnName",
				String.class, String.class, String.class, String.class, String.class, Set.class)
				.invoke(null, "mod", "Doc", "attr", null, null, columnNames);
		assertEquals("attr", result);
		assertTrue(columnNames.contains("attr"));
	}

	@Test
	void columnNameWithPrefix() throws Exception {
		Set<String> columnNames = new HashSet<>();
		String result = (String) declaredMethod("columnName",
				String.class, String.class, String.class, String.class, String.class, Set.class)
				.invoke(null, "mod", "Doc", "attr", "prefix", null, columnNames);
		assertEquals("prefix_attr", result);
	}

	@Test
	void columnNameWithSuffix() throws Exception {
		Set<String> columnNames = new HashSet<>();
		String result = (String) declaredMethod("columnName",
				String.class, String.class, String.class, String.class, String.class, Set.class)
				.invoke(null, "mod", "Doc", "attr", null, "_id", columnNames);
		assertEquals("attr_id", result);
	}

	@Test
	void columnNameWithPrefixAndSuffix() throws Exception {
		Set<String> columnNames = new HashSet<>();
		String result = (String) declaredMethod("columnName",
				String.class, String.class, String.class, String.class, String.class, Set.class)
				.invoke(null, "mod", "Doc", "attr", "pfx", "_id", columnNames);
		assertEquals("pfx_attr_id", result);
	}

	@Test
	void columnNameThrowsOnDuplicate() throws Exception {
		Set<String> columnNames = new HashSet<>();
		Method m = declaredMethod("columnName",
				String.class, String.class, String.class, String.class, String.class, Set.class);
		m.invoke(null, "mod", "Doc", "attr", null, null, columnNames);
		// Second call with same column name must throw MetaDataException wrapped in InvocationTargetException
		java.lang.reflect.InvocationTargetException ex = assertThrows(
				java.lang.reflect.InvocationTargetException.class,
				() -> m.invoke(null, "mod", "Doc", "attr", null, null, columnNames));
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	// ----- identifierIsTooLong ----------------------------------------------

	@Test
	void identifierIsNeverTooLongForH2NoIndexes() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.H2_NO_INDEXES);
		Method m = declaredMethod("identifierIsTooLong", String.class);
		// H2_NO_INDEXES has limit 0 (no limit)
		assertNotEquals(Boolean.TRUE, m.invoke(gen, "a".repeat(200)));
	}

	@Test
	void identifierIsNotTooLongWhenUnderLimit() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5);
		Method m = declaredMethod("identifierIsTooLong", String.class);
		// MYSQL_5 limit is 64
		assertNotEquals(Boolean.TRUE, m.invoke(gen, "a".repeat(64)));
	}

	@Test
	void identifierIsTooLongWhenOverLimit() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5);
		Method m = declaredMethod("identifierIsTooLong", String.class);
		// MYSQL_5 limit is 64
		assertEquals(Boolean.TRUE, m.invoke(gen, "a".repeat(65)));
	}

	// ----- generateDataStoreName --------------------------------------------

	private static Class<?> dataStoreTypeClass() throws Exception {
		for (Class<?> c : OverridableDomainGenerator.class.getDeclaredClasses()) {
			if ("DataStoreType".equals(c.getSimpleName())) {
				return c;
			}
		}
		throw new IllegalStateException("DataStoreType inner enum not found");
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	private static Object dataStoreType(String name) throws Exception {
		return Enum.valueOf((Class<? extends Enum>) dataStoreTypeClass().asSubclass(Enum.class), name);
	}

	@Test
	void generateDataStoreNameForFkIncludesTableNameInH2() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.H2_NO_INDEXES);
		// H2_NO_INDEXES: isDataStoreIndexNamesInGlobalNamespace = false; FK always includes table
		Object fk = dataStoreType("FK");
		Method m = declaredMethod("generateDataStoreName", dataStoreTypeClass(), String.class, String.class);
		String result = (String) m.invoke(gen, fk, "MY_TABLE", "col_id");
		assertEquals("FK_MY_TABLE_col_id", result);
	}

	@Test
	void generateDataStoreNameForIdxExcludesTableNameInH2() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.H2_NO_INDEXES);
		// H2_NO_INDEXES: isDataStoreIndexNamesInGlobalNamespace = false; IDX does not include table
		Object idx = dataStoreType("IDX");
		Method m = declaredMethod("generateDataStoreName", dataStoreTypeClass(), String.class, String.class);
		String result = (String) m.invoke(gen, idx, "MY_TABLE", "col");
		assertEquals("IDX_col", result);
	}

	@Test
	void generateDataStoreNameForIdxIncludesTableNameInPostgresql() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.POSTGRESQL);
		// POSTGRESQL: isDataStoreIndexNamesInGlobalNamespace = true; all types include table
		Object idx = dataStoreType("IDX");
		Method m = declaredMethod("generateDataStoreName", dataStoreTypeClass(), String.class, String.class);
		String result = (String) m.invoke(gen, idx, "MY_TABLE", "col");
		assertEquals("IDX_MY_TABLE_col", result);
	}

	@Test
	void generateDataStoreNameHashesLongNameInMysql() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5);
		// MYSQL_5 limit is 64
		Object fk = dataStoreType("FK");
		Method m = declaredMethod("generateDataStoreName", dataStoreTypeClass(), String.class, String.class);
		// Create a name that will exceed 64 chars: "FK_" + 30 + "_" + 40 = 75 chars
		String result = (String) m.invoke(gen, fk, "a".repeat(30), "b".repeat(40));
		assertNotNull(result);
		assertTrue(result.startsWith("FK_"), "Hashed name should still start with type prefix");
		assertTrue(result.length() <= 64, "Hashed name should be within limit");
	}

	// ----- shouldIndex ------------------------------------------------------

	@Test
	void shouldIndexReturnsFalseForNullWithH2NoIndexes() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.H2_NO_INDEXES);
		Method m = declaredMethod("shouldIndex", Boolean.class);
		assertNotEquals(Boolean.TRUE, m.invoke(gen, (Boolean) null));
	}

	@Test
	void shouldIndexReturnsTrueForNullWithMysql() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5);
		Method m = declaredMethod("shouldIndex", Boolean.class);
		assertEquals(Boolean.TRUE, m.invoke(gen, (Boolean) null));
	}

	@Test
	void shouldIndexReturnsTrueForTrue() throws Exception {
		OverridableDomainGenerator gen = generator();
		Method m = declaredMethod("shouldIndex", Boolean.class);
		assertEquals(Boolean.TRUE, m.invoke(gen, Boolean.TRUE));
	}

	@Test
	void shouldIndexReturnsFalseForFalse() throws Exception {
		OverridableDomainGenerator gen = generator();
		Method m = declaredMethod("shouldIndex", Boolean.class);
		assertNotEquals(Boolean.TRUE, m.invoke(gen, Boolean.FALSE));
	}

	// ----- attributeJavadoc -------------------------------------------------

	@Test
	void attributeJavadocWithDescriptionAndDocumentation() throws Exception {
		Text attr = new Text();
		attr.setName("myField");
		attr.setDisplayName("My Field");
		attr.setDescription("Field description");
		attr.setDocumentation("Field documentation");

		StringBuilder sb = new StringBuilder();
		declaredMethod("attributeJavadoc", Attribute.class, StringBuilder.class)
				.invoke(null, attr, sb);

		String result = sb.toString();
		assertTrue(result.contains("My Field"));
		assertTrue(result.contains("Field description"));
		assertTrue(result.contains("Field documentation"));
		assertTrue(result.startsWith("\t/**\n"));
		assertTrue(result.endsWith("\t **/\n"));
	}

	@Test
	void attributeJavadocWithNullDescriptionAndDocumentation() throws Exception {
		Text attr = new Text();
		attr.setName("simpleField");
		attr.setDisplayName("Simple Field");
		// description and documentation left null

		StringBuilder sb = new StringBuilder();
		declaredMethod("attributeJavadoc", Attribute.class, StringBuilder.class)
				.invoke(null, attr, sb);

		String result = sb.toString();
		assertTrue(result.contains("Simple Field"));
		assertFalse(result.contains("<br/>"));
	}

	// ----- accessorJavadoc --------------------------------------------------

	@Test
	void accessorJavadocMapped() throws Exception {
		Text attr = new Text();
		attr.setName("myField");
		attr.setDisplayName("My Field");

		StringBuilder sb = new StringBuilder();
		declaredMethod("accessorJavadoc", Attribute.class, StringBuilder.class, boolean.class)
				.invoke(null, attr, sb, Boolean.TRUE);

		String result = sb.toString();
		assertTrue(result.contains("myField"));
		assertTrue(result.contains("bizId"));
		assertTrue(result.contains("@return"));
	}

	@Test
	void accessorJavadocNotMapped() throws Exception {
		Text attr = new Text();
		attr.setName("myField");
		attr.setDisplayName("My Field");

		StringBuilder sb = new StringBuilder();
		declaredMethod("accessorJavadoc", Attribute.class, StringBuilder.class, boolean.class)
				.invoke(null, attr, sb, Boolean.FALSE);

		String result = sb.toString();
		assertTrue(result.contains("myField"));
		assertFalse(result.contains("bizId"));
		assertTrue(result.contains("@return"));
	}

	// ----- mutatorJavadoc ---------------------------------------------------

	@Test
	void mutatorJavadocMapped() throws Exception {
		Text attr = new Text();
		attr.setName("status");
		attr.setDisplayName("Status");

		StringBuilder sb = new StringBuilder();
		declaredMethod("mutatorJavadoc", Attribute.class, StringBuilder.class, boolean.class)
				.invoke(null, attr, sb, Boolean.TRUE);

		String result = sb.toString();
		assertTrue(result.contains("status"));
		assertTrue(result.contains("bizId"));
		assertTrue(result.contains("element"));
	}

	@Test
	void mutatorJavadocNotMapped() throws Exception {
		Text attr = new Text();
		attr.setName("status");
		attr.setDisplayName("Status");

		StringBuilder sb = new StringBuilder();
		declaredMethod("mutatorJavadoc", Attribute.class, StringBuilder.class, boolean.class)
				.invoke(null, attr, sb, Boolean.FALSE);

		String result = sb.toString();
		assertTrue(result.contains("status"));
		assertFalse(result.contains("bizId"));
		assertTrue(result.contains("@param status"));
	}

	// ----- collectionJavadoc ------------------------------------------------

	@Test
	void collectionJavadocAddIndexed() throws Exception {
		StringBuilder sb = new StringBuilder();
		declaredMethod("collectionJavadoc", String.class, StringBuilder.class, boolean.class, boolean.class)
				.invoke(null, "items", sb, Boolean.TRUE, Boolean.TRUE);

		String result = sb.toString();
		assertTrue(result.contains("items"));
		assertTrue(result.contains("add"));
		assertTrue(result.contains("index"));
		assertTrue(result.contains("element"));
	}

	@Test
	void collectionJavadocAddNotIndexed() throws Exception {
		StringBuilder sb = new StringBuilder();
		declaredMethod("collectionJavadoc", String.class, StringBuilder.class, boolean.class, boolean.class)
				.invoke(null, "items", sb, Boolean.TRUE, Boolean.FALSE);

		String result = sb.toString();
		assertTrue(result.contains("items"));
		assertTrue(result.contains("add"));
		assertFalse(result.contains("index"));
		assertTrue(result.contains("element"));
	}

	@Test
	void collectionJavadocRemoveIndexed() throws Exception {
		StringBuilder sb = new StringBuilder();
		declaredMethod("collectionJavadoc", String.class, StringBuilder.class, boolean.class, boolean.class)
				.invoke(null, "items", sb, Boolean.FALSE, Boolean.TRUE);

		String result = sb.toString();
		assertTrue(result.contains("items"));
		assertTrue(result.contains("remove"));
		assertTrue(result.contains("index"));
	}

	@Test
	void collectionJavadocRemoveNotIndexed() throws Exception {
		StringBuilder sb = new StringBuilder();
		declaredMethod("collectionJavadoc", String.class, StringBuilder.class, boolean.class, boolean.class)
				.invoke(null, "items", sb, Boolean.FALSE, Boolean.FALSE);

		String result = sb.toString();
		assertTrue(result.contains("items"));
		assertTrue(result.contains("remove"));
		assertTrue(result.contains("element"));
	}

	// ----- generateDocumentPropertyNames ------------------------------------

	@Test
	void generateDocumentPropertyNamesWithTextAttributeAndCondition() throws Exception {
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TestDoc");
		doc.setOwningModuleName("testMod");

		Text textField = new Text();
		textField.setName("description");
		doc.putAttribute(textField);

		// Add a condition directly via getConditions().put()
		org.skyve.impl.metadata.model.document.ConditionImpl cond = new org.skyve.impl.metadata.model.document.ConditionImpl();
		cond.setExpression("true");
		doc.getConditions().put("active", cond);

		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, Attribute.AttributeType> result =
				(java.util.TreeMap<String, Attribute.AttributeType>) declaredMethod(
						"generateDocumentPropertyNames", Document.class)
						.invoke(null, doc);

		assertTrue(result.containsKey("description"));
		assertEquals(Attribute.AttributeType.text, result.get("description"));
		assertTrue(result.containsKey("active"));
		assertEquals(Attribute.AttributeType.bool, result.get("active"));
	}

	@Test
	void generateDocumentPropertyNamesSkipsBizKey() throws Exception {
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TestDoc");
		doc.setOwningModuleName("testMod");

		Text bizKeyField = new Text();
		bizKeyField.setName(org.skyve.domain.Bean.BIZ_KEY);
		doc.putAttribute(bizKeyField);

		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, Attribute.AttributeType> result =
				(java.util.TreeMap<String, Attribute.AttributeType>) declaredMethod(
						"generateDocumentPropertyNames", Document.class)
						.invoke(null, doc);

		assertFalse(result.containsKey(org.skyve.domain.Bean.BIZ_KEY));
	}

	// ----- testPolymorphic --------------------------------------------------

	@Test
	void testPolymorphicReturnsFalseWhenPersistentIsNull() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		when(doc.getPersistent()).thenReturn(null);

		assertNotEquals(Boolean.TRUE, declaredMethod("testPolymorphic", Document.class)
				.invoke(gen, doc));
	}

	@Test
	void testPolymorphicReturnsFalseWhenPolymorphicallyMapped() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		Persistent persistent = mock(Persistent.class);
		when(persistent.isPolymorphicallyMapped()).thenAnswer(i -> Boolean.TRUE);
		when(doc.getPersistent()).thenReturn(persistent);

		assertNotEquals(Boolean.TRUE, declaredMethod("testPolymorphic", Document.class)
				.invoke(gen, doc));
	}

	@Test
	void testPolymorphicReturnsFalseWhenNoDerivations() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		Persistent persistent = mock(Persistent.class);
		when(persistent.isPolymorphicallyMapped()).thenAnswer(i -> Boolean.FALSE);
		when(doc.getPersistent()).thenReturn(persistent);
		when(doc.getOwningModuleName()).thenReturn("test");
		when(doc.getName()).thenReturn("Base");

		// modocDerivations is empty, so testPolymorphicAllTheWayDown returns false
		assertNotEquals(Boolean.TRUE, declaredMethod("testPolymorphic", Document.class)
				.invoke(gen, doc));
	}

	// ----- testPolymorphicAllTheWayDown -------------------------------------

	@Test
	void testPolymorphicAllTheWayDownReturnsFalseWhenNoDerivations() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("test");
		when(doc.getName()).thenReturn("Base");

		assertNotEquals(Boolean.TRUE, declaredMethod("testPolymorphicAllTheWayDown", Document.class)
				.invoke(gen, doc));
	}

	@Test
	void testPolymorphicAllTheWayDownReturnsTrueWhenChildHasJoinedStrategy() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Setup: Base document with a child document using joined strategy
		Document base = mock(Document.class);
		when(base.getOwningModuleName()).thenReturn("test");
		when(base.getName()).thenReturn("Base");

		Document child = mock(Document.class);
		when(child.getOwningModuleName()).thenReturn("test");
		when(child.getName()).thenReturn("Child");

		Persistent childPersistent = mock(Persistent.class);
		when(childPersistent.getStrategy()).thenReturn(ExtensionStrategy.joined);
		when(child.getPersistent()).thenReturn(childPersistent);

		// Manually set modocDerivations field
		java.lang.reflect.Field modocField = OverridableDomainGenerator.class.getDeclaredField("modocDerivations");
		modocField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Document>> modoc =
				(java.util.TreeMap<String, java.util.TreeMap<String, Document>>) modocField.get(gen);
		java.util.TreeMap<String, Document> derivations = new java.util.TreeMap<>();
		derivations.put("test.Child", child);
		modoc.put("test.Base", derivations);

		assertEquals(Boolean.TRUE, declaredMethod("testPolymorphicAllTheWayDown", Document.class)
				.invoke(gen, base));
	}

	// ----- validateDocumentAttributeNames -----------------------------------

	@Test
	void validateDocumentAttributeNamesNullDocumentDoesNotThrow() throws Exception {
		OverridableDomainGenerator gen = generator();
		declaredMethod("validateDocumentAttributeNames", Document.class)
				.invoke(gen, (Document) null);
		// If no exception is thrown, the test passes
		assertNotNull(gen);
	}

	@Test
	void validateDocumentAttributeNamesValidAttributeDoesNotThrow() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TestDoc");
		doc.setOwningModuleName("testMod");
		// Transient doc (no persistent) - transient attributes are skipped after underscore check
		Text field = new Text();
		field.setName("validName");
		doc.putAttribute(field);

		// Should not throw
		declaredMethod("validateDocumentAttributeNames", Document.class)
				.invoke(gen, doc);
		assertNotNull(doc.getName());
	}

	@Test
	void validateDocumentAttributeNamesUnderscoreThrows() {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TestDoc");
		doc.setOwningModuleName("testMod");
		Text field = new Text();
		field.setName("invalid_name");
		doc.putAttribute(field);

		java.lang.reflect.InvocationTargetException ex = assertThrows(
				java.lang.reflect.InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
		assertTrue(ex.getCause() instanceof MetaDataException);
		assertTrue(ex.getCause().getMessage().contains("underscore"));
	}

	// ----- appendEnumDefinition ---------------------------------------------

	@Test
	void appendEnumDefinitionGeneratesEnumSourceCode() throws Exception {
		OverridableDomainGenerator gen = generator();

		Enumeration enumeration = new Enumeration();
		enumeration.setName("status");
		enumeration.setDisplayName("Status");

		EnumeratedValue pending = new EnumeratedValue();
		pending.setCode("pending");
		pending.setDescription("Pending");

		EnumeratedValue active = new EnumeratedValue();
		active.setCode("active");
		active.setDescription("Active");

		enumeration.getXmlValues().add(pending);
		enumeration.getXmlValues().add(active);

		StringBuilder sb = new StringBuilder();
		declaredMethod("appendEnumDefinition",
				Enumeration.class, String.class, StringBuilder.class)
				.invoke(gen, enumeration, "Status", sb);

		String result = sb.toString();
		assertTrue(result.contains("public static enum Status"));
		assertTrue(result.contains("pending(\"pending\", \"Pending\")"));
		assertTrue(result.contains("active(\"active\", \"Active\")"));
		assertTrue(result.contains("private String code;"));
		assertTrue(result.contains("toCode()"));
		assertTrue(result.contains("fromCode(String code)"));
		assertTrue(result.contains("toDomainValues()"));
	}

	// ----- generateDomainTest -----------------------------------------------

	@Test
	void generateDomainTestProducesTestClass() throws Exception {
		OverridableDomainGenerator gen = generator();
		StringBuilder sb = new StringBuilder();

		declaredMethod("generateDomainTest",
				StringBuilder.class, String.class, String.class, String.class)
				.invoke(gen, sb, "modules/testMod", "modules.testMod.domain", "TestDocument");

		String result = sb.toString();
		assertTrue(result.contains("package modules.testMod.domain;"));
		assertTrue(result.contains("import util.AbstractDomainTest;"));
		assertTrue(result.contains("public class TestDocumentTest"));
		assertTrue(result.contains("extends AbstractDomainTest<TestDocument>"));
		assertTrue(result.contains("getBean()"));
		assertTrue(result.contains("DataBuilder"));
	}

	// ----- generateActionTest -----------------------------------------------

	@Test
	void generateActionTestWithoutExtension() throws Exception {
		OverridableDomainGenerator gen = generator();
		StringBuilder sb = new StringBuilder();

		declaredMethod("generateActionTest",
				StringBuilder.class, String.class, String.class, String.class,
				String.class, String.class, boolean.class)
				.invoke(gen, sb, "modules/testMod", "modules.testMod.TestDoc.actions",
						"modules.testMod.domain", "TestDoc", "Save", Boolean.FALSE);

		String result = sb.toString();
		assertTrue(result.contains("package modules.testMod.TestDoc.actions;"));
		assertTrue(result.contains("import util.AbstractActionTest;"));
		assertTrue(result.contains("public class SaveTest"));
		assertTrue(result.contains("extends AbstractActionTest<TestDoc, Save>"));
		assertTrue(result.contains("getAction()"));
		assertTrue(result.contains("return new Save()"));
	}

	@Test
	void generateActionTestWithExtension() throws Exception {
		OverridableDomainGenerator gen = generator();
		StringBuilder sb = new StringBuilder();

		declaredMethod("generateActionTest",
				StringBuilder.class, String.class, String.class, String.class,
				String.class, String.class, boolean.class)
				.invoke(gen, sb, "modules/testMod", "modules.testMod.TestDoc.actions",
						"modules.testMod.domain", "TestDoc", "Submit", Boolean.TRUE);

		String result = sb.toString();
		assertTrue(result.contains("TestDocExtension"));
		assertTrue(result.contains("extends AbstractActionTest<TestDocExtension, Submit>"));
	}

	// ----- generateJava with minimal transient document --------------------

	@Test
	void generateJavaForMinimalTransientDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("MinimalDoc");
		doc.setOwningModuleName("testMod");
		doc.setSingularAlias("Minimal Document");
		// No persistent set → transient document
		// No attributes, conditions, interfaces, parent

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				StringBuilder.class,
				String.class,
				String.class,
				String.class,
				boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "MinimalDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("package modules.testMod.domain;"), "Should contain package declaration");
		assertTrue(java.contains("public class MinimalDoc"), "Should contain class declaration");
		assertTrue(java.contains("extends AbstractTransientBean"), "Should extend AbstractTransientBean for transient doc");
		assertTrue(java.contains("MODULE_NAME"), "Should contain MODULE_NAME constant");
		assertTrue(java.contains("DOCUMENT_NAME"), "Should contain DOCUMENT_NAME constant");
		assertTrue(java.contains("getBizModule()"), "Should contain getBizModule()");
		assertTrue(java.contains("getBizDocument()"), "Should contain getBizDocument()");
		assertTrue(java.contains("serialVersionUID"), "Should contain serialVersionUID");
	}

	@Test
	void generateJavaForMinimalTransientDocumentWithCondition() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DocWithCondition");
		doc.setOwningModuleName("testMod");
		// Add a condition directly via getConditions().put()
		org.skyve.impl.metadata.model.document.ConditionImpl cond = new org.skyve.impl.metadata.model.document.ConditionImpl();
		cond.setExpression("name != null");
		doc.getConditions().put("hasName", cond);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				StringBuilder.class,
				String.class,
				String.class,
				String.class,
				boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DocWithCondition", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("isHasName()"), "Should contain condition method");
		assertTrue(java.contains("isNotHasName()"), "Should contain negated condition method");
	}

	// ----- generateJava with multiple field types ---------------------------

	@Test
	void generateJavaWithDateTimeAndDecimalFields() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("FieldDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.field.Date dateField =
				new org.skyve.impl.metadata.model.document.field.Date();
		dateField.setName("myDate");
		dateField.setDisplayName("My Date");
		doc.putAttribute(dateField);

		org.skyve.impl.metadata.model.document.field.DateTime dtField =
				new org.skyve.impl.metadata.model.document.field.DateTime();
		dtField.setName("myDateTime");
		dtField.setDisplayName("My DateTime");
		doc.putAttribute(dtField);

		org.skyve.impl.metadata.model.document.field.Decimal2 dec2Field =
				new org.skyve.impl.metadata.model.document.field.Decimal2();
		dec2Field.setName("myDecimal");
		dec2Field.setDisplayName("My Decimal");
		doc.putAttribute(dec2Field);

		org.skyve.impl.metadata.model.document.field.Decimal5 dec5Field =
				new org.skyve.impl.metadata.model.document.field.Decimal5();
		dec5Field.setName("myDecimal5");
		dec5Field.setDisplayName("My Decimal5");
		doc.putAttribute(dec5Field);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "FieldDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("DateOnly"), "Should contain DateOnly type for Date field");
		assertTrue(java.contains("DateTimeMapper"), "Should contain DateTimeMapper for DateTime field");
		assertTrue(java.contains("Decimal2Mapper"), "Should contain Decimal2Mapper");
		assertTrue(java.contains("Decimal5Mapper"), "Should contain Decimal5Mapper");
		assertTrue(java.contains("getMyDate()"), "Should have getter for Date field");
		assertTrue(java.contains("getMyDecimal()"), "Should have getter for Decimal2 field");
	}

	@Test
	void generateJavaWithDecimal10AndGeometryAndTimestamp() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("MoreFieldDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.field.Decimal10 dec10 =
				new org.skyve.impl.metadata.model.document.field.Decimal10();
		dec10.setName("amount");
		dec10.setDisplayName("Amount");
		doc.putAttribute(dec10);

		org.skyve.impl.metadata.model.document.field.Geometry geom =
				new org.skyve.impl.metadata.model.document.field.Geometry();
		geom.setName("location");
		geom.setDisplayName("Location");
		doc.putAttribute(geom);

		org.skyve.impl.metadata.model.document.field.Timestamp ts =
				new org.skyve.impl.metadata.model.document.field.Timestamp();
		ts.setName("created");
		ts.setDisplayName("Created");
		doc.putAttribute(ts);

		org.skyve.impl.metadata.model.document.field.Time timeField =
				new org.skyve.impl.metadata.model.document.field.Time();
		timeField.setName("startTime");
		timeField.setDisplayName("Start Time");
		doc.putAttribute(timeField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "MoreFieldDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Decimal10Mapper"), "Should contain Decimal10Mapper");
		assertTrue(java.contains("GeometryMapper"), "Should contain GeometryMapper");
		assertTrue(java.contains("TimestampMapper"), "Should contain TimestampMapper");
		assertTrue(java.contains("TimeOnlyMapper"), "Should contain TimeOnlyMapper");
		assertTrue(java.contains("getAmount()"), "Should have getter for Decimal10");
		assertTrue(java.contains("getLocation()"), "Should have getter for Geometry");
	}

	@Test
	void generateJavaWithBooleanIntegerLongIntegerFields() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("NumericDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.field.Boolean boolField =
				new org.skyve.impl.metadata.model.document.field.Boolean();
		boolField.setName("active");
		boolField.setDisplayName("Active");
		doc.putAttribute(boolField);

		org.skyve.impl.metadata.model.document.field.Integer intField =
				new org.skyve.impl.metadata.model.document.field.Integer();
		intField.setName("count");
		intField.setDisplayName("Count");
		doc.putAttribute(intField);

		org.skyve.impl.metadata.model.document.field.LongInteger longField =
				new org.skyve.impl.metadata.model.document.field.LongInteger();
		longField.setName("bigCount");
		longField.setDisplayName("Big Count");
		doc.putAttribute(longField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "NumericDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Boolean"), "Should contain Boolean type");
		assertTrue(java.contains("Integer"), "Should contain Integer type");
		assertTrue(java.contains("Long"), "Should contain Long type");
		assertTrue(java.contains("getActive()"), "Should have getter for bool");
		assertTrue(java.contains("getCount()"), "Should have getter for integer");
		assertTrue(java.contains("getBigCount()"), "Should have getter for longInteger");
	}

	@Test
	void generateJavaWithInlineEnumerationField() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("EnumDoc");
		doc.setOwningModuleName("testMod");

		// Create an inline Enumeration field (no attributeRef = inline)
		Enumeration enumField = new Enumeration();
		enumField.setName("status");
		enumField.setDisplayName("Status");
		// No setAttributeRef → inline enumeration

		EnumeratedValue newVal = new EnumeratedValue();
		newVal.setCode("new");
		newVal.setDescription("New");
		enumField.getXmlValues().add(newVal);

		EnumeratedValue activeVal = new EnumeratedValue();
		activeVal.setCode("active");
		activeVal.setDescription("Active");
		enumField.getXmlValues().add(activeVal);

		doc.putAttribute(enumField);

		// The enumeration needs to know its owning document for class name resolution
		enumField.setOwningDocument(doc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "EnumDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Status"), "Should contain Status enum name");
		assertTrue(java.contains("getStatus()"), "Should have getter for enumeration field");
		// Inline enum generates DomainValue imports
		assertTrue(java.contains("DomainValue"), "Should import DomainValue for inline enum");
	}

	@Test
	void generateJavaWithBizKeyAndDeprecatedField() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BizKeyDoc");
		doc.setOwningModuleName("testMod");
		doc.setBizKeyMethodCode("\t\treturn name;");

		Text nameField = new Text();
		nameField.setName("name");
		nameField.setDisplayName("Name");
		doc.putAttribute(nameField);

		Text deprecatedField = new Text();
		deprecatedField.setName("oldField");
		deprecatedField.setDisplayName("Old Field");
		deprecatedField.setDeprecated(true);
		doc.putAttribute(deprecatedField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "BizKeyDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getBizKey()"), "Should contain bizKey method");
		assertTrue(java.contains("return name;"), "Should contain bizKey method code");
		assertTrue(java.contains("@Deprecated"), "Should contain @Deprecated for deprecated field");
		assertTrue(java.contains("getOldField()"), "Should have getter for deprecated field");
	}

	@Test
	void generateJavaWithMarkupAndMemoFields() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TextDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.field.Markup markupField =
				new org.skyve.impl.metadata.model.document.field.Markup();
		markupField.setName("richText");
		markupField.setDisplayName("Rich Text");
		doc.putAttribute(markupField);

		org.skyve.impl.metadata.model.document.field.Memo memoField =
				new org.skyve.impl.metadata.model.document.field.Memo();
		memoField.setName("notes");
		memoField.setDisplayName("Notes");
		doc.putAttribute(memoField);

		org.skyve.impl.metadata.model.document.field.Content contentField =
				new org.skyve.impl.metadata.model.document.field.Content();
		contentField.setName("attachment");
		contentField.setDisplayName("Attachment");
		doc.putAttribute(contentField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "TextDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getRichText()"), "Should have getter for Markup");
		assertTrue(java.contains("getNotes()"), "Should have getter for Memo");
		assertTrue(java.contains("getAttachment()"), "Should have getter for Content");
		assertTrue(java.contains("String"), "Should have String types");
	}

	// ----- generateJava with default values ---------------------------------

	@Test
	void generateJavaWithStringDefaultValue() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DefaultDoc");
		doc.setOwningModuleName("testMod");

		Text nameField = new Text();
		nameField.setName("code");
		nameField.setDisplayName("Code");
		nameField.setLength(50);
		nameField.setDefaultValue("ACTIVE");
		doc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DefaultDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("= \"ACTIVE\""), "Should have plain string default value");
	}

	@Test
	void generateJavaWithStringExpressionDefaultValue() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ExprDefaultDoc");
		doc.setOwningModuleName("testMod");

		Text nameField = new Text();
		nameField.setName("label");
		nameField.setDisplayName("Label");
		nameField.setLength(100);
		// containsSkyveExpressions returns true for strings containing unescaped {
		nameField.setDefaultValue("{title}");
		doc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "ExprDefaultDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Binder.formatMessage"), "Should have Binder.formatMessage for expression default");
	}

	@Test
	void generateJavaWithNonStringDefaultValue() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("NumDefaultDoc");
		doc.setOwningModuleName("testMod");

		// Integer field with a default value (takes the bool/integer/longInteger branch)
		org.skyve.impl.metadata.model.document.field.Integer intField =
				new org.skyve.impl.metadata.model.document.field.Integer();
		intField.setName("count");
		intField.setDisplayName("Count");
		intField.setDefaultValue("0");
		doc.putAttribute(intField);

		// Date field with non-Skyve expression default (takes the "new ClassName(...)" branch)
		org.skyve.impl.metadata.model.document.field.Date dateField =
				new org.skyve.impl.metadata.model.document.field.Date();
		dateField.setName("startDate");
		dateField.setDisplayName("Start Date");
		dateField.setDefaultValue("2024-01-01");
		doc.putAttribute(dateField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "NumDefaultDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Integer.valueOf(0)"), "Should have Integer.valueOf for integer default");
		assertTrue(java.contains("new DateOnly(\"2024-01-01\")"), "Should use constructor for DateOnly default");
	}

	@Test
	void generateJavaWithNonStringSkyveExpressionDefault() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SkyveExprDoc");
		doc.setOwningModuleName("testMod");

		// Date field with Skyve expression default
		org.skyve.impl.metadata.model.document.field.Date dateField =
				new org.skyve.impl.metadata.model.document.field.Date();
		dateField.setName("eventDate");
		dateField.setDisplayName("Event Date");
		// isSkyveExpression returns true when format is {expression}
		dateField.setDefaultValue("{el:someExpression}");
		doc.putAttribute(dateField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "SkyveExprDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("ExpressionEvaluator.evaluate"), "Should use ExpressionEvaluator for Skyve expression default");
	}

	// ----- generateJava with trackChanges -----------------------------------

	@Test
	void generateJavaWithTrackChanges() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TrackDoc");
		doc.setOwningModuleName("testMod");

		Text nameField = new Text();
		nameField.setName("status");
		nameField.setDisplayName("Status");
		nameField.setLength(50);
		nameField.setTrackChanges(true);
		doc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "TrackDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("preset(statusPropertyName, status)"),
				"Should have preset() call when trackChanges=true");
	}

	// ----- generateJava with 'created' condition (overridden) ---------------

	@Test
	void generateJavaWithCreatedConditionGeneratesOverride() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("CreatedCondDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.ConditionImpl cond =
				new org.skyve.impl.metadata.model.document.ConditionImpl();
		cond.setExpression("bizId != null");
		// "created" is a special name that generates @Override
		doc.getConditions().put("created", cond);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "CreatedCondDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@Override"), "Should have @Override for 'created' condition");
		assertTrue(java.contains("isCreated()"), "Should have isCreated() method");
		assertTrue(java.contains("isNotCreated()"), "Should have isNotCreated() negation");
	}

	@Test
	void generateJavaWithSkyveExpressionCondition() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SkyveCondDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.ConditionImpl cond =
				new org.skyve.impl.metadata.model.document.ConditionImpl();
		// Skyve expression (starts with '{' and ends with '}')
		cond.setExpression("{el:name != null}");
		doc.getConditions().put("hasName", cond);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "SkyveCondDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("ExpressionEvaluator.evaluate"), "Should use ExpressionEvaluator for Skyve expression condition");
		assertTrue(java.contains("Boolean.TRUE.equals"), "Should check Boolean.TRUE.equals for Skyve expression");
	}

	@Test
	void generateJavaWithConditionDescriptionAndDocumentation() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("CondDocDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.ConditionImpl cond =
				new org.skyve.impl.metadata.model.document.ConditionImpl();
		cond.setExpression("active == true");
		cond.setDescription("Whether active");
		cond.setDocumentation("Long documentation text");
		doc.getConditions().put("active", cond);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "CondDocDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Whether active"), "Should contain description in javadoc");
		assertTrue(java.contains("Long documentation text"), "Should contain documentation in javadoc");
		assertTrue(java.contains("isActive()"), "Should have isActive() method");
	}

	// ----- generateJava with child document (parent document name) ----------

	@Test
	void generateJavaForChildDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Parent document with a child collection referencing the child document
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("ParentDoc");
		parentDoc.setOwningModuleName("testMod");

		// Add child collection to parent so generateJava can find the back-reference
		org.skyve.impl.metadata.model.document.CollectionImpl childCollection =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		childCollection.setName("children");
		childCollection.setDisplayName("Children");
		childCollection.setDocumentName("ChildDoc");
		childCollection.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		parentDoc.putAttribute(childCollection);

		// Child document
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildDoc");
		childDoc.setOwningModuleName("testMod");
		childDoc.setParentDocumentName("ParentDoc");

		Text nameField = new Text();
		nameField.setName("childName");
		nameField.setDisplayName("Child Name");
		nameField.setLength(100);
		childDoc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ParentDoc")).thenReturn(parentDoc);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, childDoc, contents,
						"modules.testMod.domain", "ChildDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("ChildBean"), "Should import ChildBean for child document");
		assertTrue(java.contains("getChildName()"), "Should have getter for child field");
		assertTrue(java.contains("ParentDoc"), "Should reference parent document");
	}

	// ----- generateJava with persistent document ----------------------------

	@Test
	void generateJavaForPersistentDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("PersistDoc");
		doc.setOwningModuleName("testMod");
		Persistent persistent = new Persistent();
		persistent.setName("TEST_PERSIST_DOC");
		doc.setPersistent(persistent);

		Text nameField = new Text();
		nameField.setName("persName");
		nameField.setDisplayName("Persistent Name");
		nameField.setLength(100);
		doc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "PersistDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("AbstractPersistentBean"), "Should extend AbstractPersistentBean for persistent doc");
		assertTrue(java.contains("getPersName()"), "Should have getter");
	}

	// ----- generateJava with boolean default value -------------------------

	@Test
	void generateJavaWithBooleanDefaultValue() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BoolDefaultDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.field.Boolean boolField =
				new org.skyve.impl.metadata.model.document.field.Boolean();
		boolField.setName("enabled");
		boolField.setDisplayName("Enabled");
		boolField.setDefaultValue("true");
		doc.putAttribute(boolField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "BoolDefaultDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("Boolean.valueOf(true)"), "Should have Boolean.valueOf for bool default");
	}

	// ----- generateJava with interface --------------------------------------

	@Test
	void generateJavaWithInterface() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("InterfaceDoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.InterfaceImpl iface = new org.skyve.impl.metadata.model.InterfaceImpl();
		iface.setInterfaceName("java.io.Serializable");
		doc.putInterface(iface);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "InterfaceDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("implements java.io.Serializable"),
				"Should have implements clause for added interface");
	}

	// ----- generateJava with abstract document ------------------------------

	@Test
	void generateJavaForAbstractDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("AbstractDoc");
		doc.setOwningModuleName("testMod");
		doc.setAbstract(true);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "AbstractDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("abstract class AbstractDoc"),
				"Should have abstract keyword for abstract document");
	}

	// ----- generateJava for hierarchical document --------------------------

	@Test
	void generateJavaForHierarchicalDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		// A hierarchical document has parentDocumentName == its own name
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TreeNode");
		doc.setOwningModuleName("testMod");
		doc.setParentDocumentName("TreeNode"); // hierarchical: parent == self

		Text nameField = new Text();
		nameField.setName("nodeName");
		nameField.setDisplayName("Node Name");
		nameField.setLength(100);
		doc.putAttribute(nameField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TreeNode")).thenReturn(doc);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "TreeNode", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("HierarchicalBean"), "Should implement HierarchicalBean");
		assertTrue(java.contains("getBizParentId()"), "Should have getBizParentId() for hierarchical");
		assertTrue(java.contains("getParent()"), "Should have getParent() for hierarchical");
		assertTrue(java.contains("getChildren()"), "Should have getChildren() for hierarchical");
	}

	// ----- generateJava with child document having interface ----------------

	@Test
	void generateJavaForChildDocumentWithInterface() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Parent document with a child collection
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("OwnerDoc");
		parentDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.CollectionImpl childColl =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		childColl.setName("items");
		childColl.setDisplayName("Items");
		childColl.setDocumentName("ItemDoc");
		childColl.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		parentDoc.putAttribute(childColl);

		// Child document with an interface
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ItemDoc");
		childDoc.setOwningModuleName("testMod");
		childDoc.setParentDocumentName("OwnerDoc");

		org.skyve.impl.metadata.model.InterfaceImpl iface = new org.skyve.impl.metadata.model.InterfaceImpl();
		iface.setInterfaceName("java.lang.Cloneable");
		childDoc.putInterface(iface);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "OwnerDoc")).thenReturn(parentDoc);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, childDoc, contents,
						"modules.testMod.domain", "ItemDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("ChildBean"), "Should implement ChildBean");
		assertTrue(java.contains("Cloneable"), "Should also implement Cloneable interface");
	}

	// ----- validateDocumentAttributeNames - dialect reserved words ----------

	@Test
	void validateDocumentAttributeNamesMssqlReservedWordThrows() throws Exception {
		// "exec" is SQL Server reserved but NOT H2 reserved
		// The MSSQL check triggers first
		OverridableDomainGenerator gen = generator(DialectOptions.MSSQL_2014);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("MssqlDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("MssqlDocTable"));

		Text field = new Text();
		field.setName("exec"); // SQL Server reserved word
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc),
				"Should throw MetaDataException for SQL Server reserved word 'exec'");
	}

	@Test
	void validateDocumentAttributeNamesMssql2016ReservedWordThrows() throws Exception {
		// Use MSSQL_2016 dialect as well to cover that branch
		OverridableDomainGenerator gen = generator(DialectOptions.MSSQL_2016);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("Mssql2016Doc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("Mssql2016DocTable"));

		Text field = new Text();
		field.setName("pivot"); // SQL Server reserved word, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesMysql5ReservedWordThrows() throws Exception {
		// "accessible" is MySQL 5 reserved but NOT H2 reserved
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("Mysql5Doc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("Mysql5DocTable"));

		Text field = new Text();
		field.setName("accessible"); // MySQL 5 reserved, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesMysql5ByteCharsetReservedWordThrows() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_5_4_BYTE_CHARSET);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("Mysql5ByteDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("Mysql5ByteDocTable"));

		Text field = new Text();
		field.setName("generated"); // MySQL 5 reserved, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesMysql8ReservedWordThrows() throws Exception {
		// "lateral" is MySQL 8 reserved but NOT H2 reserved
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_8);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("Mysql8Doc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("Mysql8DocTable"));

		Text field = new Text();
		field.setName("lateral"); // MySQL 8 reserved, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesMysql8ByteReservedWordThrows() throws Exception {
		OverridableDomainGenerator gen = generator(DialectOptions.MYSQL_8_4_BYTE_CHARSET);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("Mysql8ByteDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("Mysql8ByteDocTable"));

		Text field = new Text();
		field.setName("recursive"); // MySQL 8 reserved, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesPostgresqlReservedWordThrows() throws Exception {
		// "verbose" is PostgreSQL reserved but NOT H2 reserved
		OverridableDomainGenerator gen = generator(DialectOptions.POSTGRESQL);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("PgDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("PgDocTable"));

		Text field = new Text();
		field.setName("verbose"); // PostgreSQL reserved, not H2
		field.setLength(50);
		doc.putAttribute(field);

		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesH2ValidOnOtherDialect() throws Exception {
		// Test that a word reserved in H2 throws even on MSSQL dialect (H2 always checked)
		OverridableDomainGenerator gen = generator(DialectOptions.MSSQL_2014);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("H2ReservedOnMssql");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("H2ReservedOnMssqlTable"));

		Text field = new Text();
		field.setName("rownum"); // H2 reserved, not SQL Server reserved
		field.setLength(50);
		doc.putAttribute(field);

		// H2 is always checked at the end of validateDocumentAttributeNames
		assertThrows(InvocationTargetException.class,
				() -> declaredMethod("validateDocumentAttributeNames", Document.class)
						.invoke(gen, doc));
	}

	@Test
	void validateDocumentAttributeNamesTransientAttributeSkipped() throws Exception {
		// Transient attributes (no persistent on doc) should be skipped
		OverridableDomainGenerator gen = generator(DialectOptions.MSSQL_2014);
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientDoc");
		doc.setOwningModuleName("testMod");
		// No persistent set - all attributes are transient

		Text field = new Text();
		field.setName("exec"); // SQL Server reserved, but doc is transient
		field.setLength(50);
		doc.putAttribute(field);

		// Should NOT throw because doc has no Persistent = all attributes transient
		declaredMethod("validateDocumentAttributeNames", Document.class).invoke(gen, doc);
	}

	// ----- generateJava with bizKeyMethodCode --------------------------------

	@Test
	void generateJavaWithBizKey() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BizKeyDoc");
		doc.setOwningModuleName("testMod");
		doc.setBizKeyMethodCode("\t\treturn \"myBizKey\";");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "BizKeyDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getBizKey()"), "Should have getBizKey method");
		assertTrue(java.contains("myBizKey"), "Should include bizKey implementation");
	}

	// ----- generateJava with dynamic field ----------------------------------

	@Test
	void generateJavaWithDynamicField() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DynamicFieldDoc");
		doc.setOwningModuleName("testMod");

		// Dynamic field - only generates the static property name constant
		Text field = new Text();
		field.setName("dynamicProp");
		field.setLength(100);
		field.setDynamic(true);
		doc.putAttribute(field);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DynamicFieldDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("dynamicPropPropertyName"), "Dynamic field should generate property name constant");
		// No getter/setter generated for dynamic fields
		assertFalse(java.contains("getDynamicProp()"), "Dynamic field should NOT generate getter");
	}

	// ---- helper for persistent document attr tests --------------------------

	private static Persistent persistentOf(String tableName) {
		Persistent p = new Persistent();
		p.setName(tableName);
		return p;
	}

	// ----- generateJava with document description/documentation -------------

	@Test
	void generateJavaWithDocumentDescription() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DescriptionDoc");
		doc.setOwningModuleName("testMod");
		doc.setSingularAlias("Description Document");
		doc.setDescription("A document with a description");
		doc.setDocumentation("Javadoc documentation for this document");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DescriptionDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("A document with a description"), "Should include description");
		assertTrue(java.contains("Javadoc documentation for this document"), "Should include documentation");
	}

	// ----- generateJava with extended (base) document ----------------------

	@Test
	void generateJavaExtendsBaseDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Child/extension document
		DocumentImpl doc = new DocumentImpl();
		doc.setName("ExtendedDoc");
		doc.setOwningModuleName("testMod");

		// Base document
		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseDoc");
		baseDoc.setOwningModuleName("testMod");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDoc);

		StringBuilder contents = new StringBuilder();
		// Pass "BaseDoc" as the baseDocumentName parameter (6th arg = documentName, 7th = baseDocumentName)
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "ExtendedDoc", "BaseDoc", Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("extends BaseDoc"), "Should extend BaseDoc not AbstractBean");
		assertFalse(java.contains("extends AbstractPersistentBean"), "Should NOT extend AbstractPersistentBean");
		assertFalse(java.contains("extends AbstractTransientBean"), "Should NOT extend AbstractTransientBean");
	}

	// ----- generateJava with deprecated attribute ---------------------------

	@Test
	void generateJavaWithDeprecatedAttribute() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DeprecatedDoc");
		doc.setOwningModuleName("testMod");

		Text field = new Text();
		field.setName("oldField");
		field.setLength(100);
		field.setDeprecated(true);
		doc.putAttribute(field);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DeprecatedDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@Deprecated"), "Should have @Deprecated annotation");
	}

	// ----- generateJava persistent with description -------------------------

	@Test
	void generateJavaPersistentWithDescription() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("PersistentDescDoc");
		doc.setOwningModuleName("testMod");
		doc.setSingularAlias("Persistent Description Document");
		doc.setPersistent(persistentOf("PST_DESC_DOC"));

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "PersistentDescDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@stereotype \"persistent\""), "Persistent doc should have persistent stereotype");
		assertTrue(java.contains("extends AbstractPersistentBean"), "Should extend AbstractPersistentBean");
	}

	// ----- generateJava with debug mode -------------------------------------

	@Test
	void generateJavaWithDebugMode() throws Exception {
		// debug=true activates extra print statements in generateJava
		OverridableDomainGenerator gen = new OverridableDomainGenerator(false, true, false,
				DialectOptions.H2_NO_INDEXES, "", "", "", "", null);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DebugDoc");
		doc.setOwningModuleName("testMod");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		// Should not throw - the debug println goes to System.out
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DebugDoc", null, Boolean.FALSE);

		assertTrue(contents.toString().contains("DebugDoc"), "Should generate class for DebugDoc");
	}

	// ----- generateJava with transient field --------------------------------

	@Test
	void generateJavaWithTransientField() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientFieldDoc");
		doc.setOwningModuleName("testMod");

		Text field = new Text();
		field.setName("tempData");
		field.setDisplayName("Temp Data");
		field.setLength(50);
		field.setTransient(true); // Mark as transient - generates 'transient' keyword
		doc.putAttribute(field);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "TransientFieldDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("transient String tempData"), "Should have transient keyword in field declaration");
	}

	// ----- generateJava with dynamic deprecated field -----------------------

	@Test
	void generateJavaWithDynamicDeprecatedField() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DynDeprecatedDoc");
		doc.setOwningModuleName("testMod");

		// Dynamic deprecated field - generates @Deprecated above the property name constant
		Text field = new Text();
		field.setName("oldDynamicProp");
		field.setLength(100);
		field.setDynamic(true);
		field.setDeprecated(true);
		doc.putAttribute(field);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "DynDeprecatedDoc", null, Boolean.FALSE);

		String java = contents.toString();
		// The @Deprecated annotation appears before the property name constant
		assertTrue(java.contains("@Deprecated"), "Dynamic deprecated field should have @Deprecated annotation");
		assertTrue(java.contains("oldDynamicPropPropertyName"), "Should have property name constant");
	}

	// ----- generateJava with association (covers reference names UML comment) ---

	@Test
	void generateJavaWithAssociationCoverReferenceNamesLoop() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Setup reference document
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("RefDoc");
		refDoc.setOwningModuleName("testMod");

		// Document with an association attribute
		DocumentImpl doc = new DocumentImpl();
		doc.setName("OwnerWithAssoc");
		doc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("relatedDoc");
		assoc.setDisplayName("Related Document");
		assoc.setDocumentName("RefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "RefDoc")).thenReturn(refDoc);

		// Need to populate moduleDocumentVanillaClasses so addReference doesn't NPE on customer.getName()
		// DomainClass is a private inner class - instantiate via reflection
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("RefDoc", domainClassInstance); // non-null entry avoids NPE on customer.getName()
		vanillaClasses.put("testMod", modClasses);

		doc.putRelation(assoc);

		StringBuilder contents = new StringBuilder();
		// This will call addReference which will try to process the association
		// With vanilla classes set, it won't need customer.getName()
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "OwnerWithAssoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@navhas"), "Should have @navhas UML comment for aggregation");
	}

	// ----- generateJava with child collection (covers addReference + UML) --

	@Test
	void generateJavaWithChildCollection() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Child document that references parent
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildItem");
		childDoc.setOwningModuleName("testMod");
		childDoc.setParentDocumentName("ParentContainer"); // required for child collection validation

		// Parent document with a child collection
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("ParentContainer");
		parentDoc.setOwningModuleName("testMod");
		parentDoc.setPersistent(persistentOf("PARENT_CONTAINER"));

		org.skyve.impl.metadata.model.document.CollectionImpl childColl =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		childColl.setName("items");
		childColl.setDisplayName("Items");
		childColl.setDocumentName("ChildItem");
		childColl.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);

		parentDoc.putRelation(childColl);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ChildItem")).thenReturn(childDoc);

		// Set up moduleDocumentVanillaClasses so addReference doesn't NPE
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("ChildItem", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, parentDoc, contents,
						"modules.testMod.domain", "ParentContainer", null, Boolean.FALSE);

		String java = contents.toString();
		// Child collection generates @navcomposed 1 in UML comment
		assertTrue(java.contains("@navcomposed 1"), "Should have @navcomposed 1 UML comment for child collection");
		// And generates the list field + add/remove methods
		assertTrue(java.contains("List<ChildItem>"), "Should have List<ChildItem> field");
		assertTrue(java.contains("addItemsElement"), "Should have addItemsElement method");
	}

	// ----- generateJava with composition collection -------------------------

	@Test
	void generateJavaWithCompositionCollection() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Member document for the composition
		DocumentImpl memberDoc = new DocumentImpl();
		memberDoc.setName("MemberDoc");
		memberDoc.setOwningModuleName("testMod");

		// Owning document with a composition collection
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.CollectionImpl compColl =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		compColl.setName("members");
		compColl.setDisplayName("Members");
		compColl.setDocumentName("MemberDoc");
		compColl.setType(org.skyve.metadata.model.document.Collection.CollectionType.composition);
		compColl.setMinCardinality(0);

		ownerDoc.putRelation(compColl);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "MemberDoc")).thenReturn(memberDoc);

		// Set up moduleDocumentVanillaClasses
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("MemberDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "OwnerDoc", null, Boolean.FALSE);

		String java = contents.toString();
		// Composition collection generates @navcomposed n in UML comment
		assertTrue(java.contains("@navcomposed n"), "Should have @navcomposed n UML comment for composition");
		assertTrue(java.contains("List<MemberDoc>"), "Should have List<MemberDoc> field");
	}

	// ----- generateJava with composition association (covers @navcomposed n UML) ---

	@Test
	void generateJavaWithCompositionAssociation() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Target document
		DocumentImpl targetDoc = new DocumentImpl();
		targetDoc.setName("TargetDoc");
		targetDoc.setOwningModuleName("testMod");

		// Document with a composition association
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerWithCompositionAssoc");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("composedTarget");
		assoc.setDisplayName("Composed Target");
		assoc.setDocumentName("TargetDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.composition);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TargetDoc")).thenReturn(targetDoc);

		// Set up moduleDocumentVanillaClasses
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("TargetDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "OwnerWithCompositionAssoc", null, Boolean.FALSE);

		String java = contents.toString();
		// Composition association generates @navcomposed n 0..1 in UML
		assertTrue(java.contains("@navcomposed n"), "Should have @navcomposed UML for composition assoc");
		// And the getter/setter for the association
		assertTrue(java.contains("getComposedTarget"), "Should have getter for composed target");
	}

	// ----- generateJava with InverseOne attribute (covers addInverse) -------

	@Test
	void generateJavaWithInverseOne() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Property document (the one that has the forward reference)
		DocumentImpl propDoc = new DocumentImpl();
		propDoc.setName("PropDoc");
		propDoc.setOwningModuleName("testMod");

		// Document with an InverseOne attribute
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("InverseOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseOne inverseOne =
				new org.skyve.impl.metadata.model.document.InverseOne();
		inverseOne.setName("backRef");
		inverseOne.setDisplayName("Back Reference");
		inverseOne.setDocumentName("PropDoc");
		inverseOne.setReferenceName("owner"); // the association name in PropDoc pointing back
		inverseOne.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToOne);

		ownerDoc.putRelation(inverseOne);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "PropDoc")).thenReturn(propDoc);

		// Set up moduleDocumentVanillaClasses
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("PropDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "InverseOwnerDoc", null, Boolean.FALSE);

		String java = contents.toString();
		// InverseOne generates a single object getter
		assertTrue(java.contains("PropDoc getBackRef()"), "Should have single-object inverse getter");
	}

	// ----- generateJava with InverseMany attribute (covers addInverse toMany) ---

	@Test
	void generateJavaWithInverseMany() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Property document
		DocumentImpl propDoc = new DocumentImpl();
		propDoc.setName("PropManyDoc");
		propDoc.setOwningModuleName("testMod");

		// Document with an InverseMany attribute
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("InverseManyOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseMany inverseMany =
				new org.skyve.impl.metadata.model.document.InverseMany();
		inverseMany.setName("backRefs");
		inverseMany.setDisplayName("Back References");
		inverseMany.setDocumentName("PropManyDoc");
		inverseMany.setReferenceName("owner");
		inverseMany.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToMany);

		ownerDoc.putRelation(inverseMany);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "PropManyDoc")).thenReturn(propDoc);

		// Set up moduleDocumentVanillaClasses
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("PropManyDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "InverseManyOwnerDoc", null, Boolean.FALSE);

		String java = contents.toString();
		// InverseMany generates a list getter + add/remove methods
		assertTrue(java.contains("List<PropManyDoc> getBackRefs()"), "Should have list inverse getter");
		assertTrue(java.contains("addBackRefsElement"), "Should have add method for inverse many");
		assertTrue(java.contains("removeBackRefsElement"), "Should have remove method for inverse many");
	}

	// ----- addReference with customer path (covers lines 2211-2214) ----------

	@Test
	void generateJavaWithAssociationUsingCustomerPackagePath() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Reference document
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("CustomerRefDoc");
		refDoc.setOwningModuleName("custMod");

		// Owning document with an aggregation association
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("CustomerOwner");
		ownerDoc.setOwningModuleName("custMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("refField");
		assoc.setDisplayName("Ref Field");
		assoc.setDocumentName("CustomerRefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("custMod");

		// Use a customer mock - when moduleDocumentVanillaClasses is empty,
		// customer.getName() will be called to build the customer-specific package path
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		when(customer.getName()).thenReturn("testCustomer");
		when(module.getDocument(customer, "CustomerRefDoc")).thenReturn(refDoc);

		// Do NOT populate moduleDocumentVanillaClasses - this forces the customer path

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, customer, module, ownerDoc, contents,
						"modules.custMod.domain", "CustomerOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getRefField"), "Should have getter for the association");
	}

	// ----- populatePropertyLengths tests ------------------------------------

	@Test
	void populatePropertyLengthsWithPersistentDocAndTextField() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Create a document with a persistent table and a text attribute
		DocumentImpl doc = new DocumentImpl();
		doc.setName("PopPropDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("TestPropertyTable"));

		Text textField = new Text();
		textField.setName("myText");
		textField.setDisplayName("My Text");
		textField.setLength(200);
		doc.putAttribute(textField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		m.invoke(gen, null, module, doc, null);

		// Verify persistentPropertyLengths was populated
		java.lang.reflect.Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Integer>> propLengths =
				(java.util.TreeMap<String, java.util.TreeMap<String, Integer>>) f.get(gen);
		assertNotNull(propLengths.get("TestPropertyTable"), "Should have entry for TestPropertyTable");
		assertEquals(Integer.valueOf(200), propLengths.get("TestPropertyTable").get("myText"), "Text field length should be 200");
	}

	@Test
	void populatePropertyLengthsWithNoPeristentSkips() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientPopDoc");
		doc.setOwningModuleName("testMod");
		// No persistent - method should return immediately

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		m.invoke(gen, null, module, doc, null); // Should not throw

		java.lang.reflect.Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Integer>> propLengths =
				(java.util.TreeMap<String, java.util.TreeMap<String, Integer>>) f.get(gen);
		assertTrue(propLengths.isEmpty(), "No entries should be added for transient doc");
	}

	@Test
	void populatePropertyLengthsWithMappedStrategyAndNullDerivedId() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("MappedPopDoc");
		doc.setOwningModuleName("testMod");

		Persistent p = new Persistent();
		p.setStrategy(ExtensionStrategy.mapped);
		// No name on persistent (mapped strategy)
		doc.setPersistent(p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		// derivedPersistentIdentifier is null too → persistentIdentifier = null → early return
		m.invoke(gen, null, module, doc, null);

		java.lang.reflect.Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Integer>> propLengths =
				(java.util.TreeMap<String, java.util.TreeMap<String, Integer>>) f.get(gen);
		assertTrue(propLengths.isEmpty(), "Mapped doc with null derivedId should produce no entries");
	}

	// ----- populateModocDerivations tests -----------------------------------

	@Test
	void populateModocDerivationsNoExtendsNoPersistentSucceeds() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("NoExtendsDoc");
		doc.setOwningModuleName("testMod");
		// No extends, no persistent

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		m.invoke(gen, module, doc, null); // Should not throw
	}

	@Test
	void populateModocDerivationsExtendsDynamicBaseWithNonDynamicDocThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Base doc is dynamic
		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("DynamicBase");
		baseDoc.setOwningModuleName("testMod");
		baseDoc.setDynamism(new Dynamic()); // makes isDynamic() return true

		// Current doc is NOT dynamic - should throw MetaDataException
		DocumentImpl doc = new DocumentImpl();
		doc.setName("NonDynChild");
		doc.setOwningModuleName("testMod");
		// dynamic is null → isDynamic() returns false

		Extends inherits = new Extends();
		inherits.setDocumentName("DynamicBase");
		doc.setExtends(inherits);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DynamicBase")).thenReturn(baseDoc);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		assertThrows(InvocationTargetException.class, () -> m.invoke(gen, module, doc, null));
	}

	@Test
	void populateModocDerivationsWithInheritsNoPersistentSucceeds() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Base doc (non-dynamic, non-persistent)
		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("PlainBase");
		baseDoc.setOwningModuleName("testMod");

		// Derived doc with extends but NO persistent
		DocumentImpl doc = new DocumentImpl();
		doc.setName("DerivedNoPersist");
		doc.setOwningModuleName("testMod");

		Extends inherits = new Extends();
		inherits.setDocumentName("PlainBase");
		doc.setExtends(inherits);
		// persistent is null → (inherits && persistent) block is skipped

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "PlainBase")).thenReturn(baseDoc);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		m.invoke(gen, module, doc, null); // Should not throw
	}

	// ----- generateActionTests tests ----------------------------------------

	@Test
	void generateActionTestsWithUnresolvableActionSkipsGeneration() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ActionDoc");
		doc.setOwningModuleName("testMod");

		// Add a fake action name - the class won't be on the classpath
		java.lang.reflect.Field actionNamesField = DocumentImpl.class.getDeclaredField("definedActionNames");
		actionNamesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.Set<String> actionNames = (java.util.Set<String>) actionNamesField.get(doc);
		actionNames.add("NonExistentAction");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateActionTests",
				String.class, String.class, String.class,
				Document.class, String.class,
				org.skyve.util.test.SkyveFactory.class);
		m.setAccessible(true);
		// annotation=null, class not found → skipGeneration=true → nothing written
		m.invoke(gen, "testMod", "modules/testMod/domain", "modules/testMod", doc, "ActionDoc", null);
	}

	@Test
	void generateActionTestsDebugLogOnSkipGeneration() throws Exception {
		// Use debug=true generator to cover the debug logging path in the else branch
		OverridableDomainGenerator gen = new OverridableDomainGenerator(false, true, false,
				DialectOptions.H2_NO_INDEXES, "", "", "", "", null);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DebugActionDoc");
		doc.setOwningModuleName("testMod");

		java.lang.reflect.Field actionNamesField = DocumentImpl.class.getDeclaredField("definedActionNames");
		actionNamesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.Set<String> actionNames = (java.util.Set<String>) actionNamesField.get(doc);
		actionNames.add("DebugAction");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateActionTests",
				String.class, String.class, String.class,
				Document.class, String.class,
				org.skyve.util.test.SkyveFactory.class);
		m.setAccessible(true);
		m.invoke(gen, "testMod", "modules/testMod/domain", "modules/testMod", doc, "DebugActionDoc", null);
	}

	// ----- addReference with deprecated flag --------------------------------

	@Test
	void generateJavaWithDeprecatedAssociation() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("DeprecatedRefDoc");
		refDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("DeprecatedAssocOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("depRef");
		assoc.setDisplayName("Dep Ref");
		assoc.setDocumentName("DeprecatedRefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);
		assoc.setDeprecated(true);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DeprecatedRefDoc")).thenReturn(refDoc);

		// Set up vanilla classes
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> ctor = domainClassType.getDeclaredConstructor();
		ctor.setAccessible(true);
		Object domainClassInst = ctor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("DeprecatedRefDoc", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "DeprecatedAssocOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@Deprecated"), "Should have @Deprecated for deprecated association");
		assertTrue(java.contains("getDepRef"), "Should have getter for deprecated association");
	}

	// ----- addReference with transient flag ---------------------------------

	@Test
	void generateJavaWithTransientAssociation() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("TransientRefDoc");
		refDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("TransientAssocOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("transRef");
		assoc.setDisplayName("Trans Ref");
		assoc.setDocumentName("TransientRefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);
		assoc.setTransient(true);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TransientRefDoc")).thenReturn(refDoc);

		// Set up vanilla classes
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> ctor = domainClassType.getDeclaredConstructor();
		ctor.setAccessible(true);
		Object domainClassInst = ctor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("TransientRefDoc", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "TransientAssocOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("transient "), "Should have transient keyword for transient association");
		assertTrue(java.contains("getTransRef"), "Should have getter for transient association");
	}
}


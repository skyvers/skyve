package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;

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

	private static String normalisedPath(Path path) {
		return path.toString() + '/';
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

	@SuppressWarnings("java:S6201")
	private static Class<?> dataStoreTypeClass() throws Exception {
		for (Class<?> c : OverridableDomainGenerator.class.getDeclaredClasses()) {
			String name = c.getSimpleName();
			if ("DataStoreType".equals(name)) {
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
	@SuppressWarnings("boxing")
	void testPolymorphicReturnsFalseWhenPolymorphicallyMapped() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		Persistent persistent = mock(Persistent.class);
		when(persistent.isPolymorphicallyMapped()).thenReturn(Boolean.TRUE);
		when(doc.getPersistent()).thenReturn(persistent);

		assertNotEquals(Boolean.TRUE, declaredMethod("testPolymorphic", Document.class)
				.invoke(gen, doc));
	}

	@Test
	@SuppressWarnings("boxing")
	void testPolymorphicReturnsFalseWhenNoDerivations() throws Exception {
		OverridableDomainGenerator gen = generator();
		Document doc = mock(Document.class);
		Persistent persistent = mock(Persistent.class);
		when(persistent.isPolymorphicallyMapped()).thenReturn(Boolean.FALSE);
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
		assertTrue(result.contains("public enum Status"));
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

	@Test
	void replaceGeneratedRemovesUnreferencedModulesAndWritesGenerationFiles() throws Exception {
		Path temp = Files.createTempDirectory("odg-replace");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		Path salesDomain = generatedSrc.resolve("modules/sales/domain");
		Path obsoleteDomain = generatedSrc.resolve("modules/obsolete/domain");
		Path hrModule = generatedSrc.resolve("modules/hr");
		Files.createDirectories(salesDomain);
		Files.createDirectories(obsoleteDomain);
		Files.createDirectories(hrModule);
		Files.writeString(salesDomain.resolve("Old.java"), "old");
		Files.writeString(obsoleteDomain.resolve("Old.java"), "old");

		Files.createDirectories(generatedTest);
		Files.writeString(generatedTest.resolve("OldTest.java"), "old-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		Path newDomainFile = salesDomain.resolve("New.java");
		Path newGeneratedTestFile = generatedTest.resolve("modules/sales/domain/NewTest.java");
		gen.generation.put(newDomainFile, "public class New {}\n");
		gen.generation.put(newGeneratedTestFile, "public class NewTest {}\n");

		declaredMethod("replaceGenerated", java.util.List.class).invoke(gen, java.util.List.of("sales", "hr"));

		assertFalse(Files.exists(obsoleteDomain.getParent()), "Unreferenced module directory should be deleted");
		assertTrue(Files.exists(generatedSrc.resolve("modules/hr/domain")), "Referenced module should have domain directory created");
		assertFalse(Files.exists(salesDomain.resolve("Old.java")), "Old generated source files should be removed");
		assertTrue(Files.exists(newDomainFile), "New generated source file should be written");
		assertFalse(Files.exists(generatedTest.resolve("OldTest.java")), "Generated test directory should be replaced");
		assertTrue(Files.exists(newGeneratedTestFile), "New generated test file should be written");
		assertTrue(gen.generation.isEmpty(), "Generation map should be drained after writing files");
	}

	@Test
	void replaceGeneratedHandlesMissingModulesDirectoryAndCreatesParentsForGenerationEntries() throws Exception {
		Path temp = Files.createTempDirectory("odg-replace-empty");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		Path outFile = generatedSrc.resolve("modules/newmod/domain/Created.java");
		gen.generation.put(outFile, "public class Created {}\n");

		declaredMethod("replaceGenerated", java.util.List.class).invoke(gen, java.util.List.of("newmod"));

		assertTrue(Files.exists(outFile), "replaceGenerated should create parent directories for generation entries");
		assertTrue(gen.generation.isEmpty(), "Generation map should be empty after write");
	}

	@Test
	void generateWithEmptyRepositoryCompletesWithoutSideEffects() throws Exception {
		OverridableDomainGenerator gen = generator();
		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		when(repo.getAllVanillaModuleNames()).thenReturn(java.util.List.of());
		when(repo.getAllCustomerNames()).thenReturn(java.util.List.of());

		gen.generate();

		assertTrue(gen.generation.isEmpty());
	}

	@Test
	void populateDataStructuresWithEmptyRepositoryLeavesMapsEmpty() throws Exception {
		OverridableDomainGenerator gen = generator();
		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		when(repo.getAllVanillaModuleNames()).thenReturn(java.util.List.of());
		when(repo.getAllCustomerNames()).thenReturn(java.util.List.of());

		declaredMethod("populateDataStructures").invoke(gen);

		Field vanillaClassesField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaClassesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, ?> vanillaClasses = (TreeMap<String, ?>) vanillaClassesField.get(gen);
		assertTrue(vanillaClasses.isEmpty());

		Field propertyLengthsField = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		propertyLengthsField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, ?> propertyLengths = (TreeMap<String, ?>) propertyLengthsField.get(gen);
		assertTrue(propertyLengths.isEmpty());
	}

	@Test
	void generateVanillaWithEmptyModuleCreatesMappingEntry() throws Exception {
		Path temp = Files.createTempDirectory("odg-generate-vanilla");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(java.util.Map.of());

		declaredMethod("generateVanilla", Module.class).invoke(gen, module);

		Path mappingPath = generatedSrc.resolve("modules/sales/domain/sales_orm.hbm.xml");
		assertTrue(gen.generation.containsKey(mappingPath));
	}

	@Test
	void generateOverriddenWithNoModulesCreatesCustomerOrmEntry() throws Exception {
		Path temp = Files.createTempDirectory("odg-generate-overridden");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		when(customer.getModules()).thenReturn(java.util.List.of());

		Path customerModulesPath = generatedSrc.resolve("customers/acme/modules");
		Files.createDirectories(customerModulesPath);

		declaredMethod("generateOverridden", Customer.class, String.class)
				.invoke(gen, customer, normalisedPath(customerModulesPath));

		Path mappingPath = customerModulesPath.resolve("orm.hbm.xml");
		assertTrue(gen.generation.containsKey(mappingPath));
	}

	@Test
	void generateWithVanillaModuleAndCustomerModulesDirectoryWritesMappings() throws Exception {
		Path temp = Files.createTempDirectory("odg-generate-full");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(java.util.Map.of());

		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		when(customer.getModules()).thenReturn(java.util.List.of());

		when(repo.getAllVanillaModuleNames()).thenReturn(java.util.List.of("sales"));
		when(repo.getModule(null, "sales")).thenReturn(module);
		when(repo.getAllCustomerNames()).thenReturn(java.util.List.of("acme"));
		when(repo.getCustomer("acme")).thenReturn(customer);

		Path customerModulesPath = generatedSrc.resolve("customers/acme/modules");
		Files.createDirectories(customerModulesPath);

		gen.generate();

		assertTrue(Files.exists(generatedSrc.resolve("modules/sales/domain/sales_orm.hbm.xml")));
		assertTrue(Files.exists(customerModulesPath.resolve("orm.hbm.xml")));
		assertTrue(gen.generation.isEmpty(), "Generation map should be written and drained");
	}

	@Test
	void generateOverriddenWithOverrideWithoutExtraPropertiesCreatesOnlyOrmEntry() throws Exception {
		Path temp = Files.createTempDirectory("odg-generate-overridden-existing");
		Path generatedSrc = temp.resolve("generated-src");
		Path generatedTest = temp.resolve("generated-test");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(generatedTest),
				null);

		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		Text vanillaCode = new Text();
		vanillaCode.setName("code");
		vanillaCode.setDisplayName("Code");

		DocumentImpl vanillaDocument = new DocumentImpl();
		vanillaDocument.setName("Order");
		vanillaDocument.setOwningModuleName("sales");
		vanillaDocument.setPersistent(persistentOf("SALES_ORDER"));
		vanillaDocument.putAttribute(vanillaCode);

		Text overrideCode = new Text();
		overrideCode.setName("code");
		overrideCode.setDisplayName("Code");

		DocumentImpl overrideDocument = new DocumentImpl();
		overrideDocument.setName("Order");
		overrideDocument.setOwningModuleName("sales");
		overrideDocument.setPersistent(persistentOf("SALES_ORDER"));
		overrideDocument.putAttribute(overrideCode);

		Module.DocumentRef documentRef = new Module.DocumentRef();
		documentRef.setOwningModuleName("sales");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(java.util.Map.of("Order", documentRef));

		CustomerImpl customer = new CustomerImpl();
		customer.setName("acme");
		customer.getModuleEntries().put("sales", null);

		when(module.getDocument(null, "Order")).thenReturn(vanillaDocument);
		when(module.getDocument(customer, "Order")).thenReturn(overrideDocument);
		when(repo.getModule(null, "sales")).thenReturn(module);
		when(repo.getModule(customer, "sales")).thenReturn(module);
		when(repo.getAllVanillaModuleNames()).thenReturn(java.util.List.of("sales"));
		when(repo.getAllCustomerNames()).thenReturn(java.util.List.of());
		when(repo.vtable("acme", "modules/sales/Order")).thenReturn("customers/acme/modules/sales/Order");

		Path customerModulesPath = generatedSrc.resolve("customers/acme/modules");
		Files.createDirectories(customerModulesPath);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(repo);
		try {
			declaredMethod("populateDataStructures").invoke(gen);
			declaredMethod("generateOverridden", Customer.class, String.class)
					.invoke(gen, customer, normalisedPath(customerModulesPath));
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}

		assertTrue(gen.generation.containsKey(customerModulesPath.resolve("orm.hbm.xml")));
		assertFalse(gen.generation.containsKey(generatedSrc.resolve("customers/acme/modules/sales/domain/OrderExt.java")));
	}

	@Test
	void populateDataStructuresWithCustomerOverrideMarksVanillaClassAbstract() throws Exception {
		Path temp = Files.createTempDirectory("odg-populate-override");
		Path generatedSrc = temp.resolve("generated-src");

		OverridableDomainGenerator gen = new OverridableDomainGenerator(false,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				"",
				normalisedPath(generatedSrc),
				"",
				normalisedPath(temp.resolve("generated-test")),
				null);

		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		Text vanillaCode = new Text();
		vanillaCode.setName("code");
		vanillaCode.setDisplayName("Code");
		Text vanillaDescription = new Text();
		vanillaDescription.setName("description");
		vanillaDescription.setDisplayName("Description");

		DocumentImpl vanillaDocument = new DocumentImpl();
		vanillaDocument.setName("Order");
		vanillaDocument.setOwningModuleName("sales");
		vanillaDocument.setPersistent(persistentOf("SALES_ORDER"));
		vanillaDocument.putAttribute(vanillaCode);
		vanillaDocument.putAttribute(vanillaDescription);

		Text overrideCode = new Text();
		overrideCode.setName("code");
		overrideCode.setDisplayName("Code");

		DocumentImpl overrideDocument = new DocumentImpl();
		overrideDocument.setName("Order");
		overrideDocument.setOwningModuleName("sales");
		overrideDocument.setPersistent(persistentOf("SALES_ORDER"));
		overrideDocument.putAttribute(overrideCode);

		Module.DocumentRef documentRef = new Module.DocumentRef();
		documentRef.setOwningModuleName("sales");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(java.util.Map.of("Order", documentRef));

		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		when(customer.getModules()).thenReturn(java.util.List.of(module));

		when(module.getDocument(null, "Order")).thenReturn(vanillaDocument);
		when(module.getDocument(customer, "Order")).thenReturn(overrideDocument);
		when(repo.getAllVanillaModuleNames()).thenReturn(java.util.List.of("sales"));
		when(repo.getModule(null, "sales")).thenReturn(module);
		when(repo.getAllCustomerNames()).thenReturn(java.util.List.of("acme"));
		when(repo.getCustomer("acme")).thenReturn(customer);
		when(repo.vtable("acme", "modules/sales/Order")).thenReturn("customers/acme/modules/sales/Order");

		Files.createDirectories(generatedSrc.resolve("customers/acme/modules"));

		declaredMethod("populateDataStructures").invoke(gen);

		Field vanillaClassesField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaClassesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, ?>> vanillaClasses = (TreeMap<String, TreeMap<String, ?>>) vanillaClassesField.get(gen);
		Object domainClass = vanillaClasses.get("sales").get("Order");

		Field attributesField = domainClass.getClass().getDeclaredField("attributes");
		attributesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, ?> attributes = (TreeMap<String, ?>) attributesField.get(domainClass);
		assertTrue(attributes.containsKey("code"));
		assertFalse(attributes.containsKey("description"));

		Field isAbstractField = domainClass.getClass().getDeclaredField("isAbstract");
		isAbstractField.setAccessible(true);
		assertEquals(Boolean.TRUE, isAbstractField.get(domainClass));

		Field propertyLengthsField = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		propertyLengthsField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Integer>> propertyLengths = (TreeMap<String, TreeMap<String, Integer>>) propertyLengthsField.get(gen);
		assertTrue(propertyLengths.containsKey("SALES_ORDER"));
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
		// This uses an unescaped { which makes containsSkyveExpressions return true
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
		// The {el:...} format makes isSkyveExpression return true
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
	void validateDocumentAttributeNamesMssqlReservedWordThrows() {
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
	void validateDocumentAttributeNamesMssql2016ReservedWordThrows() {
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
	void validateDocumentAttributeNamesMysql5ReservedWordThrows() {
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
	void validateDocumentAttributeNamesMysql5ByteCharsetReservedWordThrows() {
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
	void validateDocumentAttributeNamesMysql8ReservedWordThrows() {
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
	void validateDocumentAttributeNamesMysql8ByteReservedWordThrows() {
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
	void validateDocumentAttributeNamesPostgresqlReservedWordThrows() {
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
	void validateDocumentAttributeNamesH2ValidOnOtherDialect() {
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
		assertNull(declaredMethod("validateDocumentAttributeNames", Document.class).invoke(gen, doc),
				"void method invocation should return null for transient doc");
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

	@Test
	void populatePropertyLengthsWithGeneratedEnumerationUsesLongestCode() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("EnumDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ENUM_TABLE"));

		Enumeration enumeration = new Enumeration();
		enumeration.setName("status");
		enumeration.setDisplayName("Status");

		EnumeratedValue shortValue = new EnumeratedValue();
		shortValue.setCode("A");
		EnumeratedValue longValue = new EnumeratedValue();
		longValue.setCode("LONGCODE");
		enumeration.getXmlValues().add(shortValue);
		enumeration.getXmlValues().add(longValue);
		doc.putAttribute(enumeration);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		m.invoke(gen, null, module, doc, null);

		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Integer>> propLengths =
				(TreeMap<String, TreeMap<String, Integer>>) f.get(gen);
		assertEquals(Integer.valueOf(8), propLengths.get("ENUM_TABLE").get("status"));
	}

	@Test
	void populatePropertyLengthsWithHandCodedEnumerationUsesLongestCode() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("HandEnumDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("HAND_ENUM_TABLE"));

		Enumeration enumeration = new Enumeration();
		enumeration.setName("status");
		enumeration.setDisplayName("Status");
		enumeration.setXmlImplementingEnumClassName(HandCodedLengthEnum.class.getName());
		doc.putAttribute(enumeration);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		m.invoke(gen, null, module, doc, null);

		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Integer>> propLengths =
				(TreeMap<String, TreeMap<String, Integer>>) f.get(gen);
		assertEquals(Integer.valueOf(12), propLengths.get("HAND_ENUM_TABLE").get("status"));
	}

	@Test
	void populatePropertyLengthsWithMappedBaseInheritsDerivedIdentifier() throws Exception {
		OverridableDomainGenerator gen = generator();
		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseDoc");
		baseDoc.setOwningModuleName("testMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setStrategy(ExtensionStrategy.mapped);
		baseDoc.setPersistent(basePersistent);

		Text baseField = new Text();
		baseField.setName("baseText");
		baseField.setDisplayName("Base Text");
		baseField.setLength(50);
		baseDoc.putAttribute(baseField);

		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildDoc");
		childDoc.setOwningModuleName("testMod");
		childDoc.setPersistent(persistentOf("CHILD_TABLE"));

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseDoc");
		childDoc.setExtends(inherits);

		Text childField = new Text();
		childField.setName("childText");
		childField.setDisplayName("Child Text");
		childField.setLength(100);
		childDoc.putAttribute(childField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDoc);
		when(repo.getModule(null, "testMod")).thenReturn(module);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populatePropertyLengths",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, String.class);
		m.setAccessible(true);
		m.invoke(gen, null, module, childDoc, null);

		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Integer>> propLengths =
				(TreeMap<String, TreeMap<String, Integer>>) f.get(gen);
		assertEquals(Integer.valueOf(50), propLengths.get("CHILD_TABLE").get("baseText"));
		assertEquals(Integer.valueOf(100), propLengths.get("CHILD_TABLE").get("childText"));
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
		assertNull(m.invoke(gen, module, doc, null), "void populateModocDerivations should return null");
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
		assertNull(m.invoke(gen, module, doc, null), "void populateModocDerivations with inherits should return null");
	}

	@Test
	void populateModocDerivationsWithConflictingStrategyThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ConflictingDoc");
		doc.setOwningModuleName("testMod");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.single);
		persistent.setName("CONF_TABLE");
		doc.setPersistent(persistent);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		InvocationTargetException ex = assertThrows(InvocationTargetException.class,
				() -> m.invoke(gen, module, doc, ExtensionStrategy.joined));
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	void populateModocDerivationsWithMissingBaseDocumentThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DerivedMissingBase");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("DERIVED_TABLE"));

		Extends inherits = new Extends();
		inherits.setDocumentName("MissingBase");
		doc.setExtends(inherits);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "MissingBase")).thenReturn(null);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		InvocationTargetException ex = assertThrows(InvocationTargetException.class,
				() -> m.invoke(gen, module, doc, null));
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	void populateModocDerivationsWithJoinedStrategySameIdentifierThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseJoined");
		baseDoc.setOwningModuleName("testMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setStrategy(ExtensionStrategy.joined);
		basePersistent.setName("SAME_TABLE");
		baseDoc.setPersistent(basePersistent);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ChildJoined");
		doc.setOwningModuleName("testMod");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.joined);
		persistent.setName("SAME_TABLE");
		doc.setPersistent(persistent);

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseJoined");
		doc.setExtends(inherits);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseJoined")).thenReturn(baseDoc);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		InvocationTargetException ex = assertThrows(InvocationTargetException.class,
				() -> m.invoke(gen, module, doc, null));
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	void populateModocDerivationsWithSingleStrategyDifferentIdentifierThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseSingle");
		baseDoc.setOwningModuleName("testMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setStrategy(ExtensionStrategy.single);
		basePersistent.setName("BASE_TABLE");
		baseDoc.setPersistent(basePersistent);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ChildSingle");
		doc.setOwningModuleName("testMod");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.single);
		persistent.setName("CHILD_TABLE");
		doc.setPersistent(persistent);

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseSingle");
		doc.setExtends(inherits);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseSingle")).thenReturn(baseDoc);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		InvocationTargetException ex = assertThrows(InvocationTargetException.class,
				() -> m.invoke(gen, module, doc, null));
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	void populateModocDerivationsWithValidSingleStrategyRegistersDerivation() throws Exception {
		OverridableDomainGenerator gen = generator();
		ProvidedRepository repo = mock(ProvidedRepository.class);
		gen.repository = repo;

		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseSingle");
		baseDoc.setOwningModuleName("testMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setStrategy(ExtensionStrategy.single);
		basePersistent.setName("BASE_TABLE");
		baseDoc.setPersistent(basePersistent);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ChildSingle");
		doc.setOwningModuleName("testMod");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.single);
		persistent.setName("BASE_TABLE");
		doc.setPersistent(persistent);

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseSingle");
		doc.setExtends(inherits);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseSingle")).thenReturn(baseDoc);
		when(repo.getModule(null, "testMod")).thenReturn(module);

		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"populateModocDerivations",
				Module.class, Document.class, ExtensionStrategy.class);
		m.setAccessible(true);
		assertNull(m.invoke(gen, module, doc, null));

		Field field = OverridableDomainGenerator.class.getDeclaredField("modocDerivations");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Document>> derivations =
				(TreeMap<String, TreeMap<String, Document>>) field.get(gen);
		assertSame(doc, derivations.get("testMod.BaseSingle").get("testMod.ChildSingle"));
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
		assertNull(m.invoke(gen, "testMod", "modules/testMod/domain", "modules/testMod", doc, "ActionDoc", null),
				"void generateActionTests should return null when skipping");
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
		assertNull(m.invoke(gen, "testMod", "modules/testMod/domain", "modules/testMod", doc, "DebugActionDoc", null),
				"void generateActionTests should return null for debug mode");
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

	// ----- generateJava with deprecated association (covers addReference deprecated branch) ---

	@Test
	void generateJavaWithDeprecatedAssociationNew() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("DepRefDoc");
		refDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("DepAssocOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("depRef");
		assoc.setDisplayName("Deprecated Ref");
		assoc.setDocumentName("DepRefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);
		assoc.setDeprecated(true);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DepRefDoc")).thenReturn(refDoc);

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
		modClasses.put("DepRefDoc", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "DepAssocOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@Deprecated"), "Should have @Deprecated annotation");
		assertTrue(java.contains("getDepRef"), "Should have getter for deprecated association");
	}

	// ----- generateJava with aggregation collection (covers Collection branch in addReference) ---

	@Test
	void generateJavaWithAggregationCollection() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl itemDoc = new DocumentImpl();
		itemDoc.setName("LineItem");
		itemDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OrderDoc");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.CollectionImpl coll =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		coll.setName("lineItems");
		coll.setDisplayName("Line Items");
		coll.setDocumentName("LineItem");
		coll.setType(org.skyve.metadata.model.document.Collection.CollectionType.aggregation);

		ownerDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "LineItem")).thenReturn(itemDoc);

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
		modClasses.put("LineItem", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "OrderDoc", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("List<LineItem>"), "Should have list of line items");
		assertTrue(java.contains("getLineItems"), "Should have getter");
		assertTrue(java.contains("addLineItemsElement"), "Should have add method");
		assertTrue(java.contains("removeLineItemsElement"), "Should have remove method");
	}

	// ----- generateJava with deprecated aggregation collection (covers deprecated Collection path) ---

	@Test
	void generateJavaWithDeprecatedAggregationCollection() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl itemDoc = new DocumentImpl();
		itemDoc.setName("DepItem");
		itemDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("DepCollOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.CollectionImpl coll =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		coll.setName("depItems");
		coll.setDisplayName("Deprecated Items");
		coll.setDocumentName("DepItem");
		coll.setType(org.skyve.metadata.model.document.Collection.CollectionType.aggregation);
		coll.setDeprecated(true);

		ownerDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DepItem")).thenReturn(itemDoc);

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
		modClasses.put("DepItem", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "DepCollOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("@Deprecated"), "Should have @Deprecated annotation on collection");
		assertTrue(java.contains("List<DepItem>"), "Should have list type");
	}

	// ----- generateJava with association with inverse back-ref (covers inverse != null in addReference) ---

	@Test
	void generateJavaWithAssociationHavingInverseBackRef() throws Exception {
		OverridableDomainGenerator gen = generator();

		// refDoc has an InverseMany back to ownerDoc via "orders"
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("RefWithInverse");
		refDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseMany inverseMany =
				new org.skyve.impl.metadata.model.document.InverseMany();
		inverseMany.setName("orders");
		inverseMany.setDisplayName("Orders");
		inverseMany.setDocumentName("AssocWithInverseOwner");
		inverseMany.setReferenceName("myRef"); // matches the association name below
		inverseMany.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToMany);
		refDoc.putRelation(inverseMany);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("AssocWithInverseOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("myRef");
		assoc.setDisplayName("My Ref");
		assoc.setDocumentName("RefWithInverse");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "RefWithInverse")).thenReturn(refDoc);
		when(module.getDocument(null, "AssocWithInverseOwner")).thenReturn(ownerDoc);

		// Set up vanilla classes - only include refDoc, NOT ownerDoc so documentClass==null
		// which means the condition (documentClass == null) is true and attribute gets generated
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> ctor = domainClassType.getDeclaredConstructor();
		ctor.setAccessible(true);
		Object domainClassInst1 = ctor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("RefWithInverse", domainClassInst1);
		// Do NOT put AssocWithInverseOwner so documentClass=null → attribute gets generated
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "AssocWithInverseOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getMyRef"), "Should have getter");
		// With one-to-many inverse, the setter should reference the inverse collection
		assertTrue(java.contains("getOrders"), "Setter should reference inverse collection");
	}

	// ----- generateJava with association with oneToOne inverse (covers oneToOne inverse path in addReference) ---

	@Test
	void generateJavaWithAssociationHavingOneToOneInverse() throws Exception {
		OverridableDomainGenerator gen = generator();

		// refDoc has an InverseOne back to ownerDoc via "owner"
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("RefWithOneToOneInverse");
		refDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseOne inverseOne =
				new org.skyve.impl.metadata.model.document.InverseOne();
		inverseOne.setName("owner");
		inverseOne.setDisplayName("Owner");
		inverseOne.setDocumentName("OneToOneOwner");
		inverseOne.setReferenceName("myRef");
		inverseOne.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToOne);
		refDoc.putRelation(inverseOne);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OneToOneOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("myRef");
		assoc.setDisplayName("My Ref");
		assoc.setDocumentName("RefWithOneToOneInverse");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "RefWithOneToOneInverse")).thenReturn(refDoc);
		when(module.getDocument(null, "OneToOneOwner")).thenReturn(ownerDoc);

		// Set up vanilla classes
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> ctor = domainClassType.getDeclaredConstructor();
		ctor.setAccessible(true);
		Object domainClassInst1 = ctor.newInstance();

		java.lang.reflect.Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.TreeMap<String, java.util.TreeMap<String, Object>> vanillaClasses =
				(java.util.TreeMap<String, java.util.TreeMap<String, Object>>) vanillaField.get(gen);
		java.util.TreeMap<String, Object> modClasses = new java.util.TreeMap<>();
		modClasses.put("RefWithOneToOneInverse", domainClassInst1);
		// Do NOT add OneToOneOwner so documentClass=null → attributes get generated
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "OneToOneOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("getMyRef"), "Should have getter");
		// With oneToOne inverse, the setter should reference nullMyRef
		assertTrue(java.contains("nullMyRef"), "Should have null method for oneToOne inverse");
	}

	// ----- generateJava with collection and composition type (covers trackChanges path) ---

	@Test
	void generateJavaWithTrackChangesCompositionCollection() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl itemDoc = new DocumentImpl();
		itemDoc.setName("CompositionItem");
		itemDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("CompositionOwner");
		ownerDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.CollectionImpl coll =
				new org.skyve.impl.metadata.model.document.CollectionImpl();
		coll.setName("items");
		coll.setDisplayName("Items");
		coll.setDocumentName("CompositionItem");
		coll.setType(org.skyve.metadata.model.document.Collection.CollectionType.composition);
		coll.setTrackChanges(true);

		ownerDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "CompositionItem")).thenReturn(itemDoc);

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
		modClasses.put("CompositionItem", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, ownerDoc, contents,
						"modules.testMod.domain", "CompositionOwner", null, Boolean.FALSE);

		String java = contents.toString();
		assertTrue(java.contains("ChangeTrackingArrayList"), "Should use ChangeTrackingArrayList for trackChanges collection");
		assertTrue(java.contains("getItems"), "Should have getter");
	}

	// ----- generateJava with text attribute with bizKeyMethodCode ---

	@Test
	void generateJavaWithBizKeyMethodCode() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BizKeyDoc");
		doc.setOwningModuleName("testMod");
		doc.setBizKeyMethodCode("return getName();");

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
		assertTrue(java.contains("getBizKey"), "Should contain getBizKey method");
		assertTrue(java.contains("return getName()"), "Should contain the bizKey method code");
	}

	// ----- generateJava with overridden document (extension class) ---

	@Test
	void generateJavaForOverriddenDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseDoc");
		baseDoc.setOwningModuleName("testMod");

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ExtDoc");
		doc.setOwningModuleName("testMod");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDoc);

		// Set up vanilla class so the overridden=true path sees a base class
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
		modClasses.put("ExtDoc", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"modules.testMod.domain", "ExtDoc", "BaseDoc", Boolean.FALSE);

		String java = contents.toString();
		// overridden=false but baseDocumentName set means this doc extends another doc
		assertTrue(java.contains("class ExtDoc"), "Should contain class name");
		assertTrue(java.contains("extends BaseDoc"), "Should extend base document class");
	}

	// ----- generateJava with persistent document and overridden=true (extension class path) ---

	@Test
	void generateJavaWithOverriddenTrueExtensionClass() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("CustExtDoc");
		doc.setOwningModuleName("testMod");
		// No persistent = transient

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		// Set up vanilla class so overridden path has a documentClass reference
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
		modClasses.put("CustExtDoc", domainClassInst);
		vanillaClasses.put("testMod", modClasses);

		StringBuilder contents = new StringBuilder();
		// overridden = Boolean.TRUE means this is an extension/override class
		declaredMethod("generateJava",
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, StringBuilder.class,
				String.class, String.class, String.class, boolean.class)
				.invoke(gen, null, module, doc, contents,
						"customers.testCust.modules.testMod.domain", "CustExtDoc", null, Boolean.TRUE);

		String java = contents.toString();
		assertTrue(java.contains("class CustExtDoc"), "Should contain class name");
		// overridden=true with last param Boolean.TRUE generates an extension class
		assertTrue(java.contains("CustExtDoc"), "Should contain document class name");
	}

	// ----- generateAttributeMappings with child collection ------------------

	private static void setupPersistentPropertyLengths(OverridableDomainGenerator gen, String persistentId)
			throws Exception {
		Field pplField = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		pplField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Integer>> ppl =
				(TreeMap<String, TreeMap<String, Integer>>) pplField.get(gen);
		ppl.put(persistentId, new TreeMap<>());
	}

	private enum HandCodedLengthEnum implements org.skyve.domain.types.Enumeration {
		SHORT("A"),
		LONG("LONGEST_CODE");

		private final String code;

		HandCodedLengthEnum(String code) {
			this.code = code;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return code;
		}

		@Override
		public DomainValue toDomainValue() {
			return new DomainValue(code);
		}

		@SuppressWarnings("unused")
		public static java.util.List<DomainValue> toDomainValues() {
			return java.util.List.of(SHORT.toDomainValue(), LONG.toDomainValue());
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsChildCollectionGeneratesBagElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_TABLE");

		// Referenced doc with a concrete Persistent (non-null name → isPolymorphicallyMapped = false)
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("ItemDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("ITEM_TABLE");
		refDoc.setPersistent(refPersistent);

		// Build the collection attribute
		CollectionImpl col = new CollectionImpl();
		col.setName("items");
		col.setDocumentName("ItemDoc");
		col.setType(CollectionType.child);

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		// Module mock
		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ItemDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerDoc", false, "");

		String xml = contents.toString();
		assertTrue(xml.contains("<bag name=\"items\""), "Expected child bag element, got: " + xml);
		assertTrue(xml.contains("cascade=\"all-delete-orphan\""), "Expected child cascade, got: " + xml);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsAggregationCollectionGeneratesManyToMany() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_TABLE2");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("TagDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("TAG_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("tags");
		col.setDocumentName("TagDoc");
		col.setType(CollectionType.aggregation);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc2");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TagDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_TABLE2");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerDoc2", false, "");

		String xml = contents.toString();
		assertTrue(xml.contains("<bag name=\"tags\"") || xml.contains("<list name=\"tags\""),
				"Expected bag/list element, got: " + xml);
		assertTrue(xml.contains("cascade=\"persist,save-update,refresh,merge\""),
				"Expected aggregation cascade, got: " + xml);
		assertTrue(xml.contains("<many-to-many"), "Expected many-to-many, got: " + xml);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsCompositionCollectionGeneratesComposeBag() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_TABLE3");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("LineDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("LINE_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("lines");
		col.setDocumentName("LineDoc");
		col.setType(CollectionType.composition);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc3");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "LineDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_TABLE3");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerDoc3", false, "");

		String xml = contents.toString();
		assertTrue(xml.contains("cascade=\"all-delete-orphan\""), "Expected compose cascade, got: " + xml);
		assertTrue(xml.contains("<many-to-many"), "Expected many-to-many, got: " + xml);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsOrderedCompositionGeneratesListElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_TABLE4");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("OrderedItem");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("ORDERED_ITEM");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("orderedItems");
		col.setDocumentName("OrderedItem");
		col.setType(CollectionType.composition);
		col.setOrdered(Boolean.TRUE);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc4");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "OrderedItem")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_TABLE4");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerDoc4", false, "");

		String xml = contents.toString();
		assertTrue(xml.contains("<list name=\"orderedItems\""), "Expected ordered list element, got: " + xml);
		assertTrue(xml.contains("<list-index"), "Expected list-index for ordinal, got: " + xml);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsTransientCollectionIsSkipped() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_TABLE5");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("TransientItem");
		refDoc.setOwningModuleName("testMod");

		CollectionImpl col = new CollectionImpl();
		col.setName("transientItems");
		col.setDocumentName("TransientItem");
		col.setType(CollectionType.aggregation);
		col.setPersistent(false); // transient: isPersistent() = false

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc5");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TransientItem")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_TABLE5");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerDoc5", false, "");

		// Transient collection should be skipped - no bag/list elements
		String xml = contents.toString();
		assertFalse(xml.contains("<bag"), "Transient collection should be skipped, got: " + xml);
		assertFalse(xml.contains("<list"), "Transient collection should be skipped, got: " + xml);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsAssociationGeneratesManyToOne() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_ASSOC_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("RefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("REF_TABLE");
		refDoc.setPersistent(refPersistent);

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("myRef");
		assoc.setDocumentName("RefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerAssocDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "RefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_ASSOC_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "OwnerAssocDoc", false, "");

		String xml = contents.toString();
		assertTrue(xml.contains("<many-to-one name=\"myRef\""), "Expected many-to-one for association, got: " + xml);
	}

	// ----- generateORM basic tests ------------------------------------------

	private static void injectRepository(OverridableDomainGenerator gen, ProvidedRepository repo) throws Exception {
		Field repoField = DomainGenerator.class.getDeclaredField("repository");
		repoField.setAccessible(true);
		repoField.set(gen, repo);
	}

	private static void setupModuleDocumentVanillaClasses(OverridableDomainGenerator gen,
			String moduleName,
			java.util.Map<String, ?> documents) throws Exception {
		Field field = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Object>> value =
				(TreeMap<String, TreeMap<String, Object>>) field.get(gen);
		value.put(moduleName, new TreeMap<>(documents));
	}

	@Test
	@SuppressWarnings("boxing")
	void generateORMWithNoPersistentDocumentGeneratesNothing() throws Exception {
		OverridableDomainGenerator gen = generator();

		ProvidedRepository repo = mock(ProvidedRepository.class);
		injectRepository(gen, repo);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientDoc");
		doc.setOwningModuleName("testMod");
		// persistent is null by default → transient document

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();

		declaredMethod("generateORM",
				StringBuilder.class, Module.class, Document.class,
				String.class, boolean.class, boolean.class,
				org.skyve.metadata.customer.Customer.class,
				StringBuilder.class, String.class)
				.invoke(gen, contents, module, doc, "modules.testMod.domain.", false, true,
						null, filterDefs, "");

		// No persistent → no ORM class element
		assertFalse(contents.toString().contains("<class"), "Transient doc should generate no ORM class, got: " + contents);
	}

	@Test
	void generateOverriddenOrmWithCustomerOnlyDocumentUsesCustomerPackagePrefix() throws Exception {
		OverridableDomainGenerator gen = generator();

		ProvidedRepository repo = mock(ProvidedRepository.class);
		injectRepository(gen, repo);
		setupModuleDocumentVanillaClasses(gen, "sales", java.util.Map.of());
		setupPersistentPropertyLengths(gen, "CUSTOM_TABLE");

		CustomerImpl customer = new CustomerImpl();
		customer.setName("acme");
		customer.getModuleEntries().put("sales", null);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("CustomOnly");
		doc.setOwningModuleName("sales");
		doc.setPersistent(persistentOf("CUSTOM_TABLE"));

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(repo.getModule(customer, "sales")).thenReturn(module);
		when(repo.vtable("acme", "modules/sales/CustomOnly"))
				.thenReturn("customers/acme/modules/sales/CustomOnly");
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, doc)).thenReturn(null);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(repo);
		try {
			StringBuilder contents = new StringBuilder();
			StringBuilder filterDefs = new StringBuilder();
			declaredMethod("generateOverriddenORM",
					StringBuilder.class,
					Customer.class,
					Module.class,
					Document.class,
					StringBuilder.class)
					.invoke(gen, contents, customer, module, doc, filterDefs);
			assertTrue(contents.toString().contains("customers.acme.modules.sales.domain.CustomOnly"),
					"Expected customer package prefix in ORM output, got: " + contents);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	void generateOverriddenOrmWithChildCollectionRecursesToChildDocument() throws Exception {
		OverridableDomainGenerator gen = generator();

		ProvidedRepository repo = mock(ProvidedRepository.class);
		injectRepository(gen, repo);
		setupModuleDocumentVanillaClasses(gen, "sales", java.util.Map.of());
		setupPersistentPropertyLengths(gen, "OWNER_TABLE");
		setupPersistentPropertyLengths(gen, "CHILD_TABLE");

		CustomerImpl customer = new CustomerImpl();
		customer.setName("acme");
		customer.getModuleEntries().put("sales", null);

		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildDoc");
		childDoc.setOwningModuleName("sales");
		childDoc.setPersistent(persistentOf("CHILD_TABLE"));

		CollectionImpl childCollection = new CollectionImpl();
		childCollection.setName("children");
		childCollection.setDocumentName("ChildDoc");
		childCollection.setType(CollectionType.child);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("sales");
		ownerDoc.setPersistent(persistentOf("OWNER_TABLE"));
		ownerDoc.putRelation(childCollection);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocument(customer, "ChildDoc")).thenReturn(childDoc);
		when(repo.getModule(customer, "sales")).thenReturn(module);
		when(repo.vtable("acme", "modules/sales/OwnerDoc")).thenReturn(null);
		when(repo.vtable("acme", "modules/sales/ChildDoc")).thenReturn(null);
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, ownerDoc)).thenReturn(null);
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, childDoc)).thenReturn(null);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(repo);
		try {
			StringBuilder contents = new StringBuilder();
			StringBuilder filterDefs = new StringBuilder();
			declaredMethod("generateOverriddenORM",
					StringBuilder.class,
					Customer.class,
					Module.class,
					Document.class,
					StringBuilder.class)
					.invoke(gen, contents, customer, module, ownerDoc, filterDefs);

			Field visitedField = OverridableDomainGenerator.class.getDeclaredField("visitedOverriddenORMDocumentsPerCustomer");
			visitedField.setAccessible(true);
			@SuppressWarnings("unchecked")
			Set<String> visited = (Set<String>) visitedField.get(gen);
			assertTrue(visited.contains("sales.OwnerDoc"));
			assertTrue(visited.contains("sales.ChildDoc"));
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	void generateOverriddenOrmWithTransientExportedReferenceThrows() throws Exception {
		OverridableDomainGenerator gen = generator();

		ProvidedRepository repo = mock(ProvidedRepository.class);
		injectRepository(gen, repo);
		setupModuleDocumentVanillaClasses(gen, "sales", java.util.Map.of());
		setupPersistentPropertyLengths(gen, "OWNER_TABLE_TRANSIENT_REF");

		CustomerImpl customer = new CustomerImpl();
		customer.setName("acme");
		customer.getModuleEntries().put("sales", null);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("sales");
		ownerDoc.setPersistent(persistentOf("OWNER_TABLE_TRANSIENT_REF"));

		DocumentImpl transientRefDoc = new DocumentImpl();
		transientRefDoc.setName("TransientRef");
		transientRefDoc.setOwningModuleName("sales");

		org.skyve.impl.metadata.customer.ExportedReference ref = new org.skyve.impl.metadata.customer.ExportedReference();
		ref.setModuleName("sales");
		ref.setDocumentName("TransientRef");

		Field exportedField = CustomerImpl.class.getDeclaredField("exportedReferences");
		exportedField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.Map<String, java.util.List<org.skyve.impl.metadata.customer.ExportedReference>> exported =
				(java.util.Map<String, java.util.List<org.skyve.impl.metadata.customer.ExportedReference>>) exportedField.get(customer);
		exported.put("sales.OwnerDoc", java.util.List.of(ref));

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");
		when(module.getDocument(customer, "TransientRef")).thenReturn(transientRefDoc);
		when(repo.getModule(customer, "sales")).thenReturn(module);
		when(repo.vtable("acme", "modules/sales/OwnerDoc")).thenReturn(null);
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, ownerDoc)).thenReturn(null);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(repo);
		try {
			StringBuilder contents = new StringBuilder();
			StringBuilder filterDefs = new StringBuilder();
			InvocationTargetException ex = assertThrows(InvocationTargetException.class,
					() -> declaredMethod("generateOverriddenORM",
							StringBuilder.class,
							Customer.class,
							Module.class,
							Document.class,
							StringBuilder.class)
							.invoke(gen, contents, customer, module, ownerDoc, filterDefs));
			assertTrue(ex.getCause() instanceof IllegalStateException);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings({ "boxing", "null" })
	void generateORMWithPersistentDocumentGeneratesClassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(null, null, null))
				.thenReturn(null);
		injectRepository(gen, repo);

		// Use a minimal setup - persistent doc with no attributes, no inherits, no parent
		Persistent persistent = new Persistent();
		persistent.setName("TEST_ORM_TABLE");

		DocumentImpl doc = new DocumentImpl();
		doc.setName("PersistDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistent);

		// isPersistable needs persistent != null and persistent.getName() != null → satisfied

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		// repository.findNearestPersistentSingleOrJoinedSuperDocument is called with the document, 
		// configure so it returns null (not an ORM subclass)
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(null, module, doc)).thenReturn(null);

		// Set up persistentPropertyLengths for this persistent ID
		setupPersistentPropertyLengths(gen, "TEST_ORM_TABLE");

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();

		declaredMethod("generateORM",
				StringBuilder.class, Module.class, Document.class,
				String.class, boolean.class, boolean.class,
				org.skyve.metadata.customer.Customer.class,
				StringBuilder.class, String.class)
				.invoke(gen, contents, module, doc, "modules.testMod.domain.", false, true,
						null, filterDefs, "");

		String xml = contents.toString();
		assertTrue(xml.contains("<class name="), "Expected class element in ORM, got: " + xml);
		assertTrue(xml.contains("TEST_ORM_TABLE"), "Expected table name, got: " + xml);
		assertTrue(xml.contains("</class>"), "Expected closing class tag, got: " + xml);
	}

	// ----- generateAttributeMappings with catalog/schema on Persistent ------

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsWithCatalogOnPersistentIncludesCatalogAttribute() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_CAT_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("CatRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("CAT_REF_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("catItems");
		col.setDocumentName("CatRefDoc");
		col.setType(CollectionType.aggregation);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("CatOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "CatRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_CAT_TABLE");
		ownerPersistent.setCatalog("mydb");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "CatOwnerDoc", false, "");

		assertTrue(contents.toString().contains("catalog=\"mydb\""),
				"Expected catalog attribute in output, got: " + contents);
	}

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsWithSchemaOnPersistentIncludesSchemaAttribute() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_SCH_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("SchRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("SCH_REF_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("schItems");
		col.setDocumentName("SchRefDoc");
		col.setType(CollectionType.aggregation);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("SchOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "SchRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_SCH_TABLE");
		ownerPersistent.setSchema("myschema");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "SchOwnerDoc", false, "");

		assertTrue(contents.toString().contains("schema=\"myschema\""),
				"Expected schema attribute in output, got: " + contents);
	}

	// ----- generateAttributeMappings with cacheName on non-child collection --

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsWithCacheNameOnAggregationIncludesCacheElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_CACHE_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("CacheRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("CACHE_REF_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("cacheItems");
		col.setDocumentName("CacheRefDoc");
		col.setType(CollectionType.aggregation);
		col.setCacheName("myRegion");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("CacheOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "CacheRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_CACHE_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "CacheOwnerDoc", false, "");

		assertTrue(contents.toString().contains("<cache usage=\"read-write\" region=\"myRegion\""),
				"Expected cache element in output, got: " + contents);
	}

	// ----- generateAttributeMappings with ownerDatabaseIndex = true ----------

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsWithOwnerDatabaseIndexGeneratesKeyWithIndex() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_IDX_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("IdxRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("IDX_REF_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("idxItems");
		col.setDocumentName("IdxRefDoc");
		col.setType(CollectionType.aggregation);
		col.setOwnerDatabaseIndex(Boolean.TRUE);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("IdxOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "IdxRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_IDX_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "IdxOwnerDoc", false, "");

		// Indexed key uses <key ...> with <column name=... index=... />
		String xml = contents.toString();
		assertTrue(xml.contains("<key foreign-key="), "Expected key element with index, got: " + xml);
		assertTrue(xml.contains("index="), "Expected index attribute on key column, got: " + xml);
	}

	// ----- generateAttributeMappings with elementDatabaseIndex = true --------

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsWithElementDatabaseIndexGeneratesManyToManyWithIndex() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_EL_IDX_TABLE");

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("ElIdxRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("EL_IDX_REF_TABLE");
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("elIdxItems");
		col.setDocumentName("ElIdxRefDoc");
		col.setType(CollectionType.aggregation);
		col.setElementDatabaseIndex(Boolean.TRUE);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("ElIdxOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ElIdxRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_EL_IDX_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "ElIdxOwnerDoc", false, "");

		// elementDatabaseIndex=true → many-to-many with <column name=... index=.../>
		String xml = contents.toString();
		assertTrue(xml.contains("<many-to-many"), "Expected many-to-many, got: " + xml);
		assertTrue(xml.contains("<column name="), "Expected column with index, got: " + xml);
	}

	// ----- generateAttributeMappings with persistent collection referencing dynamic doc ---

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsPersistentCollectionReferencingDynamicDocIsSkipped() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_DYN_TABLE");

		// Dynamic referenced doc (has a persistent but isDynamic() = true)
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("DynRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("DYN_REF_TABLE");
		refDoc.setPersistent(refPersistent);
		refDoc.setDynamism(new org.skyve.metadata.model.Dynamic());

		CollectionImpl col = new CollectionImpl();
		col.setName("dynItems");
		col.setDocumentName("DynRefDoc");
		col.setType(CollectionType.aggregation);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("DynOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DynRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_DYN_TABLE");

		StringBuilder contents = new StringBuilder();
		declaredMethod("generateAttributeMappings",
				StringBuilder.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, Document.class, Persistent.class,
				String.class, Set.class, String.class, boolean.class, String.class)
				.invoke(gen, contents, null, module, ownerDoc, ownerPersistent,
						null, new TreeSet<>(), "DynOwnerDoc", false, "");

		// Dynamic referenced document → collection is skipped
		String xml = contents.toString();
		assertFalse(xml.contains("<bag"), "Dynamic referenced doc collection should be skipped, got: " + xml);
		assertFalse(xml.contains("<list"), "Dynamic referenced doc collection should be skipped, got: " + xml);
	}

	// ----- generateAttributeMappings child + polymorphicallymapped → throw ---

	@Test
	@SuppressWarnings("boxing")
	void generateAttributeMappingsChildCollectionWithPolymorphicTargetThrows() throws Exception {
		OverridableDomainGenerator gen = generator();
		setupPersistentPropertyLengths(gen, "OWN_POLY_TABLE");

		// Polymorphically mapped: strategy=mapped, name=null
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("PolyRefDoc");
		refDoc.setOwningModuleName("testMod");
		Persistent refPersistent = new Persistent();
		// name = null (default) + strategy=mapped → isPolymorphicallyMapped() = true
		refPersistent.setStrategy(ExtensionStrategy.mapped);
		refDoc.setPersistent(refPersistent);

		CollectionImpl col = new CollectionImpl();
		col.setName("polyItems");
		col.setDocumentName("PolyRefDoc");
		col.setType(CollectionType.child);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("PolyOwnerDoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.putRelation(col);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "PolyRefDoc")).thenReturn(refDoc);

		Persistent ownerPersistent = new Persistent();
		ownerPersistent.setName("OWN_POLY_TABLE");

		Object caught = assertThrows(InvocationTargetException.class,
				() -> declaredMethod("generateAttributeMappings",
						StringBuilder.class,
						org.skyve.metadata.customer.Customer.class,
						Module.class, Document.class, Persistent.class,
						String.class, Set.class, String.class, boolean.class, String.class)
						.invoke(gen, new StringBuilder(), null, module, ownerDoc, ownerPersistent,
								null, new TreeSet<>(), "PolyOwnerDoc", false, ""));
		assertTrue(((InvocationTargetException) caught).getCause() instanceof MetaDataException,
				"Expected MetaDataException for child collection with polymorphic target");
	}

	// ----- addReference with dynamic referenced document → early return ------

	@Test
	@SuppressWarnings("boxing")
	void addReferenceWithDynamicReferencedDocumentReturnsEarlyAndGeneratesNothing() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Dynamic ref doc
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("DynRefAssocDoc");
		refDoc.setOwningModuleName("testMod");
		refDoc.setDynamism(new org.skyve.metadata.model.Dynamic());

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("dynAssoc");
		assoc.setDisplayName("Dyn Assoc");
		assoc.setDocumentName("DynRefAssocDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DynRefAssocDoc")).thenReturn(refDoc);

		Set<String> imports = new HashSet<>();
		StringBuilder attributes = new StringBuilder();
		StringBuilder methods = new StringBuilder();

		declaredMethod("addReference",
				org.skyve.metadata.model.document.Reference.class,
				boolean.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, String.class, boolean.class, String.class,
				Set.class, StringBuilder.class, StringBuilder.class)
				.invoke(gen, assoc, false, null, module, "OwnerDoc", false,
						"modules.testMod.domain", imports, attributes, methods);

		// Dynamic doc → early return: no attributes or methods generated
		assertEquals("", attributes.toString(), "Dynamic ref doc should produce no attributes");
		assertEquals("", methods.toString(), "Dynamic ref doc should produce no methods");
	}

	// ----- addReference with overriddenReference = true → early return -------

	@Test
	@SuppressWarnings("boxing")
	void addReferenceWithOverriddenReferenceFlagReturnsEarlyAndGeneratesNothing() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("OvrRefDoc");
		refDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("ovrAssoc");
		assoc.setDisplayName("Ovr Assoc");
		assoc.setDocumentName("OvrRefDoc");
		assoc.setType(org.skyve.metadata.model.document.Association.AssociationType.aggregation);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "OvrRefDoc")).thenReturn(refDoc);

		// Populate vanilla classes to satisfy the customer check further down
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();
		Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Object>> vanillaClasses =
				(TreeMap<String, TreeMap<String, Object>>) vanillaField.get(gen);
		TreeMap<String, Object> modClasses = new TreeMap<>();
		modClasses.put("OvrRefDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		Set<String> imports = new HashSet<>();
		StringBuilder attributes = new StringBuilder();
		StringBuilder methods = new StringBuilder();

		// overriddenReference = true → early return
		declaredMethod("addReference",
				org.skyve.metadata.model.document.Reference.class,
				boolean.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, String.class, boolean.class, String.class,
				Set.class, StringBuilder.class, StringBuilder.class)
				.invoke(gen, assoc, true, null, module, "OwnerDoc", false,
						"modules.testMod.domain", imports, attributes, methods);

		assertEquals("", attributes.toString(), "overriddenReference should produce no attributes");
		assertEquals("", methods.toString(), "overriddenReference should produce no methods");
	}

	// ----- addInverse with dynamic property document → early return ----------

	@Test
	@SuppressWarnings("boxing")
	void addInverseWithDynamicPropertyDocumentReturnsEarlyAndGeneratesNothing() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Dynamic property doc
		DocumentImpl propDoc = new DocumentImpl();
		propDoc.setName("DynPropDoc");
		propDoc.setOwningModuleName("testMod");
		propDoc.setDynamism(new org.skyve.metadata.model.Dynamic());

		org.skyve.impl.metadata.model.document.InverseMany inverseMany =
				new org.skyve.impl.metadata.model.document.InverseMany();
		inverseMany.setName("dynBack");
		inverseMany.setDisplayName("Dyn Back");
		inverseMany.setDocumentName("DynPropDoc");
		inverseMany.setReferenceName("owner");
		inverseMany.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToMany);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DynPropDoc")).thenReturn(propDoc);

		Set<String> imports = new HashSet<>();
		StringBuilder attributes = new StringBuilder();
		StringBuilder methods = new StringBuilder();

		declaredMethod("addInverse",
				org.skyve.impl.metadata.model.document.AbstractInverse.class,
				boolean.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, String.class, boolean.class, String.class,
				Set.class, StringBuilder.class, StringBuilder.class)
				.invoke(gen, inverseMany, false, null, module, "OwnerDoc", false,
						"modules.testMod.domain", imports, attributes, methods);

		assertEquals("", attributes.toString(), "Dynamic prop doc should produce no attributes");
		assertEquals("", methods.toString(), "Dynamic prop doc should produce no methods");
	}

	// ----- addInverse with deprecated toMany inverse -------------------------

	@Test
	@SuppressWarnings("boxing")
	void addInverseDeprecatedInverseManyGeneratesDeprecatedAnnotation() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl propDoc = new DocumentImpl();
		propDoc.setName("DepPropDoc");
		propDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseMany inverseMany =
				new org.skyve.impl.metadata.model.document.InverseMany();
		inverseMany.setName("depBackRefs");
		inverseMany.setDisplayName("Dep Back Refs");
		inverseMany.setDocumentName("DepPropDoc");
		inverseMany.setReferenceName("owner");
		inverseMany.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.oneToMany);
		inverseMany.setDeprecated(Boolean.TRUE);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "DepPropDoc")).thenReturn(propDoc);

		// Populate vanilla classes
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();
		Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Object>> vanillaClasses =
				(TreeMap<String, TreeMap<String, Object>>) vanillaField.get(gen);
		TreeMap<String, Object> modClasses = new TreeMap<>();
		modClasses.put("DepPropDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		Set<String> imports = new HashSet<>();
		StringBuilder attributes = new StringBuilder();
		StringBuilder methods = new StringBuilder();

		declaredMethod("addInverse",
				org.skyve.impl.metadata.model.document.AbstractInverse.class,
				boolean.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, String.class, boolean.class, String.class,
				Set.class, StringBuilder.class, StringBuilder.class)
				.invoke(gen, inverseMany, false, null, module, "OwnerDoc", false,
						"modules.testMod.domain", imports, attributes, methods);

		String java = attributes.toString() + methods.toString();
		assertTrue(java.contains("@Deprecated"), "Deprecated inverse should emit @Deprecated, got: " + java);
	}

	// ----- addInverse with manyToMany relationship ---------------------------

	@Test
	@SuppressWarnings("boxing")
	void addInverseManyToManyRelationshipGeneratesCorrectCollectionMethods() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl propDoc = new DocumentImpl();
		propDoc.setName("MtmPropDoc");
		propDoc.setOwningModuleName("testMod");

		org.skyve.impl.metadata.model.document.InverseMany inverseMany =
				new org.skyve.impl.metadata.model.document.InverseMany();
		inverseMany.setName("mtmBackRefs");
		inverseMany.setDisplayName("MtM Back Refs");
		inverseMany.setDocumentName("MtmPropDoc");
		inverseMany.setReferenceName("owner");
		inverseMany.setRelationship(org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship.manyToMany);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "MtmPropDoc")).thenReturn(propDoc);

		// Populate vanilla classes
		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassCtor = domainClassType.getDeclaredConstructor();
		domainClassCtor.setAccessible(true);
		Object domainClassInstance = domainClassCtor.newInstance();
		Field vanillaField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaField.setAccessible(true);
		@SuppressWarnings("unchecked")
		TreeMap<String, TreeMap<String, Object>> vanillaClasses =
				(TreeMap<String, TreeMap<String, Object>>) vanillaField.get(gen);
		TreeMap<String, Object> modClasses = new TreeMap<>();
		modClasses.put("MtmPropDoc", domainClassInstance);
		vanillaClasses.put("testMod", modClasses);

		Set<String> imports = new HashSet<>();
		StringBuilder attributes = new StringBuilder();
		StringBuilder methods = new StringBuilder();

		declaredMethod("addInverse",
				org.skyve.impl.metadata.model.document.AbstractInverse.class,
				boolean.class,
				org.skyve.metadata.customer.Customer.class,
				Module.class, String.class, boolean.class, String.class,
				Set.class, StringBuilder.class, StringBuilder.class)
				.invoke(gen, inverseMany, false, null, module, "OwnerDoc", false,
						"modules.testMod.domain", imports, attributes, methods);

		// manyToMany uses get...().add(this) instead of set....(this)
		String java = methods.toString();
		assertTrue(java.contains("getMtmBackRefsElementById"), "manyToMany should have get-by-id method, got: " + java);
		assertTrue(java.contains("addMtmBackRefsElement"), "manyToMany should have add method, got: " + java);
	}
}

package org.skyve.impl.util.json;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.notNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * Tests for {@link JSONReader} operating in dynamic mode (no user / customer required).
 * Dynamic mode is triggered whenever the first key in a JSON object is not
 * {@code "bizModule"} or {@code "class"}.
 */
@SuppressWarnings("static-method")
class JSONReaderTest {
	/** Supplies enum values used to verify typed component conversion. */
	private enum Priority {
		LOW,
		HIGH
	}

	/** Supplies a minimal nested record value for reader tests. */
	private record NestedRecord(String value) {
		// No additional behaviour.
	}

	/** Supplies all supported composite component shapes to the typed reader tests. */
	private record ExampleRecord(String name, int count, Double amount, Priority priority, NestedRecord nested,
			List<NestedRecord> children, List<NestedRecord> optionalChildren, Map<String, NestedRecord> childrenByName) {
		// Test value.
	}

	/** Verifies that reserved metadata names remain ordinary components in typed mode. */
	private record ReservedNameRecord(String bizModule, String value) {
		// Test value.
	}

	/** Holds a Skyve document nested inside a record. */
	private record DocumentRecord(Bean document) {
		// Test value.
	}

	/** Holds Skyve documents in the remaining supported typed container shapes. */
	private record DocumentContainersRecord(List<Bean> documents, Map<String, Bean> documentsByName,
			Bean[] documentArray) {
		// Test value.
	}

	/** Exercises scalar, wildcard, interface, and concrete-container conversions. */
	private record ConversionRecord(byte byteValue, short shortValue, long longValue, float floatValue, double doubleValue,
			BigInteger integerFromLong, BigInteger integerFromDecimal, BigDecimal decimalFromLong,
			BigDecimal decimalValue, char initial, boolean active, Set<Integer> numbers,
			ArrayList<String> names, LinkedHashMap<String, Integer> values, List<?> arbitrary) {
		// Test value.
	}

	/** Supplies a type variable to verify rejection of unsupported generic targets. */
	private record GenericRecord<T>(T value) {
		// Test value.
	}

	/** Supplies a parameterised target that is neither a collection nor a map. */
	private record OptionalRecord(Optional<String> value) {
		// Test value.
	}

	/** Supplies collection and map components for incompatible-shape validation. */
	private record ContainerRecord(List<String> list, Map<String, String> map) {
		// Test value.
	}

	/** Adds mutable properties used to exercise document-specific population rules. */
	private interface ReaderDocument extends PersistentBean {
		/** Assigns the document identifier. */
		void setBizId(String bizId);

		/** Returns the mutable child collection. */
		List<Object> getItems();

		/** Returns the test text property. */
		String getName();

		/** Assigns the test text property. */
		void setName(String name);
	}

	/** Groups the mocked user context and document instance used by document tests. */
	private record DocumentContext(User user, Bean document, DocumentImpl metadata) {
		// Test fixture.
	}

	/**
	 * Simple POJO used for {@link JSONReader} object-mode tests.
	 * Must be public so that {@code getDeclaredConstructor().newInstance()} works.
	 */
	public static class TestBean {
		private String name;
		private String description;
		private NestedRecord nested;
		private Bean document;
		private List<Object> values = new ArrayList<>();
		private Map<String, Object> attributes = new LinkedHashMap<>();

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getDescription() {
			return description;
		}

		public void setDescription(String description) {
			this.description = description;
		}

		/** Returns the nested record property. */
		public NestedRecord getNested() {
			return nested;
		}

		/** Assigns the nested record property. */
		public void setNested(NestedRecord nested) {
			this.nested = nested;
		}

		/** Returns the nested Skyve document property. */
		public Bean getDocument() {
			return document;
		}

		/** Assigns the nested Skyve document property. */
		public void setDocument(Bean document) {
			this.document = document;
		}

		/** Returns the mutable list populated in place by object-mode reads. */
		public List<Object> getValues() {
			return values;
		}

		/** Returns the mutable map populated in place by object-mode reads. */
		public Map<String, Object> getAttributes() {
			return attributes;
		}

		/** Returns no list so the reader's invalid Java bean contract can be verified. */
		public List<Object> getMissingValues() {
			return null;
		}

		/** Returns no map so the reader's invalid Java bean contract can be verified. */
		public Map<String, Object> getMissingAttributes() {
			return null;
		}
	}

	// ---- helper ----------------------------------------------------------

	@SuppressWarnings("unchecked")
	private static Map<Object, Object> readDynamic(String json) throws Exception {
		return (Map<Object, Object>) new JSONReader(null).read(json);
	}

	/** Creates a minimal user and metadata graph that returns a mocked document instance. */
	private static DocumentContext documentContext() throws Exception {
		return documentContext(Mockito.mock(Bean.class));
	}

	/** Creates a minimal user and metadata graph for a supplied document instance. */
	private static DocumentContext documentContext(Bean document) throws Exception {
		User user = Mockito.mock(User.class);
		Customer customer = Mockito.mock(Customer.class);
		Module module = Mockito.mock(Module.class);
		DocumentImpl documentMetaData = Mockito.mock(DocumentImpl.class);
		Mockito.when(user.getCustomer()).thenReturn(customer);
		Mockito.when(user.getDataGroupId()).thenReturn("group");
		Mockito.when(user.getId()).thenReturn("user");
		Mockito.when(customer.getName()).thenReturn("customer");
		Mockito.when(customer.getModule("test")).thenReturn(module);
		Mockito.when(module.getDocument(customer, "Document")).thenReturn(documentMetaData);
		Mockito.when(documentMetaData.newInstance(customer)).thenReturn(document);
		return new DocumentContext(user, document, documentMetaData);
	}

	// ---- simple values ---------------------------------------------------

	@Test
	void readSimpleStringValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"name\":\"alice\"}");
		assertEquals("alice", result.get("name"));
	}

	@Test
	void readIntegerValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"count\":42}");
		assertEquals(Long.valueOf(42L), result.get("count"));
	}

	@Test
	void readNegativeInteger() throws Exception {
		Map<Object, Object> result = readDynamic("{\"n\":-7}");
		assertEquals(Long.valueOf(-7L), result.get("n"));
	}

	@Test
	void readFloatingPointValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"val\":3.14}");
		Object val = result.get("val");
		assertNotNull(val);
		assertTrue(val instanceof BigDecimal);
		assertEquals(0, new BigDecimal("3.14").compareTo((BigDecimal) val));
	}

	@Test
	void readBooleanTrue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"flag\":true}");
		assertEquals(Boolean.TRUE, result.get("flag"));
	}

	@Test
	void readBooleanFalse() throws Exception {
		Map<Object, Object> result = readDynamic("{\"flag\":false}");
		assertEquals(Boolean.FALSE, result.get("flag"));
	}

	@Test
	void readNullValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"key\":null}");
		assertTrue(result.containsKey("key"));
		assertNull(result.get("key"));
	}

	// ---- compound structures ---------------------------------------------

	@Test
	void readEmptyObject() throws Exception {
		Map<Object, Object> result = readDynamic("{}");
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void readMultipleProperties() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":\"x\",\"b\":\"y\"}");
		assertEquals("x", result.get("a"));
		assertEquals("y", result.get("b"));
	}

	@Test
	void readNestedObject() throws Exception {
		Map<Object, Object> result = readDynamic("{\"outer\":{\"inner\":\"deep\"}}");
		Object outer = result.get("outer");
		assertNotNull(outer);
		assertTrue(outer instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> outerMap = (Map<Object, Object>) outer;
		assertEquals("deep", outerMap.get("inner"));
	}

	@Test
	void readArrayInObject() throws Exception {
		Map<Object, Object> result = readDynamic("{\"items\":[1,2,3]}");
		Object items = result.get("items");
		assertNotNull(items);
		assertTrue(items instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) items;
		assertEquals(3, list.size());
	}

	// ---- top-level array -------------------------------------------------

	@Test
	void readTopLevelArray() throws Exception {
		Object result = new JSONReader(null).read("[\"a\",\"b\",\"c\"]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals(3, list.size());
		assertEquals("a", list.get(0));
	}

	@Test
	void readEmptyArray() throws Exception {
		Object result = new JSONReader(null).read("[]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertTrue(list.isEmpty());
	}

	@Test
	void readArrayWithMixedTypes() throws Exception {
		Object result = new JSONReader(null).read("[\"hello\",42,true]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals("hello", list.get(0));
		assertEquals(Long.valueOf(42L), list.get(1));
		assertEquals(Boolean.TRUE, list.get(2));
	}

	/** Verifies string elements spelt like punctuation are values, not structural tokens. */
	@Test
	void readArrayOfPunctuationStrings() throws Exception {
		Map<Object, Object> result = readDynamic("{\"items\":[\"]\",\"}\",\",\",\":\"],\"next\":1}");

		assertEquals(List.of("]", "}", ",", ":"), result.get("items"));
		assertEquals(Long.valueOf(1L), result.get("next"));
	}

	/** Verifies a key spelt like an object end is a key, not the end of the object. */
	@Test
	void readObjectWithPunctuationKey() throws Exception {
		Map<Object, Object> result = readDynamic("{\"}\":1,\"]\":2}");

		assertEquals(Map.of("}", Long.valueOf(1L), "]", Long.valueOf(2L)), result);
	}

	// ---- string escapes --------------------------------------------------

	@Test
	void readStringWithNewlineEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\nb\"}");
		assertEquals("a\nb", result.get("text"));
	}

	@Test
	void readStringWithTabEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\tb\"}");
		assertEquals("a\tb", result.get("text"));
	}

	@Test
	void readStringWithQuoteEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"say \\\"hi\\\"\"}");
		assertEquals("say \"hi\"", result.get("text"));
	}

	@Test
	void readStringWithBackslashEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\\\b\"}");
		assertEquals("a\\b", result.get("text"));
	}

	@Test
	void readStringWithUnicodeEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"\\u0041\\u0042\"}");
		assertEquals("AB", result.get("text"));
	}

	// ---- order preserved -------------------------------------------------

	@Test
	void dynamicObjectPreservesInsertionOrder() throws Exception {
		Map<Object, Object> result = readDynamic("{\"z\":1,\"a\":2,\"m\":3}");
		Object[] keys = result.keySet().toArray();
		assertEquals("z", keys[0]);
		assertEquals("a", keys[1]);
		assertEquals("m", keys[2]);
	}

	// ---- whitespace tolerance -------------------------------------------

	@Test
	void readObjectWithWhitespace() throws Exception {
		Map<Object, Object> result = readDynamic("{ \"name\" : \"bob\" }");
		assertEquals("bob", result.get("name"));
	}

	// ---- negative float -------------------------------------------------

	@Test
	void readNegativeFloat() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":-2.5}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertFalse(((BigDecimal) val).compareTo(BigDecimal.ZERO) >= 0);
	}

	// ---- single-quoted strings ------------------------------------------

	@Test
	void readSingleQuotedStringValue() throws Exception {
		Object result = new JSONReader(null).read("{'name':'alice'}");
		assertTrue(result instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) result;
		assertEquals("alice", map.get("name"));
	}

	// ---- string escape characters not yet tested ------------------------

	@Test
	void readStringWithCarriageReturnEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\rb\"}");
		assertEquals("a\rb", result.get("text"));
	}

	@Test
	void readStringWithFormFeedEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\fb\"}");
		assertEquals("a\fb", result.get("text"));
	}

	@Test
	void readStringWithSlashEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\/b\"}");
		assertEquals("a/b", result.get("text"));
	}

	@Test
	void readStringWithBackspaceEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\bb\"}");
		assertEquals("a\bb", result.get("text"));
	}

	// ---- unicode escape with lowercase hex letters ----------------------

	@Test
	void readStringWithLowercaseUnicodeEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"\\u0061\\u0062\\u0063\"}");
		assertEquals("abc", result.get("text"));
	}

	// ---- scientific notation numbers ------------------------------------

	@Test
	void readScientificNotationNumbers() throws Exception {
		String[] values = {"1.5e2", "2E3", "1e-2"};
		for (String value : values) {
			Map<Object, Object> result = readDynamic("{\"v\":" + value + "}");
			Object val = result.get("v");
			assertTrue(val instanceof BigDecimal);
			assertTrue(((BigDecimal) val).compareTo(BigDecimal.ZERO) > 0);
		}
	}

	// ---- deeply nested structures ---------------------------------------

	@Test
	void readDeepNestedObjects() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":{\"b\":{\"c\":\"deep\"}}}");
		@SuppressWarnings("unchecked")
		Map<Object, Object> a = (Map<Object, Object>) result.get("a");
		@SuppressWarnings("unchecked")
		Map<Object, Object> b = (Map<Object, Object>) a.get("b");
		assertEquals("deep", b.get("c"));
	}

	@Test
	void readArrayOfObjects() throws Exception {
		Object result = new JSONReader(null).read("[{\"id\":1},{\"id\":2}]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals(2, list.size());
		@SuppressWarnings("unchecked")
		Map<Object, Object> first = (Map<Object, Object>) list.get(0);
		assertEquals(Long.valueOf(1L), first.get("id"));
	}

	@Test
	void readSingleQuoteString() throws Exception {
		Object result = new JSONReader(null).read("{'name':'value'}");
		assertTrue(result instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) result;
		assertEquals("value", map.get("name"));
	}

	@Test
	void readIntegerZero() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":0}");
		assertEquals(Long.valueOf(0L), result.get("v"));
	}

	@Test
	void readNegativeNumber() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":-99}");
		assertEquals(Long.valueOf(-99L), result.get("v"));
	}

	@Test
	void readStringWithUnicodeUpperEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":\"\\u0041\"}");
		assertEquals("A", result.get("v"));
	}

	@Test
	void readLargeInteger() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":9999999999999}");
		assertEquals(Long.valueOf(9999999999999L), result.get("v"));
	}

	@Test
	void readObjectWithMultipleProperties() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":1,\"b\":2,\"c\":3}");
		assertEquals(Long.valueOf(1L), result.get("a"));
		assertEquals(Long.valueOf(2L), result.get("b"));
		assertEquals(Long.valueOf(3L), result.get("c"));
	}

	// ---- object mode (first key is "class") ---------------------------------

	@Test
	void readObjectModeCreatesJavaBean() throws Exception {
		// Triggers JSONMode.object — loads class by name and populates via BindUtil.set
		String className = "org.skyve.impl.util.json.JSONReaderTest$TestBean";
		Object result = new JSONReader(null).read("{\"class\":\"" + className + "\",\"name\":\"Alice\",\"description\":\"test\"}");
		assertNotNull(result);
		assertTrue(result instanceof TestBean);
		assertEquals("Alice", ((TestBean) result).getName());
		assertEquals("test", ((TestBean) result).getDescription());
	}

	/** Verifies that an ordinary Java object may contain only its class discriminator. */
	@Test
	void readObjectModeSupportsClassOnlyObject() throws Exception {
		String className = "org.skyve.impl.util.json.JSONReaderTest$TestBean";

		Object result = new JSONReader(null).read("{\"class\":\"" + className + "\"}");

		assertTrue(result instanceof TestBean);
	}

	/** Verifies that class metadata is only special when it is the first object property. */
	@Test
	void readDynamicMapRetainsNonLeadingClassProperty() throws Exception {
		Map<Object, Object> result = readDynamic("{\"value\":1,\"class\":\"untrusted.Type\"}");

		assertEquals(Long.valueOf(1L), result.get("value"));
		assertEquals("untrusted.Type", result.get("class"));
	}

	/** Verifies reconstruction of a self-described record assigned to a Java bean property. */
	@Test
	void readObjectModeReconstructsRecordProperty() throws Exception {
		String beanClass = "org.skyve.impl.util.json.JSONReaderTest$TestBean";
		String recordClass = "org.skyve.impl.util.json.JSONReaderTest$NestedRecord";
		String json = "{\"class\":\"" + beanClass + "\",\"nested\":{\"class\":\"" + recordClass
				+ "\",\"value\":\"nested\"}}";

		TestBean result = (TestBean) new JSONReader(null).read(json);

		assertEquals(new NestedRecord("nested"), result.getNested());
	}

	/** Verifies object mode replaces existing mutable collection and map contents. */
	@Test
	void readObjectModePopulatesCollectionAndMapProperties() throws Exception {
		String beanClass = "org.skyve.impl.util.json.JSONReaderTest$TestBean";
		String json = "{\"class\":\"" + beanClass + "\",\"values\":[1],\"attributes\":{\"answer\":42}}";

		TestBean result = (TestBean) new JSONReader(null).read(json);

		assertEquals(List.of(Long.valueOf(1L)), result.getValues());
		assertEquals(Map.of("answer", Long.valueOf(42L)), result.getAttributes());
	}

	/** Verifies object mode rejects collection and map properties without mutable targets. */
	@Test
	void readObjectModeRejectsNullCollectionAndMapProperties() {
		String beanClass = "org.skyve.impl.util.json.JSONReaderTest$TestBean";

		assertThrows(IllegalStateException.class, () -> new JSONReader(null).read(
				"{\"class\":\"" + beanClass + "\",\"missingValues\":[1]}"));
		assertThrows(IllegalStateException.class, () -> new JSONReader(null).read(
				"{\"class\":\"" + beanClass + "\",\"missingAttributes\":{\"a\":1}}"));
	}

	/** Verifies self-described records nested inside otherwise dynamic maps and lists. */
	@Test
	void readDynamicMapReconstructsNestedRecordsFromClassMetadata() throws Exception {
		String recordClass = "org.skyve.impl.util.json.JSONReaderTest$NestedRecord";
		String json = "{\"record\":{\"class\":\"" + recordClass + "\",\"value\":\"map\"},"
				+ "\"records\":[{\"class\":\"" + recordClass + "\",\"value\":\"list\"}]}";

		Map<Object, Object> result = readDynamic(json);

		assertEquals(new NestedRecord("map"), result.get("record"));
		assertEquals(List.of(new NestedRecord("list")), result.get("records"));
	}

	/** Verifies user-aware document reconstruction in both typed and untyped records. */
	@Test
	void readDocumentNestedInTypedAndUntypedRecord() throws Exception {
		DocumentContext context = documentContext();
		String recordClass = "org.skyve.impl.util.json.JSONReaderTest$DocumentRecord";
		String documentJson = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\"}";
		String json = "{\"class\":\"" + recordClass + "\",\"document\":" + documentJson + '}';

		DocumentRecord untyped = (DocumentRecord) new JSONReader(context.user()).read(json);
		DocumentRecord typed = org.skyve.util.JSON.unmarshall(context.user(), json, DocumentRecord.class);

		assertSame(context.document(), untyped.document());
		assertNotNull(typed);
		assertSame(context.document(), typed.document());
	}

	/** Verifies typed document reconstruction inside lists, maps, and arrays. */
	@Test
	void readTypedRecordReconstructsDocumentsInContainers() throws Exception {
		DocumentContext context = documentContext();
		String documentJson = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\"}";
		String json = "{\"documents\":[" + documentJson + "],\"documentsByName\":{\"first\":"
				+ documentJson + "},\"documentArray\":[" + documentJson + "]}";

		DocumentContainersRecord result = new JSONReader(context.user()).read(json, DocumentContainersRecord.class);

		assertNotNull(result);
		assertEquals(List.of(context.document()), result.documents());
		assertSame(context.document(), result.documentsByName().get("first"));
		assertArrayEquals(new Bean[] {context.document()}, result.documentArray());
	}

	/** Verifies document reconstruction inside ordinary classes, maps, and lists. */
	@Test
	void readDocumentsNestedInClassMapAndList() throws Exception {
		DocumentContext context = documentContext();
		String beanClass = "org.skyve.impl.util.json.JSONReaderTest$TestBean";
		String documentJson = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\"}";

		TestBean bean = (TestBean) new JSONReader(context.user()).read(
				"{\"class\":\"" + beanClass + "\",\"document\":" + documentJson + '}');
		@SuppressWarnings("unchecked")
		Map<Object, Object> values = (Map<Object, Object>) new JSONReader(context.user()).read(
				"{\"document\":" + documentJson + ",\"documents\":[" + documentJson + "]}");

		assertSame(context.document(), bean.getDocument());
		assertSame(context.document(), values.get("document"));
		assertEquals(List.of(context.document()), values.get("documents"));
	}

	/** Verifies malformed Skyve document metadata is rejected at each reserved boundary. */
	@Test
	void readDocumentRejectsMalformedMetadata() throws Exception {
		DocumentContext context = documentContext();
		JSONReader reader = new JSONReader(context.user());

		assertThrows(IllegalStateException.class,
				() -> reader.read("{\"bizModule\":\"test\",\"wrong\":\"Document\"}"));
		assertThrows(IllegalStateException.class,
				() -> reader.read("{\"bizModule\":\"test\",\"bizDocument\":\"Document\":null}"));
		IllegalStateException moduleException = assertThrows(IllegalStateException.class,
				() -> reader.read("{\"bizModule\":null,\"bizDocument\":\"Document\"}"));
		IllegalStateException documentException = assertThrows(IllegalStateException.class,
				() -> reader.read("{\"bizModule\":\"test\",\"bizDocument\":null}"));
		IllegalStateException propertyException = assertThrows(IllegalStateException.class,
				() -> reader.read("{\"bizModule\":\"test\",\"bizDocument\":\"Document\",null:1}"));

		assertTrue(moduleException.getMessage().contains("moduleName"));
		assertTrue(documentException.getMessage().contains("documentName"));
		assertTrue(propertyException.getMessage().contains("propertyName"));
	}

	/** Verifies an invalid incoming document identifier leaves the generated identifier intact. */
	@Test
	void readDocumentIgnoresInvalidIdentifier() throws Exception {
		DocumentContext context = documentContext();

		Object result = new JSONReader(context.user()).read(
				"{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"bizId\":\"invalid\"}");

		assertSame(context.document(), result);
	}

	/** Verifies document-specific identifiers, collections, locks, versions, and ownership. */
	@Test
	void readDocumentPopulatesReservedProperties() throws Exception {
		ReaderDocument document = Mockito.mock(ReaderDocument.class);
		List<Object> items = new ArrayList<>();
		Mockito.when(document.getItems()).thenReturn(items);
		Mockito.doReturn(Boolean.TRUE).when(document).isNotPersisted();
		DocumentContext context = documentContext(document);
		String json = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\","
				+ "\"bizId\":\"123e4567-e89b-12d3-a456-426614174000\",\"items\":[1],"
				+ "\"bizLock\":\"20191006022000121admin\",\"bizVersion\":2,\"name\":\"\"}";

		Object result = new JSONReader(context.user()).read(json);

		assertSame(document, result);
		assertEquals(List.of(Long.valueOf(1L)), items);
		Mockito.verify(document).setBizId("123e4567-e89b-12d3-a456-426614174000");
		Mockito.verify(document).setBizLock(notNull());
		Mockito.verify(document).setBizVersion(Integer.valueOf(2));
		Mockito.verify(document).setName(null);
		Mockito.verify(document).setBizCustomer("customer");
		Mockito.verify(document).setBizDataGroupId("group");
		Mockito.verify(document).setBizUserId("user");
	}

	/** Verifies null locks, versions and values are assigned, and immutable properties are skipped. */
	@Test
	void readDocumentAssignsNullsAndSkipsImmutableProperties() throws Exception {
		ReaderDocument document = Mockito.mock(ReaderDocument.class);
		DocumentContext context = documentContext(document);
		String json = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\","
				+ "\"bizLock\":null,\"bizVersion\":null,\"bizKey\":1,\"name\":null}";

		Object result = new JSONReader(context.user()).read(json);

		assertSame(document, result);
		Mockito.verify(document).setBizLock(null);
		Mockito.verify(document).setBizVersion(null);
		Mockito.verify(document).setName(null);
	}

	/** Verifies display strings are kept when the attribute has no converter or is not convertible. */
	@Test
	void readDocumentKeepsDisplayValueWithoutConverter() throws Exception {
		ReaderDocument convertibleDocument = Mockito.mock(ReaderDocument.class);
		DocumentContext convertibleContext = documentContext(convertibleDocument);
		ConvertibleField field = Mockito.mock(ConvertibleField.class);
		Mockito.when(convertibleContext.metadata().getAttribute("name")).thenReturn(field);
		Mockito.doReturn(String.class).when(field).getImplementingType();
		ReaderDocument plainDocument = Mockito.mock(ReaderDocument.class);
		DocumentContext plainContext = documentContext(plainDocument);
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(plainContext.metadata().getAttribute("name")).thenReturn(attribute);
		Mockito.doReturn(String.class).when(attribute).getImplementingType();
		String json = "{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"name\":\"display\"}";

		new JSONReader(convertibleContext.user()).read(json);
		new JSONReader(plainContext.user()).read(json);

		Mockito.verify(convertibleDocument).setName("display");
		Mockito.verify(plainDocument).setName("display");
	}

	/** Verifies a key followed by the object end closes the object in every mode without reading past it. */
	@Test
	void readKeyWithoutValueEndsObjectInEveryMode() throws Exception {
		DocumentContext context = documentContext();
		String className = "org.skyve.impl.util.json.JSONReaderTest$TestBean";

		List<?> dynamic = (List<?>) new JSONReader(null).read("[{\"name\"},\"next\"]");
		List<?> object = (List<?>) new JSONReader(null).read(
				"[{\"class\":\"" + className + "\",\"name\"},\"next\"]");
		List<?> document = (List<?>) new JSONReader(context.user()).read(
				"[{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"name\"},\"next\"]");

		assertEquals(List.of(Map.of(), "next"), dynamic);
		assertEquals(2, object.size());
		assertNull(((TestBean) object.get(0)).getName());
		assertEquals("next", object.get(1));
		assertEquals(List.of(context.document(), "next"), document);
	}

	/** Verifies a document list property must expose an existing mutable collection. */
	@Test
	void readDocumentRejectsNullCollectionProperty() throws Exception {
		ReaderDocument document = Mockito.mock(ReaderDocument.class);
		Mockito.doReturn(null).when(document).getItems();
		DocumentContext context = documentContext(document);

		assertThrows(IllegalStateException.class, () -> new JSONReader(context.user()).read(
				"{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"items\":[1]}"));
	}

	/** Verifies non-empty document strings are converted through attribute metadata. */
	@Test
	void readDocumentConvertsDisplayValue() throws Exception {
		ReaderDocument document = Mockito.mock(ReaderDocument.class);
		DocumentContext context = documentContext(document);
		ConvertibleField field = Mockito.mock(ConvertibleField.class);
		Converter<String> converter = Mockito.mock(Converter.class);
		Customer customer = context.user().getCustomer();
		Mockito.when(context.metadata().getAttribute("name")).thenReturn(field);
		Mockito.doReturn(String.class).when(field).getImplementingType();
		Mockito.doReturn(converter).when(field).getConverterForCustomer(customer);
		Mockito.when(converter.fromDisplayValue("display")).thenReturn("converted");

		Object result = new JSONReader(context.user()).read(
				"{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"name\":\"display\"}");

		assertSame(document, result);
		Mockito.verify(document).setName("converted");
	}

	/** Verifies malformed Java class metadata is rejected before object population. */
	@Test
	void readObjectModeRejectsMalformedClassSeparator() {
		String className = "org.skyve.impl.util.json.JSONReaderTest$TestBean";

		assertThrows(IllegalStateException.class,
				() -> new JSONReader(null).read("{\"class\":\"" + className + "\":null}"));
	}

	/** Verifies unterminated document and Java object bodies cannot loop indefinitely. */
	@Test
	void readObjectModesRejectUnterminatedBodies() throws Exception {
		DocumentContext context = documentContext(Mockito.mock(ReaderDocument.class));
		String className = "org.skyve.impl.util.json.JSONReaderTest$TestBean";

		IllegalStateException documentException = assertThrows(IllegalStateException.class,
				() -> new JSONReader(context.user()).read(
						"{\"bizModule\":\"test\",\"bizDocument\":\"Document\",\"bizLock\":\"\""));
		IllegalStateException objectException = assertThrows(IllegalStateException.class,
				() -> new JSONReader(null).read("{\"class\":\"" + className + "\",\"name\":\"value\""));

		assertTrue(documentException.getMessage().contains("unterminated"));
		assertTrue(objectException.getMessage().contains("unterminated"));
	}

	// ---- explicitly typed records ---------------------------------------------

	/** Verifies canonical construction and recursive conversion for typed records. */
	@Test
	void readTypedRecordInvokesCanonicalConstructorAndConvertsComponents() throws Exception {
		String json = "{\"name\":\"example\",\"count\":2,\"amount\":1.25,\"priority\":\"HIGH\","
				+ "\"nested\":{\"value\":\"parent\"},\"children\":[{\"value\":\"child\"}],"
				+ "\"optionalChildren\":null,\"childrenByName\":{\"first\":{\"value\":\"child\"}}}";

		ExampleRecord result = new JSONReader(null).read(json, ExampleRecord.class);

		assertNotNull(result);
		assertEquals("example", result.name());
		assertEquals(2, result.count());
		assertEquals(Double.valueOf(1.25d), result.amount());
		assertEquals(Priority.HIGH, result.priority());
		assertEquals(new NestedRecord("parent"), result.nested());
		assertEquals(List.of(new NestedRecord("child")), result.children());
		assertNull(result.optionalChildren());
		assertEquals(Map.of("first", new NestedRecord("child")), result.childrenByName());
	}

	/** Verifies all supported scalar and mutable-container conversion paths. */
	@Test
	void readTypedRecordConvertsScalarAndContainerVariants() throws Exception {
		String json = "{\"byteValue\":1,\"shortValue\":2,\"longValue\":3.9,\"floatValue\":1.5,"
				+ "\"doubleValue\":2.5,"
				+ "\"integerFromLong\":4,\"integerFromDecimal\":5.9,\"decimalFromLong\":6,"
				+ "\"decimalValue\":7.25,\"initial\":\"A\",\"active\":true,\"numbers\":[8,9],"
				+ "\"names\":[\"first\",\"second\"],\"values\":{\"answer\":42},"
				+ "\"arbitrary\":[\"text\",10]}";

		ConversionRecord result = new JSONReader(null).read(json, ConversionRecord.class);

		assertNotNull(result);
		assertEquals(Byte.valueOf((byte) 1), Byte.valueOf(result.byteValue()));
		assertEquals(Short.valueOf((short) 2), Short.valueOf(result.shortValue()));
		assertEquals(Long.valueOf(3L), Long.valueOf(result.longValue()));
		assertEquals(Float.valueOf(1.5F), Float.valueOf(result.floatValue()));
		assertEquals(Double.valueOf(2.5D), Double.valueOf(result.doubleValue()));
		assertEquals(new BigInteger("4"), result.integerFromLong());
		assertEquals(new BigInteger("5"), result.integerFromDecimal());
		assertEquals(new BigDecimal("6"), result.decimalFromLong());
		assertEquals(new BigDecimal("7.25"), result.decimalValue());
		assertEquals(Character.valueOf('A'), Character.valueOf(result.initial()));
		assertTrue(result.active());
		assertEquals(Set.of(Integer.valueOf(8), Integer.valueOf(9)), result.numbers());
		assertEquals(ArrayList.class, result.names().getClass());
		assertEquals(List.of("first", "second"), result.names());
		assertEquals(LinkedHashMap.class, result.values().getClass());
		assertEquals(Map.of("answer", Integer.valueOf(42)), result.values());
		assertEquals(List.of("text", Long.valueOf(10L)), result.arbitrary());
	}

	/** Verifies element conversion when the explicitly requested target is an array. */
	@Test
	void readTypedArrayConvertsElements() throws Exception {
		int[] result = new JSONReader(null).read("[3,4]", int[].class);

		assertNotNull(result);
		assertArrayEquals(new int[] {3, 4}, result);
	}

	/** Verifies that every canonical record component must be present in the JSON object. */
	@Test
	void readTypedRecordRejectsMissingComponent() {
		JSONReader reader = new JSONReader(null);

		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> reader.read("{\"name\":\"incomplete\"}", ExampleRecord.class));

		assertTrue(exception.getMessage().contains("count"));
	}

	/** Verifies rejection of null primitive components. */
	@Test
	void readTypedRecordRejectsNullPrimitiveComponent() {
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> new JSONReader(null).read("{\"value\":null}", GenericPrimitiveRecord.class));

		assertTrue(exception.getMessage().contains("int"));
	}

	/** Holds a primitive component for null-conversion validation. */
	private record GenericPrimitiveRecord(int value) {
		// Test value.
	}

	/** Verifies clear failures for incompatible record, array, and scalar inputs. */
	@Test
	void readTypedValueRejectsIncompatibleShapes() {
		JSONReader reader = new JSONReader(null);

		assertThrows(IllegalArgumentException.class, () -> reader.read("[]", NestedRecord.class));
		assertThrows(IllegalArgumentException.class, () -> reader.read("{}", int[].class));
		assertThrows(IllegalArgumentException.class, () -> reader.read("\"long\"", Character.class));
		assertThrows(IllegalArgumentException.class, () -> reader.read("\"true\"", Boolean.class));
		assertThrows(IllegalArgumentException.class, () -> reader.read("1", AtomicInteger.class));
		assertThrows(IllegalArgumentException.class,
				() -> reader.read("{\"list\":{},\"map\":{}}", ContainerRecord.class));
		assertThrows(IllegalArgumentException.class,
				() -> reader.read("{\"list\":[],\"map\":[]}", ContainerRecord.class));
		IllegalArgumentException optionalException = assertThrows(IllegalArgumentException.class,
				() -> reader.read("{\"value\":\"text\"}", OptionalRecord.class));
		assertTrue(optionalException.getMessage().contains(Optional.class.getTypeName()));
		assertThrows(IllegalArgumentException.class, () -> reader.read("1", void.class));
	}

	/** Verifies typed JSON null remains null for a reference target. */
	@Test
	void readTypedNullReturnsNull() throws Exception {
		assertNull(new JSONReader(null).read("null", String.class));
	}

	/** Verifies rejection of generic type variables that cannot be resolved from a raw class. */
	@Test
	void readTypedRecordRejectsUnresolvedTypeVariable() {
		assertThrows(IllegalArgumentException.class,
				() -> new JSONReader(null).read("{\"value\":1}", GenericRecord.class));
	}

	/** Verifies that typed reads do not load a class named by the payload. */
	@Test
	void readTypedRecordDoesNotInterpretReservedComponentNameAsTypeMetadata() throws Exception {
		ReservedNameRecord result = new JSONReader(null).read(
				"{\"class\":\"untrusted.Type\",\"bizModule\":\"module\",\"value\":\"safe\"}", ReservedNameRecord.class);

		assertEquals(new ReservedNameRecord("module", "safe"), result);
	}

	// ---- scientific notation with explicit + sign ---------------------------

	@Test
	void readScientificNotationWithPositiveSign() throws Exception {
		// Covers the 'c == '+'' branch in number()
		Map<Object, Object> result = readDynamic("{\"v\":1.5e+2}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertEquals(0, new BigDecimal("1.5e+2").compareTo((BigDecimal) val));
	}

	// ---- unquoted key path (string with '\0' delimiter) ---------------------

	@Test
	void readUnquotedKeyFallsBackToStringMode() throws Exception {
		// An unquoted key triggers the string('\0') code path (stops at ':')
		Object result = new JSONReader(null).read("{key: 42}");
		assertTrue(result instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) result;
		assertEquals(Long.valueOf(42L), map.get("key"));
	}

	@Test
	void readUnquotedKeyStartingWithFNotFalse() throws Exception {
		// Exercise a key that starts with f without being the false literal.
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) new JSONReader(null).read("{fast: 1}");
		assertEquals(Long.valueOf(1L), map.get("fast"));
	}

	@Test
	void readUnquotedKeyStartingWithTNotTrue() throws Exception {
		// Key starts with 't' but is not "true" — exercises the t-not-true else branch
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) new JSONReader(null).read("{timeout: 5}");
		assertEquals(Long.valueOf(5L), map.get("timeout"));
	}

	@Test
	void readUnquotedKeyStartingWithNNotNull() throws Exception {
		// Key starts with 'n' but is not "null" — exercises the n-not-null else branch
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) new JSONReader(null).read("{name: \"Alice\"}");
		assertEquals("Alice", map.get("name"));
	}

	/** Verifies unquoted keys spelt with a leading literal are keys, not the literal. */
	@Test
	void readUnquotedKeysStartingWithLiterals() throws Exception {
		Map<Object, Object> result = readDynamic("{nullable:null,trueClientIp:true,falsehood:false,null_1:1,true$:2}");

		Map<Object, Object> expected = new LinkedHashMap<>();
		expected.put("nullable", null);
		expected.put("trueClientIp", Boolean.TRUE);
		expected.put("falsehood", Boolean.FALSE);
		expected.put("null_1", Long.valueOf(1L));
		expected.put("true$", Long.valueOf(2L));
		assertEquals(expected, result);
	}

	/** Verifies literals are recognised when they end the input. */
	@Test
	void readTopLevelLiterals() throws Exception {
		assertEquals(Boolean.TRUE, new JSONReader(null).read("true"));
		assertEquals(Boolean.FALSE, new JSONReader(null).read("false"));
		assertNull(new JSONReader(null).read("null"));
	}

	/** Verifies an unknown escape keeps the escaped character, as JavaScript does. */
	@Test
	void readStringKeepsCharacterOfUnknownEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":\"a\\qb\"}");
		assertEquals("aqb", result.get("v"));
	}

	// ---- malformed JSON error paths -----------------------------------------

	@Test
	void malformedUnterminatedStringThrows() {
		JSONReader reader = new JSONReader(null);
		assertThrows(IllegalStateException.class, () -> reader.read("{\"key\":\"unclosed"));
	}

	@Test
	void malformedUnterminatedArrayThrows() {
		JSONReader reader = new JSONReader(null);
		assertThrows(IllegalStateException.class, () -> reader.read("[1,2,3"));
	}

	@Test
	void malformedUnterminatedObjectThrows() {
		JSONReader reader = new JSONReader(null);
		assertThrows(IllegalStateException.class, () -> reader.read("{\"a\":1"));
	}

	// ---- unicode() — lowercase and uppercase hex branch coverage ------------

	@Test
	void readUnicodeEscapeWithLowercaseHexLettersCoversBranch() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":\"\\u00ae\"}");
		assertEquals("®", result.get("v"));
	}

	@Test
	void readUnicodeEscapeWithUppercaseHexLettersCoversBranch() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":\"\\u00AB\"}");
		assertEquals("«", result.get("v"));
	}

	@Test
	void rejectUnicodeEscapeWithNonHexadecimalCharacter() {
		assertThrows(IllegalStateException.class, () -> readDynamic("{\"v\":\"\\u12G4\"}"));
	}

	@Test
	void rejectTruncatedUnicodeEscape() {
		assertThrows(IllegalStateException.class, () -> readDynamic("{\"v\":\"\\u12"));
	}
}

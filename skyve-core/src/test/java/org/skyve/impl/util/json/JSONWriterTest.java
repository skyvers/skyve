package org.skyve.impl.util.json;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.Serial;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
class JSONWriterTest {
	private static final String ADMIN_USERNAME = "admin";
	private static final String HELLO = "hello";
	private static final String JSON_HELLO = "\"hello\"";
	private static final String VALUE = "value";

	/** Adds ordinary getters to a mocked Skyve document for attribute serialisation. */
	public abstract static class WriterDocument extends AbstractPersistentBean
	implements ChildBean<Bean>, HierarchicalBean<Bean> {
		@Serial
		private static final long serialVersionUID = 1L;

		/** Returns a reference attribute value. */
		public abstract Bean getReference();

		/** Returns a domain-backed attribute value. */
		public abstract String getChoice();

		/** Returns a Boolean attribute value. */
		public abstract Boolean getFlag();

		/** Returns a display-formatted attribute value. */
		public abstract String getText();
	}

	/** Groups metadata and the multiply-typed document used by writer tests. */
	private record WriterDocumentContext(Customer customer, WriterDocument document) {
		// Test fixture.
	}

	/** Creates document metadata covering every attribute-writing branch. */
	private static WriterDocumentContext writerDocumentContext() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document metadata = mock(Document.class);
		Reference reference = mock(Reference.class);
		Attribute domain = mock(Attribute.class);
		Attribute bool = mock(Attribute.class);
		Attribute text = mock(Attribute.class);
		@SuppressWarnings("java:S5969") // a mock stubs the whole bean contract (persistent, child and hierarchical) far more briefly than a hand-written subclass
		WriterDocument document = mock(WriterDocument.class);

		when(document.getBizModule()).thenReturn("test");
		when(document.getBizDocument()).thenReturn("Document");
		when(document.getBizId()).thenReturn("id");
		when(document.getBizCustomer()).thenReturn("customer");
		when(document.getBizDataGroupId()).thenReturn("group");
		when(document.getBizUserId()).thenReturn("user");
		when(document.getBizVersion()).thenReturn(Integer.valueOf(2));
		when(document.getBizLock()).thenReturn(new OptimisticLock(ADMIN_USERNAME, Date.from(Instant.EPOCH)));
		doReturn(Boolean.FALSE).when(document).isDynamic(anyString());
		doReturn(mock(Bean.class)).when((ChildBean<?>) document).getParent();
		when(((ChildBean<?>) document).getBizOrdinal()).thenReturn(Integer.valueOf(3));
		when(((HierarchicalBean<?>) document).getBizParentId()).thenReturn("parent-id");
		Bean referencedBean = mock(Bean.class);
		when(document.getReference()).thenReturn(referencedBean);
		when(document.getChoice()).thenReturn("choice");
		when(document.getFlag()).thenReturn(Boolean.TRUE);
		when(document.getText()).thenReturn("text");

		when(reference.getName()).thenReturn("reference");
		when(domain.getName()).thenReturn("choice");
		when(domain.getDomainType()).thenReturn(DomainType.constant);
		when(bool.getName()).thenReturn("flag");
		when(bool.getAttributeType()).thenReturn(AttributeType.bool);
		when(text.getName()).thenReturn("text");
		when(text.getAttributeType()).thenReturn(AttributeType.text);
		doReturn(String.class).when(text).getImplementingType();
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "Document")).thenReturn(metadata);
		when(metadata.getAttribute("text")).thenReturn(text);
		doReturn(List.of(reference, domain, bool, text)).when(metadata).getAllAttributes(customer);
		return new WriterDocumentContext(customer, document);
	}

	@Test
	void staticWriteSerialisesSimpleValues() {
		Object[][] cases = {
				{(Supplier<String>) () -> JSONWriter.write(42L), "42"},
				{(Supplier<String>) () -> JSONWriter.write(-1L), "-1"},
				{(Supplier<String>) () -> JSONWriter.write(3.14d), "3.14"},
				{(Supplier<String>) () -> JSONWriter.write('a'), "\"a\""},
				{(Supplier<String>) () -> JSONWriter.write(true), "true"},
				{(Supplier<String>) () -> JSONWriter.write(false), "false"}
		};
		for (Object[] testCase : cases) {
			@SuppressWarnings("unchecked")
			Supplier<String> write = (Supplier<String>) testCase[0];
			assertThat(write.get(), is(testCase[1]));
		}
	}

	@Test
	@SuppressWarnings("java:S5976") // one named test per scalar type reads more clearly than a parameterized table
	void writeNullReturnsNullLiteral() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(null, null);
		assertThat(result, is("null"));
	}

	@Test
	void writeStringEscapesSpecialCharacters() {
		JSONWriter writer = new JSONWriter(null);
		String[][] cases = {
				{HELLO, JSON_HELLO},
				{"say \"hi\"", "\"say \\\"hi\\\"\""},
				{"a\\b", "\"a\\\\b\""},
				{"line1\nline2", "\"line1\\nline2\""},
				{"a\tb", "\"a\\tb\""},
				{"a\rb", "\"a\\rb\""}
		};
		for (String[] testCase : cases) {
			String result = writer.write(testCase[0], null);
			assertThat(result, is(testCase[1]));
		}
	}

	@Test
	void writeIntegerNumber() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Integer.valueOf(7), null);
		assertThat(result, is("7"));
	}

	@Test
	void writeLongNumber() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Long.valueOf(100L), null);
		assertThat(result, is("100"));
	}

	@Test
	void writeBoolean() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Boolean.TRUE, null);
		assertThat(result, is("true"));
	}

	@Test
	void writeEmptyMap() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new LinkedHashMap<>(), null);
		assertThat(result, is("{}"));
	}

	@Test
	void writeSingleEntryMap() {
		JSONWriter writer = new JSONWriter(null);
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("key", VALUE);
		String result = writer.write(map, null);
		assertThat(result, is("{\"key\":\"" + VALUE + "\"}"));
	}

	@Test
	void writeEmptyList() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new ArrayList<>(), null);
		assertThat(result, is("[]"));
	}

	@Test
	void writeListWithElements() {
		JSONWriter writer = new JSONWriter(null);
		List<Object> list = new ArrayList<>();
		list.add("a");
		list.add("b");
		String result = writer.write(list, null);
		assertThat(result, is("[\"a\",\"b\"]"));
	}

	@Test
	void writeIntArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new int[]{1, 2, 3}, null);
		assertThat(result, is("[1,2,3]"));
	}

	@Test
	void writeStringArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new String[]{"x", "y"}, null);
		assertThat(result, is("[\"x\",\"y\"]"));
	}

	@Test
	void writeClassType() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(String.class, null);
		assertThat(result, is("\"java.lang.String\""));
	}

	@Test
	void writeCharacterObject() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Character.valueOf('Z'), null);
		assertThat(result, is("\"Z\""));
	}

	@Test
	void writeResultIsNotNull() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("test", null);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void writeEmptyString() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("", null);
		assertThat(result, is("\"\""));
	}

	@Test
	void writeMapWithNullValue() {
		JSONWriter writer = new JSONWriter(null);
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("k", null);
		String result = writer.write(map, null);
		assertThat(result, is("{\"k\":null}"));
	}

	@Test
	void writeStringWithSlash() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a/b", null);
		assertThat(result, is("\"a\\/b\""));
	}

	@Test
	void writeStringWithFormFeed() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\fb", null);
		assertThat(result, is("\"a\\fb\""));
	}

	@Test
	void writeStringWithBackspace() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\bb", null);
		assertThat(result, is("\"a\\bb\""));
	}

	// ---- Enum types (java.lang.Enum) -------------------------------------------

	private enum Colour { RED, GREEN, BLUE }

	@Test
	void writeJavaEnum() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Colour.RED, null);
		assertThat(result, is("\"RED\""));
	}

	// ---- Date type --------------------------------------------------------------

	@Test
	void writeDateProducesQuotedString() {
		JSONWriter writer = new JSONWriter(null);
		Date date = Date.from(Instant.EPOCH);
		String result = writer.write(date, null);
		assertThat(result, is("\"" + date.toString() + "\""));
	}

	// ---- Iterator type ----------------------------------------------------------

	@Test
	void writeIteratorDirectlyProducesArray() {
		JSONWriter writer = new JSONWriter(null);
		java.util.Iterator<String> it = java.util.Arrays.asList("x", "y", "z").iterator();
		String result = writer.write(it, null);
		assertThat(result, is("[\"x\",\"y\",\"z\"]"));
	}

	// ---- POJO (java bean) -------------------------------------------------------

	/** Simple JavaBean used to exercise the bean() method. */
	public static class SimpleBean {
		private String label;

		public String getLabel() {
			return label;
		}

		public void setLabel(String label) {
			this.label = label;
		}
	}

	/** Supplies an accessor that fails during ordinary Java bean serialisation. */
	@SuppressWarnings("unused") // Accessed reflectively through JavaBeans introspection.
	private static class FailingBean {
		/** Always fails so the reflective exception path can be verified. */
		public String getValue() {
			throw new IllegalStateException("getter failed");
		}

		/** Provides a writable property for JavaBeans introspection. */
		public void setValue(String value) {
			// Test fixture.
		}
	}

	/** Supplies a non-public declaring class whose accessor must be made accessible. */
	@SuppressWarnings("unused") // Accessed reflectively through JavaBeans introspection.
	private static class PrivateBean {
		/** Returns the value written by the accessibility test. */
		public String getValue() {
			return "private";
		}

		/** Provides a writable property for JavaBeans introspection. */
		public void setValue(String value) {
			// Test fixture.
		}
	}

	@Test
	void writePOJOIncludesClassAndPropertyInJSON() {
		JSONWriter writer = new JSONWriter(null);
		SimpleBean bean = new SimpleBean();
		bean.setLabel(HELLO);
		String result = writer.write(bean, null);
		assertThat(result, containsString("\"class\""));
		assertThat(result, containsString("\"label\""));
		assertThat(result, containsString(JSON_HELLO));
	}

	@Test
	void writePOJOWithNullPropertyWritesNull() {
		JSONWriter writer = new JSONWriter(null);
		SimpleBean bean = new SimpleBean();
		// label is null by default
		String result = writer.write(bean, null);
		assertThat(result, containsString("\"label\":null"));
	}

	/** Verifies inaccessible declaring classes are opened for bean accessor invocation. */
	@Test
	void writePrivatePojoMakesAccessorAccessible() {
		String result = new JSONWriter(null).write(new PrivateBean(), null);

		assertThat(result, containsString("\"value\":\"private\""));
	}

	/** Verifies a failing Java bean getter is logged and produces a closed JSON object. */
	@Test
	void writePojoHandlesGetterFailure() {
		String result = new JSONWriter(null).write(new FailingBean(), null);

		assertThat(result, startsWith("{\"class\":"));
		assertThat(result, org.hamcrest.CoreMatchers.endsWith("}"));
	}

	// ---- Java records -----------------------------------------------------------

	/** Supplies a nested record value for writer tests. */
	private record NestedRecord(String value) {
		// Test value.
	}

	/** Supplies scalar, nullable, nested, and collection components for record writing. */
	private record ExampleRecord(String name, int count, Double amount, NestedRecord nested,
			List<NestedRecord> children) {
		// Test value.
	}

	/** Supplies a record accessor that fails while being serialised. */
	private record FailingRecord(String value) {
		/**
		 * Fails deliberately to exercise record-accessor error handling.
		 *
		 * @return never returns normally
		 * @throws IllegalStateException on every invocation
		 */
		@Override
		public String value() {
			throw new IllegalStateException("accessor failed");
		}
	}

	/** Supplies a record whose mutable component can refer back to the record. */
	private record CyclicRecord(List<Object> values) {
		// Test value.
	}

	/** Verifies class metadata and declaration-ordered components in record JSON. */
	@Test
	void writeRecordUsesComponentNamesWithClassMetadata() {
		JSONWriter writer = new JSONWriter(null);
		ExampleRecord value = new ExampleRecord("example", 2, null, new NestedRecord("parent"),
				List.of(new NestedRecord("child")));

		String result = writer.write(value, null);

		assertThat(result, is("{\"class\":\"org.skyve.impl.util.json.JSONWriterTest$ExampleRecord\","
				+ "\"name\":\"example\",\"count\":2,\"amount\":null,"
				+ "\"nested\":{\"class\":\"org.skyve.impl.util.json.JSONWriterTest$NestedRecord\","
				+ "\"value\":\"parent\"},\"children\":[{\"class\":"
				+ "\"org.skyve.impl.util.json.JSONWriterTest$NestedRecord\",\"value\":\"child\"}]}"));
	}

	/** Verifies that records retain class metadata when a projection is supplied. */
	@Test
	void writeProjectedRecordRetainsClassMetadata() {
		ExampleRecord value = new ExampleRecord("example", 2, null, null, List.of());

		String result = new JSONWriter(null).write(value, java.util.Set.of("name"));

		assertThat(result, startsWith("{\"class\":\"org.skyve.impl.util.json.JSONWriterTest$ExampleRecord\""));
		assertThat(result, containsString("\"count\":2"));
	}

	/** Verifies that record component accessor failures retain their original cause. */
	@Test
	void writeRecordWrapsAccessorFailure() {
		FailingRecord value = new FailingRecord("ignored");
		JSONWriter writer = new JSONWriter(null);

		IllegalStateException exception = assertThrows(IllegalStateException.class, () -> writer.write(value, null));

		assertThat(exception.getMessage(), containsString(VALUE));
		assertSame(IllegalStateException.class, exception.getCause().getClass());
	}

	/** Verifies circular references reached through record components are written as null. */
	@Test
	void writeRecordStopsCircularReferences() {
		List<Object> values = new ArrayList<>();
		CyclicRecord value = new CyclicRecord(values);
		values.add(value);

		String result = new JSONWriter(null).write(value, null);

		assertThat(result, containsString("\"values\":[null]"));
	}

	// ---- Skyve documents -------------------------------------------------------

	/**
	 * Verifies complete document output across all attribute and standard-property paths,
	 * without querying or emitting the synthetic hierarchical children property.
	 */
	@Test
	void writeCompleteDocumentIncludesAttributesAndFrameworkProperties() {
		WriterDocumentContext context = writerDocumentContext();

		String result = new JSONWriter(context.customer()).write(context.document(), null);

		assertThat(result, containsString("\"bizModule\":\"test\""));
		assertThat(result, containsString("\"reference\":"));
		assertThat(result, containsString("\"choice\":\"choice\""));
		assertThat(result, containsString("\"flag\":true"));
		assertThat(result, containsString("\"text\":\"text\""));
		assertThat(result, containsString("\"bizVersion\":2"));
		assertThat(result, containsString("\"bizParentId\":\"parent-id\""));
		assertThat(result, not(containsString("\"children\":")));
		verify((HierarchicalBean<?>) context.document(), never()).getChildren();
	}

	/** Verifies projected documents sanitise bindings and tolerate missing properties. */
	@Test
	void writeProjectedDocumentUsesRequestedBindings() {
		WriterDocumentContext context = writerDocumentContext();
		java.util.Set<String> properties = new java.util.LinkedHashSet<>();
		properties.add("text");
		properties.add("missing.property");

		String result = new JSONWriter(context.customer()).write(context.document(), properties);

		assertThat(result, containsString("\"text\":\"text\""));
		assertThat(result, containsString("\"missing_property\":null"));
	}

	/** Verifies metadata failures are contained and still produce a closed JSON object. */
	@Test
	void writeDocumentHandlesMetadataFailure() {
		WriterDocumentContext context = writerDocumentContext();
		when(context.customer().getModule("test")).thenThrow(new IllegalStateException("metadata failed"));

		String result = new JSONWriter(context.customer()).write(context.document(), null);

		assertThat(result, startsWith("{\"bizModule\":\"test\",\"bizDocument\":\"Document\""));
		assertThat(result, org.hamcrest.CoreMatchers.endsWith("}"));
	}

	// ---- OptimisticLock -------------------------------------------------

	@Test
	void writeOptimisticLockProducesQuotedString() {
		JSONWriter writer = new JSONWriter(null);
		org.skyve.domain.types.OptimisticLock lock =
				new org.skyve.domain.types.OptimisticLock(ADMIN_USERNAME, Date.from(Instant.EPOCH));
		String result = writer.write(lock, null);
		assertThat(result, containsString(ADMIN_USERNAME));
	}

	// ---- Skyve Enumeration interface ------------------------------------

	private enum TestColour implements org.skyve.domain.types.Enumeration {
		RED, GREEN, BLUE;

		@Override
		public String toCode() {
			return name().toLowerCase();
		}

		@Override
		public String toLocalisedDescription() {
			return name();
		}

		@Override
		public org.skyve.metadata.model.document.Bizlet.DomainValue toDomainValue() {
			return new org.skyve.metadata.model.document.Bizlet.DomainValue(toCode(), toLocalisedDescription());
		}
	}

	@Test
	void writeSkyveEnumerationUsesToCode() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(TestColour.RED, null);
		assertThat(result, is("\"red\""));
	}

	// ---- Unicode / ISO control chars ------------------------------------

	@Test
	void writeStringWithControlCharProducesUnicodeEscape() {
		JSONWriter writer = new JSONWriter(null);
		// \u0001 is a SOH control char (not in the mapped escapes)
		String result = writer.write("\u0001", null);
		assertThat(result, containsString("\\u"));
	}

	// ---- cyclic reference detection -------------------------------------

	@Test
	void writeCyclicMapProducesNull() {
		JSONWriter writer = new JSONWriter(null);
		java.util.Map<String, Object> map = new java.util.LinkedHashMap<>();
		// put the map into itself to create a cycle
		map.put("self", map);
		String result = writer.write(map, null);
		// second reference to the map is cyclic, so written as null
		assertThat(result, containsString("null"));
	}

	// ---- double array ---------------------------------------------------

	@Test
	void writeDoubleArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new double[]{1.1, 2.2}, null);
		assertThat(result, is("[1.1,2.2]"));
	}

	@Test
	void writeBooleanArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new boolean[]{true, false}, null);
		assertThat(result, is("[true,false]"));
	}

	// ---- Iterable (not a Collection/List) -------------------------------

	@Test
	void writeIterableProducesArray() {
		JSONWriter writer = new JSONWriter(null);
		java.util.TreeSet<String> set = new java.util.TreeSet<>();
		set.add("alpha");
		set.add("beta");
		String result = writer.write(set, null);
		assertThat(result, is("[\"alpha\",\"beta\"]"));
	}

	// ---- nested map -----------------------------------------------------

	@Test
	void writeNestedMap() {
		JSONWriter writer = new JSONWriter(null);
		java.util.Map<String, Object> outer = new java.util.LinkedHashMap<>();
		java.util.Map<String, Object> inner = new java.util.LinkedHashMap<>();
		inner.put("x", "1");
		outer.put("inner", inner);
		String result = writer.write(outer, null);
		assertThat(result, is("{\"inner\":{\"x\":\"1\"}}"));
	}

        @Test
        void writeNullStringReturnsNull() {
                JSONWriter writer = new JSONWriter(null);
                String result = writer.write((String) null, null);
                assertThat(result, is("null"));
        }

        @Test
        void writeStringWithUnicodeHighCodePoint() {
                JSONWriter writer = new JSONWriter(null);
                // emoji: U+1F600 requires surrogate pair
                String emoji = "\uD83D\uDE00";
                String result = writer.write(emoji, null);
                assertThat(result, is(notNullValue()));
        }

        @Test
        void writeMultiEntryMap() {
                JSONWriter writer = new JSONWriter(null);
                java.util.Map<String, Object> map = new java.util.LinkedHashMap<>();
                map.put("a", Integer.valueOf(1));
                map.put("b", Integer.valueOf(2));
                String result = writer.write(map, null);
                assertThat(result, is("{\"a\":1,\"b\":2}"));
        }

        @Test
        void writeIntArrayLong() {
                JSONWriter writer = new JSONWriter(null);
                long[] arr = {100L, 200L};
                String result = writer.write(arr, null);
                assertThat(result, is("[100,200]"));
        }

        @Test
        void writeFloatArray() {
                JSONWriter writer = new JSONWriter(null);
                float[] arr = {1.5f, 2.5f};
                String result = writer.write(arr, null);
                assertThat(result, is(notNullValue()));
        }

        @Test
        void writeByteArray() {
                JSONWriter writer = new JSONWriter(null);
                byte[] arr = {0x61, 0x62}; // 'a','b'
                String result = writer.write(arr, null);
                assertThat(result, is(notNullValue()));
        }

        @Test
        void writeShortArray() {
                JSONWriter writer = new JSONWriter(null);
                short[] arr = {1, 2};
                String result = writer.write(arr, null);
                assertThat(result, is(notNullValue()));
        }

        @Test
        void writeCharArray() {
                JSONWriter writer = new JSONWriter(null);
                char[] arr = {'a', 'b'};
                String result = writer.write(arr, null);
                assertThat(result, is(notNullValue()));
        }

        @Test
        void writeObjectArray() {
                JSONWriter writer = new JSONWriter(null);
				Object[] arr = {HELLO, Integer.valueOf(42)};
                String result = writer.write(arr, null);
                assertThat(result, is("[\"hello\",42]"));
        }

        // ---- Geometry -------------------------------------------------------

        @Test
        void writeGeometryProducesWktString() {
                JSONWriter writer = new JSONWriter(null);
                Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
                String result = writer.write(point, null);
                assertThat(result, startsWith("\"POINT"));
                assertThat(result, containsString("1"));
                assertThat(result, containsString("2"));
        }

        // ---- nested map (already in writer, add a test with null value in map)

        @Test
        void writeMapContainingNullValue() {
                JSONWriter writer = new JSONWriter(null);
                Map<String, Object> map = new LinkedHashMap<>();
				map.put("present", VALUE);
                map.put("absent", null);
                String result = writer.write(map, null);
                assertThat(result, containsString("\"present\""));
                assertThat(result, containsString("null"));
        }

        // ---- Skyve Bean without customer throws --------------------------------

        @Test
        void writeSkyveBeansWithoutCustomerThrowsIllegalStateException() {
                JSONWriter writer = new JSONWriter(null);
                Bean mockBean = mock(Bean.class);
                org.junit.jupiter.api.Assertions.assertThrows(
                        IllegalStateException.class,
                        () -> writer.write(mockBean, null)
                );
        }

        // ---- Skyve Bean projection path (propertyNames not null, !topLevel) ---

        @Test
        void writeBeanInMapWithPropertyNamesUsesProjectionPath() {
                // A Bean inside a Map with propertyNames != null:
                // value() is called with topLevel=false (from map()) and propertyNames != null
                // → string(bean.getBizId()) is called instead of document()
                JSONWriter writer = new JSONWriter(null);
                Bean mockBean = mock(Bean.class);
                when(mockBean.getBizId()).thenReturn("projection-id-123");

                Map<String, Object> map = new LinkedHashMap<>();
                map.put("ref", mockBean);
                java.util.Set<String> propertyNames = new java.util.LinkedHashSet<>();
                propertyNames.add("ref");
                String result = writer.write(map, propertyNames);
                assertThat(result, containsString("projection-id-123"));
        }

	// ---- plain POJO (falls through to bean() serialization) ----

	/**
	 * Simple POJO with a getter that is not a Skyve Bean, Collection, Map, or primitive.
	 * Writing it triggers the {@code bean()} path in {@link JSONWriter}.
	 */
	public static class WriterTestBean {
		private String label;
		private int count;

		public String getLabel() {
			return label;
		}

		public void setLabel(String label) {
			this.label = label;
		}

		public int getCount() {
			return count;
		}

		public void setCount(int count) {
			this.count = count;
		}
	}

	@Test
	void writePlainPojoWithNullPropertyNamesIncludesClassField() {
		JSONWriter writer = new JSONWriter(null);
		WriterTestBean bean = new WriterTestBean();
		bean.setLabel(HELLO);
		bean.setCount(7);
		String result = writer.write(bean, null);
		// propertyNames == null → class property is prepended
		assertThat(result, containsString("\"class\""));
		assertThat(result, containsString(JSON_HELLO));
	}

	@Test
	void writePlainPojoWithPropertyNamesExcludesClassField() {
		JSONWriter writer = new JSONWriter(null);
		WriterTestBean bean = new WriterTestBean();
		bean.setLabel("world");
		java.util.Set<String> props = new java.util.LinkedHashSet<>();
		props.add("label");
		String result = writer.write(bean, props);
		assertThat(result, containsString("\"world\""));
	}

	@Test
	void writeOptimisticLockOutputsString() {
		Date timestamp = Date.from(LocalDateTime.of(2024, Month.JANUARY, 15, 10, 30)
				.atZone(ZoneId.systemDefault()).toInstant());
		OptimisticLock lock = new OptimisticLock(ADMIN_USERNAME, timestamp);
		String result = new JSONWriter(null).write(lock, null);
		assertThat(result, notNullValue());
		assertThat(result, startsWith("\""));
	}
}

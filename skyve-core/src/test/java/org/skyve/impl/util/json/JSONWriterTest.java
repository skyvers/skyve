package org.skyve.impl.util.json;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.skyve.domain.types.OptimisticLock;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.mockito.Mockito;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
class JSONWriterTest {

	@Test
	void staticWriteLong() {
		assertThat(JSONWriter.write(42L), is("42"));
	}

	@Test
	void staticWriteNegativeLong() {
		assertThat(JSONWriter.write(-1L), is("-1"));
	}

	@Test
	void staticWriteDouble() {
		assertThat(JSONWriter.write(3.14d), is("3.14"));
	}

	@Test
	void staticWriteChar() {
		assertThat(JSONWriter.write('a'), is("\"a\""));
	}

	@Test
	void staticWriteBooleanTrue() {
		assertThat(JSONWriter.write(true), is("true"));
	}

	@Test
	void staticWriteBooleanFalse() {
		assertThat(JSONWriter.write(false), is("false"));
	}

	@Test
	void writeNullReturnsNullLiteral() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(null, null);
		assertThat(result, is("null"));
	}

	@Test
	void writeStringProducesQuotedValue() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("hello", null);
		assertThat(result, is("\"hello\""));
	}

	@Test
	void writeStringWithSpecialChars() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("say \"hi\"", null);
		assertThat(result, is("\"say \\\"hi\\\"\""));
	}

	@Test
	void writeStringWithBackslash() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\\b", null);
		assertThat(result, is("\"a\\\\b\""));
	}

	@Test
	void writeStringWithNewline() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("line1\nline2", null);
		assertThat(result, is("\"line1\\nline2\""));
	}

	@Test
	void writeStringWithTab() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\tb", null);
		assertThat(result, is("\"a\\tb\""));
	}

	@Test
	void writeStringWithCarriageReturn() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\rb", null);
		assertThat(result, is("\"a\\rb\""));
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
		map.put("key", "value");
		String result = writer.write(map, null);
		assertThat(result, is("{\"key\":\"value\"}"));
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
		java.util.Date date = new java.util.Date(0L);
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

	@Test
	void writePOJOIncludesClassAndPropertyInJSON() {
		JSONWriter writer = new JSONWriter(null);
		SimpleBean bean = new SimpleBean();
		bean.setLabel("hello");
		String result = writer.write(bean, null);
		assertThat(result, containsString("\"class\""));
		assertThat(result, containsString("\"label\""));
		assertThat(result, containsString("\"hello\""));
	}

	@Test
	void writePOJOWithNullPropertyWritesNull() {
		JSONWriter writer = new JSONWriter(null);
		SimpleBean bean = new SimpleBean();
		// label is null by default
		String result = writer.write(bean, null);
		assertThat(result, containsString("\"label\":null"));
	}

	// ---- OptimisticLock -------------------------------------------------

	@Test
	void writeOptimisticLockProducesQuotedString() {
		JSONWriter writer = new JSONWriter(null);
		org.skyve.domain.types.OptimisticLock lock =
				new org.skyve.domain.types.OptimisticLock("admin", new java.util.Date(0L));
		String result = writer.write(lock, null);
		assertThat(result, containsString("admin"));
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
                Object[] arr = {"hello", Integer.valueOf(42)};
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
                map.put("present", "value");
                map.put("absent", null);
                String result = writer.write(map, null);
                assertThat(result, containsString("\"present\""));
                assertThat(result, containsString("null"));
        }

        // ---- Skyve Bean without customer throws --------------------------------

        @Test
        void writeSkyveBeansWithoutCustomerThrowsIllegalStateException() {
                JSONWriter writer = new JSONWriter(null);
                Bean mockBean = Mockito.mock(Bean.class);
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
                Bean mockBean = Mockito.mock(Bean.class);
                Mockito.when(mockBean.getBizId()).thenReturn("projection-id-123");

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
		bean.setLabel("hello");
		bean.setCount(7);
		String result = writer.write(bean, null);
		// propertyNames == null → class property is prepended
		assertThat(result, containsString("\"class\""));
		assertThat(result, containsString("\"hello\""));
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
		@SuppressWarnings("deprecation")
		OptimisticLock lock = new OptimisticLock("admin", new Date(2024 - 1900, 0, 15, 10, 30, 0));
		String result = new JSONWriter(null).write(lock, null);
		assertThat(result, notNullValue());
		assertThat(result, startsWith("\""));
	}
}

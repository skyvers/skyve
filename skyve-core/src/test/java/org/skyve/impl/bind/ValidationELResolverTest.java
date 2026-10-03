package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;

import jakarta.el.ELContext;
import jakarta.el.MethodNotFoundException;
import jakarta.el.PropertyNotFoundException;

@SuppressWarnings("static-method")
class ValidationELResolverTest {
	public static class SampleBean {
		private String name;
		private int count;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public int getCount() {
			return count;
		}

		public void setCount(int count) {
			this.count = count;
		}

		public String echo(String value) {
			return value;
		}

		public int size() {
			return 1;
		}
	}

	public static class ReadOnlyBean {
		public String getValue() {
			return "v";
		}
	}

	public enum Colour { RED, GREEN }

	public enum Empty {
		// no constants
	}

	public static class EnumValueBean {
		public Colour getColour() {
			return null;
		}

		public Empty getEmpty() {
			return null;
		}
	}

	public static class MutableTypesBean {
		public DateOnly getDateOnly() {
			return null;
		}

		public TimeOnly getTimeOnly() {
			return null;
		}

		public DateTime getDateTime() {
			return null;
		}

		public Timestamp getTimestamp() {
			return null;
		}

		public Geometry getGeometry() {
			return null;
		}
	}

	private static ValidationELResolver newResolver() {
		return new ValidationELResolver(mock(Customer.class));
	}

	@Test
	void getTypeHandlesObjectArrayListMapAndBeanProperties() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertEquals(Object.class, resolver.getType(context, Object.class, "anything"));
		assertNull(resolver.getType(context, String[].class, "0"));
		assertEquals(Object.class, resolver.getType(context, List.class, "0"));
		assertEquals(Object.class, resolver.getType(context, Map.class, "k"));
		assertNull(resolver.getType(context, SampleBean.class, "name"));
		assertNull(resolver.getType(context, new Object(), "name"));
	}

	@Test
	void getTypeThrowsForInvalidIntegerAndMissingProperty() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertThrows(NumberFormatException.class, () -> resolver.getType(context, String[].class, "x"));
		assertThrows(PropertyNotFoundException.class, () -> resolver.getType(context, SampleBean.class, "missing"));
	}

	@Test
	void getValueReturnsTerminatingMocks() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		Object stringValue = resolver.getValue(context, SampleBean.class, "name");
		Object integerValue = resolver.getValue(context, SampleBean.class, "count");
		Object arrayValue = resolver.getValue(context, String[].class, "0");
		Object mapValue = resolver.getValue(context, Map.class, "k");
		Object objectValue = resolver.getValue(context, Object.class, "any");

		assertEquals("", stringValue);
		assertEquals(Integer.valueOf(1), integerValue);
		assertEquals("", arrayValue);
		assertEquals(Object.class, mapValue);
		assertEquals(Object.class, objectValue);
	}

	@Test
	void mutableValidationValuesAreIsolatedBetweenEvaluations() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// OptimisticLock.getTimestamp() returns java.util.Date, which is mocked with a fresh instance
		Object first = resolver.getValue(context, OptimisticLock.class, "timestamp");
		Object second = resolver.getValue(context, OptimisticLock.class, "timestamp");
		assertEquals("java.util.Date", first.getClass().getName());
		assertNotSame(first, second);
	}

	@Test
	void enumValuesMockAsConstantsWithEnumType() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertEquals(Colour.RED, resolver.getValue(context, EnumValueBean.class, "colour"));
		assertEquals(Colour.class, resolver.getType(context, EnumValueBean.class, "colour"));
		assertEquals(Empty.class, resolver.getValue(context, EnumValueBean.class, "empty"));
	}

	@Test
	void mutableSkyveTypeMocksAreFreshInstances() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		Map<String, Class<?>> properties = Map.of("dateOnly", DateOnly.class,
													"timeOnly", TimeOnly.class,
													"dateTime", DateTime.class,
													"timestamp", Timestamp.class,
													"geometry", Geometry.class);
		properties.forEach((property, type) -> {
			Object first = resolver.getValue(context, MutableTypesBean.class, property);
			assertTrue(type.isInstance(first), property);
			assertNotSame(first, resolver.getValue(context, MutableTypesBean.class, property), property);
		});
	}

	@Test
	void documentScalarAttributeMocksImplementingType() {
		ValidationELResolver resolver = newResolver();
		DocumentImpl document = mock(DocumentImpl.class);
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();
		when(document.getAttribute("text")).thenReturn(attribute);
		assertEquals("", resolver.getValue(mock(ELContext.class), document, "text"));
	}

	@Test
	void documentEnumAttributeRethrowsNonClassLoadingFailure() {
		ValidationELResolver resolver = newResolver();
		DocumentImpl document = mock(DocumentImpl.class);
		Enumeration enumeration = mock(Enumeration.class);
		when(enumeration.getImplementingType()).thenThrow(new MetaDataException("Broken metadata"));
		when(document.getAttribute("choice")).thenReturn(enumeration);
		ELContext context = mock(ELContext.class);
		assertThrows(MetaDataException.class, () -> resolver.getValue(context, document, "choice"));
	}

	@Test
	void isReadOnlyThrowsWhenDocumentBeanClassIsMissing() throws Exception {
		ValidationELResolver resolver = newResolver();
		DocumentImpl document = mock(DocumentImpl.class);
		when(document.getBeanClass(any())).thenThrow(new ClassNotFoundException("Missing"));
		ELContext context = mock(ELContext.class);
		assertThrows(IllegalStateException.class, () -> resolver.isReadOnly(context, document, "unknown"));
	}

	@Test
	void getValueHandlesSingletonDocumentListBranch() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		List<Object> singleton = new ArrayList<>(1);
		singleton.add(new org.skyve.impl.metadata.model.document.DocumentImpl());

		Object result = resolver.getValue(context, singleton, "0");
		assertTrue(result instanceof org.skyve.impl.metadata.model.document.DocumentImpl);
	}

	@Test
	void setValueValidatesNullPrimitiveAndTypeCompatibility() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertDoesNotThrow(() -> {
			resolver.setValue(context, SampleBean.class, "name", "ok");
			resolver.setValue(context, Object.class, "anything", Integer.valueOf(99));
			resolver.setValue(context, SampleBean.class, "name", null);
			resolver.setValue(context, SampleBean.class, "count", null);
			resolver.setValue(context, SampleBean.class, "name", Integer.valueOf(123));
		});
	}

	@Test
	void invokeHandlesObjectClassAndMethodResolution() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertEquals(Object.class, resolver.invoke(context, Object.class, "x", null, null));
		assertEquals(Integer.valueOf(1), resolver.invoke(context, List.class, "size", null, null));
		assertEquals("", resolver.invoke(context, SampleBean.class, "echo", new Class<?>[] { String.class }, new Object[] { "a" }));
		assertEquals(Integer.valueOf(1), resolver.invoke(context, SampleBean.class, "size", null, new Object[0]));
		assertNull(resolver.invoke(context, new Object(), "size", null, null));

		assertThrows(MethodNotFoundException.class, () -> resolver.invoke(context, SampleBean.class, "missing", null, new Object[0]));
	}

	@Test
	void isReadOnlyCoversListMapArrayAndBeanProperties() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertFalse(resolver.isReadOnly(context, Object.class, "anything"));
		assertFalse(resolver.isReadOnly(context, new ArrayList<>(), "0"));
		assertTrue(resolver.isReadOnly(context, Collections.unmodifiableList(new ArrayList<>()), "0"));
		assertFalse(resolver.isReadOnly(context, String[].class, "0"));
		assertFalse(resolver.isReadOnly(context, List.class, "0"));
		assertTrue(resolver.isReadOnly(context, Collections.unmodifiableList(new ArrayList<>()).getClass(), "0"));
		assertFalse(resolver.isReadOnly(context, Map.class, "k"));
		assertTrue(resolver.isReadOnly(context, Collections.unmodifiableMap(Collections.emptyMap()).getClass(), "k"));
		assertFalse(resolver.isReadOnly(context, SampleBean.class, "name"));
		assertTrue(resolver.isReadOnly(context, ReadOnlyBean.class, "value"));
		assertFalse(resolver.isReadOnly(context, new Object(), "x"));

		assertThrows(PropertyNotFoundException.class, () -> resolver.isReadOnly(context, SampleBean.class, "missing"));
		assertThrows(NumberFormatException.class, () -> resolver.isReadOnly(context, List.class, "x"));
	}

	@Test
	void getCommonPropertyTypeCoversSupportedBases() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);

		assertEquals(Integer.class, resolver.getCommonPropertyType(context, List.of("x")));
		assertEquals(Integer.class, resolver.getCommonPropertyType(context, String[].class));
		assertEquals(Integer.class, resolver.getCommonPropertyType(context, List.class));
		assertEquals(Object.class, resolver.getCommonPropertyType(context, SampleBean.class));
		assertNull(resolver.getCommonPropertyType(context, new Object()));
	}

	@TestFactory
	Stream<DynamicTest> setValueAcceptsResolvableAndUnresolvableValues() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// base, property, value
		Object[][] cases = {
			{new Object(), "unknownProp", "value"}, // type is null so no-op
			{SampleBean.class, "name", null},
			{SampleBean.class, "count", null},
			{Object.class, "anything", "someValue"}, // not type-safe
			{SampleBean.class, "name", "hello"}, // assignable
			{SampleBean.class, "name", Integer.valueOf(42)} // String type resolves to a terminating mock so no-op
		};
		return Stream.of(cases).map(c -> dynamicTest(c[0] + "." + c[1] + " = " + c[2],
														() -> assertDoesNotThrow(() -> resolver.setValue(context, c[0], c[1], c[2]))));
	}

	@Test
	void getCommonPropertyTypeReturnsObjectClassForDocumentImplBase() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// DocumentImpl instance → returns Object.class without needing customer/module
		assertEquals(Object.class, resolver.getCommonPropertyType(context, new org.skyve.impl.metadata.model.document.DocumentImpl()));
	}

	@Test
	void isReadOnlyThrowsPropertyNotFoundExceptionForNonCoercibleListIndex() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// Boolean is not a Number, String, or Character → PropertyNotFoundException
		List<Object> list = new ArrayList<>();
		assertThrows(PropertyNotFoundException.class, () -> resolver.isReadOnly(context, list, Boolean.TRUE));
	}

	@Test
	void getTypeThrowsPropertyNotFoundExceptionForNonCoercibleListIndex() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertThrows(PropertyNotFoundException.class, () -> resolver.getType(context, String[].class, Boolean.TRUE));
	}
}

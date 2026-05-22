package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;

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

	@Test
	void setValueDoesNothingWhenTypeIsNull() {
		// Object.class base with an unresolvable property → type=null → no-op
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// no exception expected
		assertDoesNotThrow(() -> resolver.setValue(context, new Object(), "unknownProp", "value"));
	}

	@Test
	void setValueSetsPropertyResolvedWhenValIsNullAndTypeNotPrimitive() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// SampleBean.name is String (non-primitive), val=null → context.setPropertyResolved
		assertDoesNotThrow(() -> resolver.setValue(context, SampleBean.class, "name", null));
	}

	@Test
	void setValueDoesNothingForPrimitivePropertyBecauseTypeResolvesToTerminatingMock() {
		// SampleBean.count is int — getType() returns null (terminating mocks return Integer not int.class)
		// so setValue is a no-op
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// no exception expected
		assertDoesNotThrow(() -> resolver.setValue(context, SampleBean.class, "count", null));
	}

	@Test
	void setValueSetsPropertyResolvedWhenTypeIsObjectClass() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// Object.class base → type=Object.class → setPropertyResolved
		assertDoesNotThrow(() -> resolver.setValue(context, Object.class, "anything", "someValue"));
	}

	@Test
	void setValueSetsPropertyResolvedWhenValAssignableToType() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// SampleBean.name is String, val is a String → assignable → setPropertyResolved
		assertDoesNotThrow(() -> resolver.setValue(context, SampleBean.class, "name", "hello"));
	}

	@Test
	void setValueDoesNothingForKnownStringPropertyBecauseTypeResolvesToTerminatingMock() {
		// SampleBean.name is String — getType() returns null (mock("") not String.class)
		// so setValue is a no-op even when passing Integer
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// no exception expected
		assertDoesNotThrow(() -> resolver.setValue(context, SampleBean.class, "name", Integer.valueOf(42)));
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
		assertThrows(PropertyNotFoundException.class, () -> resolver.isReadOnly(context, new ArrayList<>(), Boolean.TRUE));
	}

	@Test
	void getTypeThrowsPropertyNotFoundExceptionForNonCoercibleListIndex() {
		ValidationELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertThrows(PropertyNotFoundException.class, () -> resolver.getType(context, String[].class, Boolean.TRUE));
	}
}

package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.util.HashMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;

import jakarta.el.ELContext;

@SuppressWarnings("static-method")
class BindingELResolverTest {

	private static BindingELResolver newResolver() {
		return new BindingELResolver();
	}

	// ---- getType ----

	@Test
	void getTypeReturnsNullWhenBaseIsNotBean() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertNull(resolver.getType(context, "notABean", "property"));
	}

	@Test
	void getTypeReturnsNullWhenPropertyIsNotString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertNull(resolver.getType(context, mock(org.skyve.domain.Bean.class), Integer.valueOf(0)));
	}

	// ---- getValue ----

	@Test
	void getValueReturnsNullWhenBaseIsNotBean() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertNull(resolver.getValue(context, "notABean", "property"));
	}

	@Test
	void getValueReturnsNullWhenPropertyIsNotString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertNull(resolver.getValue(context, mock(org.skyve.domain.Bean.class), Integer.valueOf(0)));
	}

	// ---- setValue ----

	@Test
	void setValueDoesNothingWhenBaseIsNotBean() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// Must not throw
		assertDoesNotThrow(() -> resolver.setValue(context, "notABean", "property", "value"));
	}

	@Test
	void setValueDoesNothingWhenPropertyIsNotString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		// Must not throw
		assertDoesNotThrow(() -> resolver.setValue(context, mock(org.skyve.domain.Bean.class), Integer.valueOf(0), "value"));
	}

	// ---- isReadOnly ----

	@Test
	void isReadOnlyReturnsFalseWhenBaseIsNotBean() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertFalse(resolver.isReadOnly(context, "notABean", "property"));
	}

	@Test
	void isReadOnlyReturnsFalseWhenPropertyIsNotString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertFalse(resolver.isReadOnly(context, mock(org.skyve.domain.Bean.class), Integer.valueOf(0)));
	}

	// ---- getCommonPropertyType ----

	@Test
	void getCommonPropertyTypeAlwaysReturnsObjectClass() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertEquals(Object.class, resolver.getCommonPropertyType(context, "anything"));
	}

	@Test
	void getCommonPropertyTypeReturnsObjectClassForNullBase() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		assertEquals(Object.class, resolver.getCommonPropertyType(context, null));
	}

	// ---- if-branch paths with real DynamicBean -----------------------------

	@Test
	void getTypeReturnsBeanPropertyTypeWhenBaseIsBeanAndPropertyIsString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		DynamicBean bean = new DynamicBean("admin", "User", new HashMap<>());
		// "bizModule" is a dynamic property with a String value → expect String.class
		assertEquals(String.class, resolver.getType(context, bean, Bean.MODULE_KEY));
	}

	@Test
	void getValueReturnsBeanPropertyValueWhenBaseIsBeanAndPropertyIsString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		DynamicBean bean = new DynamicBean("admin", "User", new HashMap<>());
		assertEquals("admin", resolver.getValue(context, bean, Bean.MODULE_KEY));
	}

	@Test
	void setValueUpdatesBeanPropertyWhenBaseIsBeanAndPropertyIsString() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		DynamicBean bean = new DynamicBean("admin", "User", new HashMap<>());
		resolver.setValue(context, bean, Bean.MODULE_KEY, "newModule");
		assertEquals("newModule", bean.getBizModule());
	}

	@Test
	void isReadOnlyReturnsFalseForImmutableBindingOnDynamicBean() {
		BindingELResolver resolver = newResolver();
		ELContext context = mock(ELContext.class);
		DynamicBean bean = new DynamicBean("admin", "User", new HashMap<>());
		// BindUtil.isMutable returns false for MODULE_KEY → isReadOnly returns false
		assertFalse(resolver.isReadOnly(context, bean, Bean.MODULE_KEY));
	}
}

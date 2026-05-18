package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class BeanValidatorTest {

	// ---- validateBeanPropertyAgainstAttribute(User, Attribute, Bean, ValidationException) ----

	@Test
	void validateBeanPropertyDelegatesToValidationUtilWithoutRuntime() {
		// Attribute that is not ConvertibleField, not required, not inverseOne/Many
		Attribute attr = mock(Attribute.class);
		when(attr.getName()).thenReturn(Bean.MODULE_KEY);
		when(attr.getAttributeType()).thenReturn(AttributeType.text);
		when(attr.isRequired()).thenReturn(false);

		// DynamicBean has MODULE_KEY in its map so BindUtil.get will work
		Map<String, Object> props = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "User", props);

		User user = mock(User.class);
		ValidationException e = new ValidationException();

		// Should complete without throwing
		assertDoesNotThrow(() -> BeanValidator.validateBeanPropertyAgainstAttribute(user, attr, bean, e));
		// No errors should be added since attribute is not required and value is present
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void validateBeanPropertyAddsMessageForMissingRequiredField() {
		Attribute attr = mock(Attribute.class);
		when(attr.getName()).thenReturn("someField");
		when(attr.getAttributeType()).thenReturn(AttributeType.text);
		when(attr.isRequired()).thenReturn(true);
		when(attr.getLocalisedDisplayName()).thenReturn("Some Field");

		// DynamicBean: "someField" is not in properties → BindUtil.get returns null → required validation fails
		Map<String, Object> props = new HashMap<>();
		props.put("someField", null);
		DynamicBean bean = new DynamicBean("admin", "User", props);

		User user = mock(User.class);
		ValidationException e = new ValidationException();

		BeanValidator.validateBeanPropertyAgainstAttribute(user, attr, bean, e);
		assertEquals(1, e.getMessages().size());
	}

	@Test
	void validateBeanPropertySkipsInverseOneAttribute() {
		Attribute attr = mock(Attribute.class);
		when(attr.getName()).thenReturn("inverseAttr");
		when(attr.getAttributeType()).thenReturn(AttributeType.inverseOne);

		Map<String, Object> props = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "User", props);

		User user = mock(User.class);
		ValidationException e = new ValidationException();

		// inverseOne → early return, no processing
		assertDoesNotThrow(() -> BeanValidator.validateBeanPropertyAgainstAttribute(user, attr, bean, e));
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void validateBeanPropertySkipsInverseManyAttribute() {
		Attribute attr = mock(Attribute.class);
		when(attr.getName()).thenReturn("inverseMany");
		when(attr.getAttributeType()).thenReturn(AttributeType.inverseMany);

		Map<String, Object> props = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "User", props);

		User user = mock(User.class);
		ValidationException e = new ValidationException();

		assertDoesNotThrow(() -> BeanValidator.validateBeanPropertyAgainstAttribute(user, attr, bean, e));
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void validateBeanPropertySkipsBizKeyBinding() {
		Attribute attr = mock(Attribute.class);
		when(attr.getName()).thenReturn(Bean.BIZ_KEY);
		when(attr.getAttributeType()).thenReturn(AttributeType.text);

		Map<String, Object> props = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "User", props);

		User user = mock(User.class);
		ValidationException e = new ValidationException();

		// BIZ_KEY binding → early return
		assertDoesNotThrow(() -> BeanValidator.validateBeanPropertyAgainstAttribute(user, attr, bean, e));
		assertEquals(0, e.getMessages().size());
	}

	// ---- processMessageBindings(MessageException, String) -------------------

	@Test
	void processMessageBindingsPrependsPrefixToMessageBindings() {
		Message msg = new Message("someField", "error text");
		ValidationException e = new ValidationException();
		e.getMessages().add(msg);

		BeanValidator.processMessageBindings(e, "contact");

		// setBindingPrefix prepends "contact." to each binding in the message
		assertEquals("contact.someField", msg.getBindings().iterator().next());
	}

	@Test
	void processMessageBindingsHandlesEmptyMessageException() {
		ValidationException e = new ValidationException();
		// No messages → should not throw
		assertDoesNotThrow(() -> BeanValidator.processMessageBindings(e, "prefix"));
	}

	@Test
	void processMessageBindingsAppliesPrefixToMultipleMessages() {
		Message msg1 = new Message("field1", "error1");
		Message msg2 = new Message("field2", "error2");
		ValidationException e = new ValidationException();
		e.getMessages().add(msg1);
		e.getMessages().add(msg2);

		BeanValidator.processMessageBindings(e, "parent");

		assertEquals("parent.field1", msg1.getBindings().iterator().next());
		assertEquals("parent.field2", msg2.getBindings().iterator().next());
	}

	// ---- validateBeanAgainstDocument(Document, Bean) constant string tests ---

	@Test
	void beanValidatorConstantsHaveExpectedValues() {
		assertEquals("validation.required", BeanValidator.VALIDATION_REQUIRED_KEY);
		assertEquals("validation.format", BeanValidator.VALIDATION_FORMAT_KEY);
		assertEquals("validation.length", BeanValidator.VALIDATION_LENGTH_KEY);
		assertEquals("validation.access", BeanValidator.VALIDATION_ACCESS_KEY);
	}
}

package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
class ValidationUtilTest {

	@Mock
	private User user;

	@Mock
	private Bean bean;

	@Test
	@SuppressWarnings("static-method")
	void validateBeanPropertyInverseOneSkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.inverseOne);
		Mockito.when(attribute.getName()).thenReturn("someInverse");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertThat(e.getMessages().isEmpty(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateBeanPropertyInverseManySkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.inverseMany);
		Mockito.when(attribute.getName()).thenReturn("someInverseMany");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertThat(e.getMessages().isEmpty(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateBeanPropertyBizKeyBindingSkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		// Use a non-inverse type but the special bizKey binding name
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(attribute.getName()).thenReturn(Bean.BIZ_KEY);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertThat(e.getMessages().isEmpty(), is(true));
	}
}

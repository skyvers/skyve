package org.skyve.impl.metadata.model.document.field.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Date;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.user.SuperUser;

@SuppressWarnings("java:S8692") // system clock OK
class DateValidatorTest {

	private DateValidator validator;
	private SuperUser user;

	@BeforeEach
	void before() {
		validator = new DateValidator();
		user = new SuperUser();
	}

	@Test
	void validateNullValueDoesNotAddMessage() {
		ValidationException e = new ValidationException();
		validator.validate(user, null, "binding", "DateField", null, e);
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void validateNoRangeDoesNotAddMessage() {
		ValidationException e = new ValidationException();
		validator.validate(user, new Date(), "binding", "DateField", null, e);
		assertEquals(0, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateMinRangeInvalidAddMessage() {
		Date min = new Date(2000L);
		Date value = new Date(1000L); // before min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(1, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateMinRangeValidNoMessage() {
		Date min = new Date(1000L);
		Date value = new Date(2000L); // after min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(0, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateMaxRangeInvalidAddMessage() {
		Date max = new Date(1000L);
		Date value = new Date(2000L); // after max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(1, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateMaxRangeValidNoMessage() {
		Date max = new Date(2000L);
		Date value = new Date(1000L); // before max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(0, e.getMessages().size());
	}

	@Test
	void validateMinMaxRangeInvalidBelowMin() {
		Date min = new Date(2000L);
		Date max = new Date(5000L);
		Date value = new Date(1000L); // before min

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(1, e.getMessages().size());
	}

	@Test
	void validateMinMaxRangeInvalidAboveMax() {
		Date min = new Date(1000L);
		Date max = new Date(3000L);
		Date value = new Date(4000L); // after max

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(1, e.getMessages().size());
	}

	@Test
	void validateMinMaxRangeValidInRange() {
		Date min = new Date(1000L);
		Date max = new Date(5000L);
		Date value = new Date(3000L); // between min and max

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertEquals(0, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateExactlyMinValueIsValid() {
		Date min = new Date(2000L);
		Date value = new Date(2000L); // equal to min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		// not before min, so valid
		assertEquals(0, e.getMessages().size());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void validateExactlyMaxValueIsValid() {
		Date max = new Date(3000L);
		Date value = new Date(3000L); // equal to max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		// not after max, so valid
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void constructMessageConverterThrowsWrapsInIllegalStateException() {
		Converter<Date> converter = mock(Converter.class);
		when(converter.toDisplayValue(any(Date.class))).thenThrow(new RuntimeException("test"));
		validator.setMin(new Date(2000L));
		assertThrows(IllegalStateException.class, () -> validator.constructMessage(null, "Field", converter));
	}}

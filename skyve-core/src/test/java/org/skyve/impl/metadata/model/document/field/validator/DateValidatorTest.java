package org.skyve.impl.metadata.model.document.field.validator;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Date;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.user.SuperUser;

class DateValidatorTest {

	private DateValidator validator;
	private SuperUser user;

	@BeforeEach
	void before() {
		validator = new DateValidator();
		user = new SuperUser();
	}

	@Test
	@SuppressWarnings("static-method")
	void validateNullValueDoesNotAddMessage() {
		ValidationException e = new ValidationException();
		validator.validate(user, null, "binding", "DateField", null, e);
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateNoRangeDoesNotAddMessage() {
		ValidationException e = new ValidationException();
		validator.validate(user, new Date(), "binding", "DateField", null, e);
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinRangeInvalidAddMessage() {
		Date min = new Date(2000L);
		Date value = new Date(1000L); // before min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinRangeValidNoMessage() {
		Date min = new Date(1000L);
		Date value = new Date(2000L); // after min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMaxRangeInvalidAddMessage() {
		Date max = new Date(1000L);
		Date value = new Date(2000L); // after max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMaxRangeValidNoMessage() {
		Date max = new Date(2000L);
		Date value = new Date(1000L); // before max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinMaxRangeInvalidBelowMin() {
		Date min = new Date(2000L);
		Date max = new Date(5000L);
		Date value = new Date(1000L); // before min

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinMaxRangeInvalidAboveMax() {
		Date min = new Date(1000L);
		Date max = new Date(3000L);
		Date value = new Date(4000L); // after max

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinMaxRangeValidInRange() {
		Date min = new Date(1000L);
		Date max = new Date(5000L);
		Date value = new Date(3000L); // between min and max

		validator.setMin(min);
		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateExactlyMinValueIsValid() {
		Date min = new Date(2000L);
		Date value = new Date(2000L); // equal to min

		validator.setMin(min);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		// not before min, so valid
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateExactlyMaxValueIsValid() {
		Date max = new Date(3000L);
		Date value = new Date(3000L); // equal to max

		validator.setMax(max);

		ValidationException e = new ValidationException();
		validator.validate(user, value, "binding", "DateField", null, e);

		// not after max, so valid
		assertThat(e.getMessages().size(), is(0));
	}
}

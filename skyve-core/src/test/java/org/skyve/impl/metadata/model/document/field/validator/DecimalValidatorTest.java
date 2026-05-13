package org.skyve.impl.metadata.model.document.field.validator;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.metadata.user.SuperUser;

class DecimalValidatorTest {

	private DecimalValidator validator;
	private SuperUser user;

	@BeforeEach
	void before() {
		validator = new DecimalValidator();
		user = new SuperUser();
	}

	@Test
	@SuppressWarnings("static-method")
	void validateNullValueDoesNotAddMessage() {
		ValidationException e = new ValidationException();
		validator.validate(user, null, "binding", "Binding", null, e);
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void validateMinRangeInvalid() {
		validator.setMin(new Decimal2(10.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(5.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinRangeValid() {
		validator.setMin(new Decimal2(10.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(15.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMaxRangeInvalid() {
		validator.setMax(new Decimal2(100.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(150.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMaxRangeValid() {
		validator.setMax(new Decimal2(100.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(50.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinMaxRangeInvalid() {
		validator.setMin(new Decimal2(10.0));
		validator.setMax(new Decimal2(100.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(200.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateMinMaxRangeValid() {
		validator.setMin(new Decimal2(10.0));
		validator.setMax(new Decimal2(100.0));

		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(50.0), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void validatePrecisionMismatchAddsMessage() {
		validator.setPrecision(Integer.valueOf(2));

		// Decimal5 has scale 5, not 2
		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal5(1.5), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(1));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void validatePrecisionMatchDoesNotAddMessage() {
		validator.setPrecision(Integer.valueOf(2));

		// Decimal2 has scale 2
		ValidationException e = new ValidationException();
		validator.validate(user, new Decimal2(1.5), "binding", "Amount", null, e);

		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void constructPrecisionMessageContainsFieldName() {
		validator.setPrecision(Integer.valueOf(2));
		String message = validator.constructPrecisionMessage("MyField");
		assertThat(message, containsString("MyField"));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void getPrecisionNullByDefault() {
		assertThat(validator.getPrecision(), is((Integer) null));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void setPrecisionAndGet() {
		validator.setPrecision(Integer.valueOf(5));
		assertThat(validator.getPrecision(), is(Integer.valueOf(5)));
	}
}

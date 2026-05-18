package org.skyve.impl.metadata.model.document.field.validator;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.user.SuperUser;

class IntegerValidatorTest {

	private RangeValidator<Integer> validator;
	private SuperUser user;

	@BeforeEach
	void before() {
		validator = new IntegerValidator();
		user = new SuperUser();
	}

	@Test
	void testValidateMinRangeInvalid() {
		// setup the test data
		validator.setMin(Integer.valueOf(1));

		Integer value = Integer.valueOf(0);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(1, e.getMessages().size());
		assertThat(e.getMessages().get(0).getText(), containsString(" less than "));
	}

	@Test
	void testValidateMinRangeValid() {
		// setup the test data
		validator.setMin(Integer.valueOf(1));

		Integer value = Integer.valueOf(2);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void testValidateMaxRangeInvalid() {
		// setup the test data
		validator.setMax(Integer.valueOf(1));

		Integer value = Integer.valueOf(2);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(1, e.getMessages().size());
		assertThat(e.getMessages().get(0).getText(), containsString(" greater than "));
	}

	@Test
	void testValidateMaxRangeValid() {
		// setup the test data
		validator.setMax(Integer.valueOf(1));

		Integer value = Integer.valueOf(0);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(0, e.getMessages().size());
	}

	@Test
	void testValidateMinMaxRangeInValid() {
		// setup the test data
		validator.setMin(Integer.valueOf(0));
		validator.setMax(Integer.valueOf(2));

		Integer value = Integer.valueOf(3);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(1, e.getMessages().size());
		assertThat(e.getMessages().get(0).getText(), containsString(" between "));
	}

	@Test
	void testValidateMinMaxRangeValid() {
		// setup the test data
		validator.setMin(Integer.valueOf(0));
		validator.setMax(Integer.valueOf(2));

		Integer value = Integer.valueOf(1);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertEquals(0, e.getMessages().size());
	}

}

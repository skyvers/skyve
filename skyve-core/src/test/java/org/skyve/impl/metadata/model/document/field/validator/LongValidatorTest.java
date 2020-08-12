package org.skyve.impl.metadata.model.document.field.validator;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.user.SuperUser;

public class LongValidatorTest {

	private RangeValidator<Long> validator;
	private SuperUser user;

	@Before
	public void before() {
		validator = new LongValidator();
		user = new SuperUser();
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMinRangeInvalid() throws Exception {
		// setup the test data
		validator.setMin(Long.valueOf(1));

		Long value = Long.valueOf(0);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), containsString("less than"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMinRangeValid() throws Exception {
		// setup the test data
		validator.setMin(Long.valueOf(1));

		Long value = Long.valueOf(2);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMaxRangeInvalid() throws Exception {
		// setup the test data
		validator.setMax(Long.valueOf(1));

		Long value = Long.valueOf(2);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), containsString("greater than"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMaxRangeValid() throws Exception {
		// setup the test data
		validator.setMax(Long.valueOf(1));

		Long value = Long.valueOf(0);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(0));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMinMaxRangeInValid() throws Exception {
		// setup the test data
		validator.setMin(Long.valueOf(0));
		validator.setMax(Long.valueOf(2));

		Long value = Long.valueOf(3);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(1));
		assertThat(e.getMessages().get(0).getText(), containsString("less than"));
		assertThat(e.getMessages().get(0).getText(), containsString("greater than"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testValidateMinMaxRangeValid() throws Exception {
		// setup the test data
		validator.setMin(Long.valueOf(0));
		validator.setMax(Long.valueOf(2));

		Long value = Long.valueOf(1);

		ValidationException e = new ValidationException();

		// call the method under test
		validator.validate(user, value, "binding", "Binding", null, e);

		// verify the result
		assertThat(e.getMessages().size(), is(0));
	}

}

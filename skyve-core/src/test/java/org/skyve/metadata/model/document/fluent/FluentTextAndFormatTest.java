package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;

@SuppressWarnings("static-method")
class FluentTextAndFormatTest {

	// ---- FluentText ----

	@Test
	void textDefaultConstructorCreatesInstance() {
		assertThat(new FluentText().get(), is(notNullValue()));
	}

	@Test
	void textWrappingConstructorUsesInstance() {
		Text t = new Text();
		assertThat(new FluentText(t).get(), is(t));
	}

	@Test
	void textLengthSetsValue() {
		assertEquals(100, new FluentText().length(100).get().getLength());
	}

	@Test
	void textFormatSetsValue() {
		FluentTextFormat fmt = new FluentTextFormat().mask("###");
		assertThat(new FluentText().format(fmt).get().getFormat(), is(notNullValue()));
	}

	@Test
	void textValidatorSetsValue() {
		FluentTextValidator v = new FluentTextValidator().regularExpression("[a-z]+");
		assertThat(new FluentText().validator(v).get().getValidator(), is(notNullValue()));
	}

	@Test
	void textFromCopiesLength() {
		Text src = new Text();
		src.setLength(50);
		assertEquals(50, new FluentText().from(src).get().getLength());
	}

	@Test
	void textFromCopiesFormat() {
		Text src = new Text();
		TextFormat fmt = new TextFormat();
		fmt.setMask("###");
		src.setFormat(fmt);
		assertThat(new FluentText().from(src).get().getFormat(), is(notNullValue()));
	}

	@Test
	void textFromCopiesValidator() {
		Text src = new Text();
		TextValidator v = new TextValidator();
		v.setType(ValidatorType.email);
		src.setValidator(v);
		assertThat(new FluentText().from(src).get().getValidator(), is(notNullValue()));
	}

	// ---- FluentTextFormat ----

	@Test
	void textFormatDefaultConstructorCreatesInstance() {
		assertThat(new FluentTextFormat().get(), is(notNullValue()));
	}

	@Test
	void textFormatMaskSetsValue() {
		assertThat(new FluentTextFormat().mask("###-###").get().getMask(), is("###-###"));
	}

	@Test
	void textFormatTextCaseSetsValue() {
		assertThat(new FluentTextFormat().textCase(TextCase.upper).get().getCase(), is(TextCase.upper));
	}

	@Test
	void textFormatFromCopiesMask() {
		TextFormat src = new TextFormat();
		src.setMask("AAA");
		assertThat(new FluentTextFormat().from(src).get().getMask(), is("AAA"));
	}

	@Test
	void textFormatFromCopiesTextCase() {
		TextFormat src = new TextFormat();
		src.setCase(TextCase.lower);
		assertThat(new FluentTextFormat().from(src).get().getCase(), is(TextCase.lower));
	}

	// ---- FluentTextValidator ----

	@Test
	void textValidatorDefaultConstructorCreatesInstance() {
		assertThat(new FluentTextValidator().get(), is(notNullValue()));
	}

	@Test
	void textValidatorTypeSetsValue() {
		assertThat(new FluentTextValidator().type(ValidatorType.email).get().getType(), is(ValidatorType.email));
	}

	@Test
	void textValidatorRegularExpressionSetsValue() {
		assertThat(new FluentTextValidator().regularExpression("[a-z]+").get().getRegularExpression(), is("[a-z]+"));
	}

	@Test
	void textValidatorValidationMessageSetsValue() {
		assertThat(new FluentTextValidator().validationMessage("Invalid email").get().getValidationMessage(), is("Invalid email"));
	}

	@Test
	void textValidatorFromCopiesType() {
		TextValidator src = new TextValidator();
		src.setType(ValidatorType.url);
		assertThat(new FluentTextValidator().from(src).get().getType(), is(ValidatorType.url));
	}

	@Test
	void textValidatorFromCopiesRegularExpression() {
		TextValidator src = new TextValidator();
		src.setRegularExpression("[A-Z]+");
		assertThat(new FluentTextValidator().from(src).get().getRegularExpression(), is("[A-Z]+"));
	}
}

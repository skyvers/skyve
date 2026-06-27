package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;

@SuppressWarnings("static-method")
class FluentTemporalAndNumericAttributeTest {

	// ---- FluentDate ----

	@Test
	void dateDefaultConstructorCreatesInstance() {
		assertThat(new FluentDate().get(), is(notNullValue()));
	}

	@Test
	void dateWrappingConstructorUsesInstance() {
		Date d = new Date();
		assertThat(new FluentDate(d).get(), is(d));
	}

	@Test
	void dateValidatorSetsValue() {
		DateValidator v = new DateValidator();
		assertThat(new FluentDate().validator(v).get().getValidator(), is(v));
	}

	@Test
	void dateFromCopiesValidator() {
		Date src = new Date();
		DateValidator v = new DateValidator();
		src.setValidator(v);
		assertThat(new FluentDate().from(src).get().getValidator(), is(v));
	}

	// ---- FluentDateTime ----

	@Test
	void dateTimeDefaultConstructorCreatesInstance() {
		assertThat(new FluentDateTime().get(), is(notNullValue()));
	}

	@Test
	void dateTimeWrappingConstructorUsesInstance() {
		DateTime dt = new DateTime();
		assertThat(new FluentDateTime(dt).get(), is(dt));
	}

	@Test
	void dateTimeValidatorSetsValue() {
		DateValidator v = new DateValidator();
		assertThat(new FluentDateTime().validator(v).get().getValidator(), is(v));
	}

	@Test
	void dateTimeFromCopiesValidator() {
		DateTime src = new DateTime();
		DateValidator v = new DateValidator();
		src.setValidator(v);
		assertThat(new FluentDateTime().from(src).get().getValidator(), is(v));
	}

	// ---- FluentTime ----

	@Test
	void timeDefaultConstructorCreatesInstance() {
		assertThat(new FluentTime().get(), is(notNullValue()));
	}

	@Test
	void timeWrappingConstructorUsesInstance() {
		Time t = new Time();
		assertThat(new FluentTime(t).get(), is(t));
	}

	@Test
	void timeValidatorSetsValue() {
		DateValidator v = new DateValidator();
		assertThat(new FluentTime().validator(v).get().getValidator(), is(v));
	}

	@Test
	void timeFromCopiesValidator() {
		Time src = new Time();
		DateValidator v = new DateValidator();
		src.setValidator(v);
		assertThat(new FluentTime().from(src).get().getValidator(), is(v));
	}

	// ---- FluentTimestamp ----

	@Test
	void timestampDefaultConstructorCreatesInstance() {
		assertThat(new FluentTimestamp().get(), is(notNullValue()));
	}

	@Test
	void timestampWrappingConstructorUsesInstance() {
		Timestamp ts = new Timestamp();
		assertThat(new FluentTimestamp(ts).get(), is(ts));
	}

	@Test
	void timestampValidatorSetsValue() {
		DateValidator v = new DateValidator();
		assertThat(new FluentTimestamp().validator(v).get().getValidator(), is(v));
	}

	@Test
	void timestampFromCopiesValidator() {
		Timestamp src = new Timestamp();
		DateValidator v = new DateValidator();
		src.setValidator(v);
		assertThat(new FluentTimestamp().from(src).get().getValidator(), is(v));
	}

	// ---- FluentDecimal2 ----

	@Test
	void decimal2DefaultConstructorCreatesInstance() {
		assertThat(new FluentDecimal2().get(), is(notNullValue()));
	}

	@Test
	void decimal2WrappingConstructorUsesInstance() {
		Decimal2 d = new Decimal2();
		assertThat(new FluentDecimal2(d).get(), is(d));
	}

	@Test
	void decimal2ValidatorSetsValue() {
		DecimalValidator v = new DecimalValidator();
		assertThat(new FluentDecimal2().validator(v).get().getValidator(), is(v));
	}

	@Test
	void decimal2FromCopiesValidator() {
		Decimal2 src = new Decimal2();
		DecimalValidator v = new DecimalValidator();
		src.setValidator(v);
		assertThat(new FluentDecimal2().from(src).get().getValidator(), is(v));
	}

	// ---- FluentDecimal5 ----

	@Test
	void decimal5DefaultConstructorCreatesInstance() {
		assertThat(new FluentDecimal5().get(), is(notNullValue()));
	}

	@Test
	void decimal5WrappingConstructorUsesInstance() {
		Decimal5 d = new Decimal5();
		assertThat(new FluentDecimal5(d).get(), is(d));
	}

	@Test
	void decimal5ValidatorSetsValue() {
		DecimalValidator v = new DecimalValidator();
		assertThat(new FluentDecimal5().validator(v).get().getValidator(), is(v));
	}

	@Test
	void decimal5FromCopiesValidator() {
		Decimal5 src = new Decimal5();
		DecimalValidator v = new DecimalValidator();
		src.setValidator(v);
		assertThat(new FluentDecimal5().from(src).get().getValidator(), is(v));
	}

	// ---- FluentDecimal10 ----

	@Test
	void decimal10DefaultConstructorCreatesInstance() {
		assertThat(new FluentDecimal10().get(), is(notNullValue()));
	}

	@Test
	void decimal10WrappingConstructorUsesInstance() {
		Decimal10 d = new Decimal10();
		assertThat(new FluentDecimal10(d).get(), is(d));
	}

	@Test
	void decimal10ValidatorSetsValue() {
		DecimalValidator v = new DecimalValidator();
		assertThat(new FluentDecimal10().validator(v).get().getValidator(), is(v));
	}

	@Test
	void decimal10FromCopiesValidator() {
		Decimal10 src = new Decimal10();
		DecimalValidator v = new DecimalValidator();
		src.setValidator(v);
		assertThat(new FluentDecimal10().from(src).get().getValidator(), is(v));
	}

	// ---- FluentLongInteger ----

	@Test
	void longIntegerDefaultConstructorCreatesInstance() {
		assertThat(new FluentLongInteger().get(), is(notNullValue()));
	}

	@Test
	void longIntegerWrappingConstructorUsesInstance() {
		LongInteger li = new LongInteger();
		assertThat(new FluentLongInteger(li).get(), is(li));
	}

	@Test
	void longIntegerValidatorSetsValue() {
		LongValidator v = new LongValidator();
		assertThat(new FluentLongInteger().validator(v).get().getValidator(), is(v));
	}

	@Test
	void longIntegerFromCopiesValidator() {
		LongInteger src = new LongInteger();
		LongValidator v = new LongValidator();
		src.setValidator(v);
		assertThat(new FluentLongInteger().from(src).get().getValidator(), is(v));
	}
}

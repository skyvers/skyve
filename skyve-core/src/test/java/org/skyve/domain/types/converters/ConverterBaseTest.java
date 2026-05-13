package org.skyve.domain.types.converters;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Format.TextCase;

@SuppressWarnings("static-method")
class ConverterBaseTest {

	// ---- Format ----

	@Test
	void formatGetMask() {
		Format<String> f = new Format<>("###-###", null);
		assertThat(f.getMask(), is("###-###"));
	}

	@Test
	void formatGetTextCaseNull() {
		Format<String> f = new Format<>("###", null);
		assertThat(f.getTextCase(), is(nullValue()));
	}

	@Test
	void formatGetTextCaseUpper() {
		Format<String> f = new Format<>("LLL", TextCase.upper);
		assertThat(f.getTextCase(), is(TextCase.upper));
	}

	@Test
	void formatGetTextCaseLower() {
		Format<String> f = new Format<>("LLL", TextCase.lower);
		assertThat(f.getTextCase(), is(TextCase.lower));
	}

	@Test
	void formatGetTextCaseCapital() {
		Format<String> f = new Format<>(null, TextCase.capital);
		assertThat(f.getTextCase(), is(TextCase.capital));
	}

	@Test
	void formatToDisplayValueNoMask() throws ParseException {
		Format<String> f = new Format<>(null, null);
		assertThat(f.toDisplayValue("hello"), is("hello"));
	}

	@Test
	void formatToDisplayValueNullValueNoMask() throws ParseException {
		Format<String> f = new Format<>(null, null);
		assertThat(f.toDisplayValue(null), is(""));
	}

	@Test
	void formatToDisplayValueUpperCase() throws ParseException {
		Format<String> f = new Format<>(null, TextCase.upper);
		assertThat(f.toDisplayValue("hello"), is("HELLO"));
	}

	@Test
	void formatToDisplayValueLowerCase() throws ParseException {
		Format<String> f = new Format<>(null, TextCase.lower);
		assertThat(f.toDisplayValue("HELLO"), is("hello"));
	}

	@Test
	void formatFromDisplayValueNoMask() throws ParseException {
		Format<String> f = new Format<>(null, null);
		assertThat(f.fromDisplayValue("hello"), is("hello"));
	}

	@Test
	void formatFromDisplayValueUpperCase() throws ParseException {
		Format<String> f = new Format<>(null, TextCase.upper);
		assertThat(f.fromDisplayValue("hello"), is("HELLO"));
	}

	@Test
	void formatFromDisplayValueLowerCase() throws ParseException {
		Format<String> f = new Format<>(null, TextCase.lower);
		assertThat(f.fromDisplayValue("HELLO"), is("hello"));
	}

	// ---- TextCase enum ----

	@Test
	void textCaseValues() {
		assertThat(TextCase.values(), is(notNullValue()));
		assertThat(TextCase.values().length, is(3));
	}

	@Test
	void textCaseUpperNotNull() {
		assertThat(TextCase.upper, is(notNullValue()));
	}

	@Test
	void textCaseLowerNotNull() {
		assertThat(TextCase.lower, is(notNullValue()));
	}

	@Test
	void textCaseCapitalNotNull() {
		assertThat(TextCase.capital, is(notNullValue()));
	}

	@Test
	void textCaseValueOf() {
		assertThat(TextCase.valueOf("upper"), is(TextCase.upper));
	}
}

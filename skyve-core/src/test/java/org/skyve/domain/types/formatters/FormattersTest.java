package org.skyve.domain.types.formatters;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/** Unit tests for formatter classes. */
@SuppressWarnings("static-method")
class FormattersTest {

	// ---- Formatters (registry) ----

	@Test
	void formattersGetByNameReturnsRegisteredFormatter() {
		Formatter<?> f = Formatters.get(FormatterName.DD_MM_YYYY.name());
		assertThat(f, is(notNullValue()));
	}

	@Test
	void formattersGetByUnknownNameReturnsNull() {
		Formatter<?> f = Formatters.get("UNKNOWN_FORMATTER_XYZ");
		assertThat(f, is(nullValue()));
	}

	@Test
	void formattersGetNamesIsNotEmpty() {
		assertThat(Formatters.getNames().isEmpty(), is(false));
	}

	@Test
	void formattersGetNamesContainsKnownFormatter() {
		assertThat(Formatters.getNames().contains(FormatterName.YYYY_MM_DD.name()), is(true));
	}

	@Test
	void formattersRegisterDuplicateThrowsIllegalState() {
		assertThrows(IllegalStateException.class, () -> Formatters.register(FormatterName.DD_MM_YYYY.name(), FormatterName.DD_MM_YYYY.getFormatter()));
	}

	// ---- TimeDurationFormatter ----

	@Test
	void timeDurationFormatterGetValueTypeIsDecimal() {
		TimeDurationFormatter f = new TimeDurationFormatter();
		assertThat(f.getValueType().getSimpleName(), is("Decimal"));
	}

	@Test
	void timeDurationFormatterZeroReturnsZeroZero() {
		TimeDurationFormatter f = new TimeDurationFormatter();
		assertThat(f.toDisplayValue(Decimal5.ZERO), is("0:00"));
	}

	@Test
	void timeDurationFormatterOneAndHalfHours() {
		TimeDurationFormatter f = new TimeDurationFormatter();
		assertThat(f.toDisplayValue(new Decimal5("1.5")), is("1:30"));
	}

	@Test
	void timeDurationFormatterOneHour() {
		TimeDurationFormatter f = new TimeDurationFormatter();
		assertThat(f.toDisplayValue(new Decimal5("1.0")), is("1:00"));
	}

	@Test
	void timeDurationFormatterNegativeHours() {
		TimeDurationFormatter f = new TimeDurationFormatter();
		String result = f.toDisplayValue(new Decimal5("-1.5"));
		assertThat(result, is("-1:30"));
	}

	// ---- StringFormatter ----

	@Test
	void stringFormatterGetValueTypeIsString() {
		StringFormatter f = new StringFormatter(false, false, false, null);
		assertThat(f.getValueType().getSimpleName(), is("String"));
	}

	@Test
	void stringFormatterNoEscapeReturnsValueUnchanged() {
		StringFormatter f = new StringFormatter(false, false, false, null);
		assertThat(f.toDisplayValue("hello world"), is("hello world"));
	}

	@Test
	void stringFormatterEscapeHtmlEscapesSpecialChars() {
		StringFormatter f = new StringFormatter(true, false, false, null);
		String result = f.toDisplayValue("<script>alert('xss')</script>");
		assertThat(result.contains("<script>"), is(false));
	}

	@Test
	void stringFormatterSanitiseOnlyWithNullSanitiseDoesNothing() {
		StringFormatter f = new StringFormatter(false, false, false, null);
		assertThat(f.toDisplayValue("normal text"), is("normal text"));
	}

	// ---- DecimalFormatter ----

	@Test
	void decimalFormatterGetValueTypeIsNumber() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, null);
		assertThat(f.getValueType().getSimpleName(), is("Number"));
	}

	@Test
	void decimalFormatterTwoDecimalPlacesFormatsCorrectly() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, null);
		String result = f.toDisplayValue(1234.5);
		assertThat(result, is(notNullValue()));
		assertThat(result.contains("1,234.50"), is(true));
	}

	@Test
	void decimalFormatterAbsoluteIgnoresSign() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, true, null);
		String negResult = f.toDisplayValue(-1234.5);
		String posResult = f.toDisplayValue(1234.5);
		assertThat(negResult, is(posResult));
	}

	@Test
	void decimalFormatterZeroDecimalPlacesFormatsInteger() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, false, null);
		String result = f.toDisplayValue(42);
		assertThat(result, is("42"));
	}

	// ---- SimpleDateFormatter ----

	@Test
	void simpleDateFormatterGetValueTypeIsDate() {
		SimpleDateFormatter f = new SimpleDateFormatter("dd/MM/yyyy");
		assertThat(f.getValueType().getSimpleName(), is("Date"));
	}
}

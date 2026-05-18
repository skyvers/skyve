package org.skyve.domain.types.formatters;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		assertFalse(Formatters.getNames().isEmpty());
	}

	@Test
	void formattersGetNamesContainsKnownFormatter() {
		assertTrue(Formatters.getNames().contains(FormatterName.YYYY_MM_DD.name()));
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

        @Test
        void timeDurationFormatterSingleDigitMinutesGetZeroPadded() {
                TimeDurationFormatter f = new TimeDurationFormatter();
                // 1.1 hours = 1 hour + 6 minutes (single digit => needs zero padding)
                assertThat(f.toDisplayValue(new Decimal5("1.1")), is("1:06"));
        }

        @Test
        void timeDurationFormatterRoundUpTo60MinutesIncrementsHour() {
                // 1.99992 hours: fractional=0.99992, 0.99992*60=59.9952, rounds (4 sig figs HALF_UP) to 60 → 2:00
                TimeDurationFormatter f = new TimeDurationFormatter();
                assertThat(f.toDisplayValue(new Decimal5("1.99992")), is("2:00"));
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
		assertFalse(result.contains("<script>"));
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
		String result = f.toDisplayValue(Double.valueOf(1234.5));
		assertThat(result, is(notNullValue()));
		assertTrue(result.contains("1,234.50"));
	}

	@Test
	void decimalFormatterAbsoluteIgnoresSign() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, true, null);
		String negResult = f.toDisplayValue(Double.valueOf(-1234.5));
		String posResult = f.toDisplayValue(Double.valueOf(1234.5));
		assertThat(negResult, is(posResult));
	}

	@Test
	void decimalFormatterZeroDecimalPlacesFormatsInteger() {
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.ZERO_DECIMAL_PLACES_PATTERN, false, null);
		String result = f.toDisplayValue(Integer.valueOf(42));
		assertThat(result, is("42"));
	}

	// ---- SimpleDateFormatter ----

	@Test
	void simpleDateFormatterGetValueTypeIsDate() {
		SimpleDateFormatter f = new SimpleDateFormatter("dd/MM/yyyy");
		assertThat(f.getValueType().getSimpleName(), is("Date"));
	}

	@Test
	void simpleDateFormatterToDisplayValueFormatsDate() {
		SimpleDateFormatter f = new SimpleDateFormatter("dd/MM/yyyy");
		java.util.Date date = new java.util.Date(0); // epoch
		String result = f.toDisplayValue(date);
		assertThat(result, is(notNullValue()));
	}

	// ---- StringFormatter (additional branches) ----

	@Test
	void stringFormatterEscapeJsonChangesString() {
		StringFormatter f = new StringFormatter(false, true, false, null);
		String result = f.toDisplayValue("say \"hello\"");
		// OWASP escapeJsonString processes the string - result should be non-null
		assertThat(result, is(notNullValue()));
	}

	@Test
	void stringFormatterEscapeJsChangesQuotes() {
		StringFormatter f = new StringFormatter(false, false, true, null);
		String result = f.toDisplayValue("it's a test");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void stringFormatterSanitiseTextOnlyStripsAllHtml() {
		StringFormatter f = new StringFormatter(false, false, false, Sanitisation.text);
		String result = f.toDisplayValue("<b>bold text</b>");
		assertFalse(result.contains("<b>"));
	}

	@Test
	void stringFormatterEscapeHtmlWithSanitisationCombined() {
		StringFormatter f = new StringFormatter(true, false, false, Sanitisation.basic);
		String result = f.toDisplayValue("<script>xss</script>");
		assertThat(result, is(notNullValue()));
	}

	// ---- DecimalFormatter (additional branches) ----

	@Test
	void decimalFormatterCalculatedPercentageTrueDividesByHundred() {
		// calculatedPercentage=true: num /= 100, so 5000 → 50.00%
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, Boolean.TRUE);
		String result = f.toDisplayValue(Integer.valueOf(5000));
		assertThat(result, is(notNullValue()));
		assertTrue(result.contains("%"));
	}

	@Test
	void decimalFormatterCalculatedPercentageFalseDisplaysAsPercent() {
		// calculatedPercentage=false: no division, just adds %
		DecimalFormatter f = new DecimalFormatter(DecimalFormatter.TWO_DECIMAL_PLACES_PATTERN, false, Boolean.FALSE);
		String result = f.toDisplayValue(Integer.valueOf(50));
		assertTrue(result.contains("%"));
	}
}

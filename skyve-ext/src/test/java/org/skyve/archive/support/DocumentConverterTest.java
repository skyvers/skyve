package org.skyve.archive.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Date;

import org.junit.jupiter.api.Test;

@SuppressWarnings("java:S8692") // system clock OK
class DocumentConverterTest {

	@Test
	@SuppressWarnings("static-method")
	void testDateToStringReturnsNonNull() {
		// covers DocumentConverter.dateToString@31
		String result = DocumentConverter.dateToString(new Date());
		assertThat(result, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testStringToDateRoundTrip() {
		// covers DocumentConverter.stringToDate@36
		Date original = new Date();
		String encoded = DocumentConverter.dateToString(original);
		Date decoded = DocumentConverter.stringToDate(encoded);
		assertThat(decoded, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testStringToDateInvalidStringThrowsRuntimeException() {
		// covers DocumentConverter.stringToDate@38 (ParseException path)
		assertThrows(RuntimeException.class, () -> DocumentConverter.stringToDate("not-a-date"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToSortBindingAppendsSortSuffix() {
		// covers DocumentConverter.toSortBinding@60
		String result = DocumentConverter.toSortBinding("myField");
		assertThat(result, is("myField_sort"));
	}

}

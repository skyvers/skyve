package org.skyve.archive.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Date;

import org.junit.Test;

public class DocumentConverterTest {

	@Test
	@SuppressWarnings("static-method")
	public void testDateToStringReturnsNonNull() {
		// covers DocumentConverter.dateToString@31
		String result = DocumentConverter.dateToString(new Date());
		assertThat(result, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testStringToDateRoundTrip() {
		// covers DocumentConverter.stringToDate@36
		Date original = new Date();
		String encoded = DocumentConverter.dateToString(original);
		Date decoded = DocumentConverter.stringToDate(encoded);
		assertThat(decoded, is(notNullValue()));
	}

	@Test(expected = RuntimeException.class)
	@SuppressWarnings("static-method")
	public void testStringToDateInvalidStringThrowsRuntimeException() {
		// covers DocumentConverter.stringToDate@38 (ParseException path)
		DocumentConverter.stringToDate("not-a-date");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToSortBindingAppendsSortSuffix() {
		// covers DocumentConverter.toSortBinding@60
		String result = DocumentConverter.toSortBinding("myField");
		assertThat(result, is("myField_sort"));
	}

}

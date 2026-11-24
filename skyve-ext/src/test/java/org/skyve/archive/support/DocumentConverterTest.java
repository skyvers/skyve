package org.skyve.archive.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.withSettings;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.ModuleDocument;

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

	@Test
	@SuppressWarnings("static-method")
	void typedHandlesDelegatesToTheStringForm() {
		DocumentConverter converter = mock(DocumentConverterDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		doReturn(Boolean.TRUE).when(converter).handles("sales", "Order");

		assertAll(() -> assertTrue(converter.handles(SalesDocument.ORDER)),
					() -> verify(converter).handles("sales", "Order"));
	}

	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class DocumentConverterDouble implements DocumentConverter {
		// no state
	}

	private enum SalesDocument implements ModuleDocument {
		ORDER;

		@Override
		public String moduleName() {
			return "sales";
		}

		@Override
		public String documentName() {
			return "Order";
		}
	}
}

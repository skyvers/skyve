package org.skyve.impl.web.faces.converters.date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;

import jakarta.faces.convert.ConverterException;

public class DateConverterTest {

	// ---- DD_MM_YYYY ----

	@Test
	void ddMmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsStringFormatsDate() {
		DateOnly date = new DateOnly(0L);
		String result = new DD_MM_YYYY().getAsString(null, null, date);
		assertNotNull(result);
		assertNotNull(result);
	}

	@Test
	void ddMmYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsObjectReturnsNullForEmpty() {
		assertNull(new DD_MM_YYYY().getAsObject(null, null, "  "));
	}

	@Test
	void ddMmYyyy_getAsObjectParsesValidDate() {
		assertNotNull(new DD_MM_YYYY().getAsObject(null, null, "01/01/2023"));
	}

	@Test
	void ddMmYyyy_getAsObjectThrowsForInvalidDate() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY ----

	@Test
	void ddMmmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyy_getAsStringFormatsDate() {
		assertNotNull(new DD_MMM_YYYY().getAsString(null, null, new DateOnly(0L)));
	}

	@Test
	void ddMmmYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyy_getAsObjectParsesValidDate() {
		assertNotNull(new DD_MMM_YYYY().getAsObject(null, null, "01-Jan-2023"));
	}

	@Test
	void ddMmmYyyy_getAsObjectThrowsForInvalidDate() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY ----

	@Test
	void mmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyy_getAsStringFormatsDate() {
		assertNotNull(new MM_DD_YYYY().getAsString(null, null, new DateOnly(0L)));
	}

	@Test
	void mmDdYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyy_getAsObjectParsesValidDate() {
		assertNotNull(new MM_DD_YYYY().getAsObject(null, null, "01/01/2023"));
	}

	@Test
	void mmDdYyyy_getAsObjectThrowsForInvalidDate() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY ----

	@Test
	void mmmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyy_getAsStringFormatsDate() {
		assertNotNull(new MMM_DD_YYYY().getAsString(null, null, new DateOnly(0L)));
	}

	@Test
	void mmmDdYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyy_getAsObjectParsesValidDate() {
		assertNotNull(new MMM_DD_YYYY().getAsObject(null, null, "Jan-01-2023"));
	}

	@Test
	void mmmDdYyyy_getAsObjectThrowsForInvalidDate() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD ----

	@Test
	void yyyyMmDd_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDd_getAsStringFormatsDate() {
		assertNotNull(new YYYY_MM_DD().getAsString(null, null, new DateOnly(0L)));
	}

	@Test
	void yyyyMmDd_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDd_getAsObjectParsesValidDate() {
		assertNotNull(new YYYY_MM_DD().getAsObject(null, null, "2023/01/01"));
	}

	@Test
	void yyyyMmDd_getAsObjectThrowsForInvalidDate() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD().getAsObject(null, null, "not-a-date"));
	}
}

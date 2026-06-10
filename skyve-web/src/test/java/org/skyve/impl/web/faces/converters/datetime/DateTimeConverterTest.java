package org.skyve.impl.web.faces.converters.datetime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;

import jakarta.faces.convert.ConverterException;
@SuppressWarnings({"static-method", "java:S5778"})
public class DateTimeConverterTest {

	// ---- DD_MM_YYYY ----

	@Test
	void ddMmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MM_YYYY().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsObjectParsesValidDateTime() {
		assertNotNull(new DD_MM_YYYY().getAsObject(null, null, "01/01/2023"));
	}

	@Test
	void ddMmYyyy_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MM_YYYY_HH_MI ----

	@Test
	void ddMmYyyyHhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY_HH_MI().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyyHhMi_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MM_YYYY_HH_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmYyyyHhMi_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY_HH_MI().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyyHhMi_getAsObjectParsesValid() {
		assertNotNull(new DD_MM_YYYY_HH_MI().getAsObject(null, null, "01/01/2023 12:00 PM"));
	}

	@Test
	void ddMmYyyyHhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY_HH_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MM_YYYY_HH24_MI ----

	@Test
	void ddMmYyyyHh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY_HH24_MI().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyyHh24Mi_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MM_YYYY_HH24_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmYyyyHh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY_HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyyHh24Mi_getAsObjectParsesValid() {
		assertNotNull(new DD_MM_YYYY_HH24_MI().getAsObject(null, null, "01/01/2023 14:30"));
	}

	@Test
	void ddMmYyyyHh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY_HH24_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY ----

	@Test
	void ddMmmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyy_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MMM_YYYY().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmmYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyy_getAsObjectParsesValid() {
		assertNotNull(new DD_MMM_YYYY().getAsObject(null, null, "01-Jan-2023"));
	}

	@Test
	void ddMmmYyyy_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY_HH_MI ----

	@Test
	void ddMmmYyyyHhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY_HH_MI().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyyHhMi_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MMM_YYYY_HH_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmmYyyyHhMi_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY_HH_MI().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyyHhMi_getAsObjectParsesValid() {
		assertNotNull(new DD_MMM_YYYY_HH_MI().getAsObject(null, null, "01-Jan-2023 12:00 PM"));
	}

	@Test
	void ddMmmYyyyHhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY_HH_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY_HH24_MI ----

	@Test
	void ddMmmYyyyHh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY_HH24_MI().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyyHh24Mi_getAsStringFormatsDateTime() {
		assertNotNull(new DD_MMM_YYYY_HH24_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void ddMmmYyyyHh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY_HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyyHh24Mi_getAsObjectParsesValid() {
		assertNotNull(new DD_MMM_YYYY_HH24_MI().getAsObject(null, null, "01-Jan-2023 14:30"));
	}

	@Test
	void ddMmmYyyyHh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY_HH24_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY ----

	@Test
	void mmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyy_getAsStringFormatsDateTime() {
		assertNotNull(new MM_DD_YYYY().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmDdYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyy_getAsObjectParsesValid() {
		assertNotNull(new MM_DD_YYYY().getAsObject(null, null, "01/01/2023"));
	}

	@Test
	void mmDdYyyy_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY_HH_MI ----

	@Test
	void mmDdYyyyHhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY_HH_MI().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyyHhMi_getAsStringFormatsDateTime() {
		assertNotNull(new MM_DD_YYYY_HH_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmDdYyyyHhMi_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY_HH_MI().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyyHhMi_getAsObjectParsesValid() {
		assertNotNull(new MM_DD_YYYY_HH_MI().getAsObject(null, null, "01/01/2023 12:00 PM"));
	}

	@Test
	void mmDdYyyyHhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY_HH_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY_HH24_MI ----

	@Test
	void mmDdYyyyHh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY_HH24_MI().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyyHh24Mi_getAsStringFormatsDateTime() {
		assertNotNull(new MM_DD_YYYY_HH24_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmDdYyyyHh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY_HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyyHh24Mi_getAsObjectParsesValid() {
		assertNotNull(new MM_DD_YYYY_HH24_MI().getAsObject(null, null, "01/01/2023 14:30"));
	}

	@Test
	void mmDdYyyyHh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY_HH24_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY ----

	@Test
	void mmmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyy_getAsStringFormatsDateTime() {
		assertNotNull(new MMM_DD_YYYY().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmmDdYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyy_getAsObjectParsesValid() {
		assertNotNull(new MMM_DD_YYYY().getAsObject(null, null, "Jan-01-2023"));
	}

	@Test
	void mmmDdYyyy_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY_HH_MI ----

	@Test
	void mmmDdYyyyHhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY_HH_MI().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyyHhMi_getAsStringFormatsDateTime() {
		assertNotNull(new MMM_DD_YYYY_HH_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmmDdYyyyHhMi_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY_HH_MI().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyyHhMi_getAsObjectParsesValid() {
		assertNotNull(new MMM_DD_YYYY_HH_MI().getAsObject(null, null, "Jan-01-2023 12:00 PM"));
	}

	@Test
	void mmmDdYyyyHhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY_HH_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY_HH24_MI ----

	@Test
	void mmmDdYyyyHh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY_HH24_MI().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyyHh24Mi_getAsStringFormatsDateTime() {
		assertNotNull(new MMM_DD_YYYY_HH24_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void mmmDdYyyyHh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY_HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyyHh24Mi_getAsObjectParsesValid() {
		assertNotNull(new MMM_DD_YYYY_HH24_MI().getAsObject(null, null, "Jan-01-2023 14:30"));
	}

	@Test
	void mmmDdYyyyHh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY_HH24_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD ----

	@Test
	void yyyyMmDd_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDd_getAsStringFormatsDateTime() {
		assertNotNull(new YYYY_MM_DD().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void yyyyMmDd_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDd_getAsObjectParsesValid() {
		assertNotNull(new YYYY_MM_DD().getAsObject(null, null, "2023/01/01"));
	}

	@Test
	void yyyyMmDd_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD_HH_MI ----

	@Test
	void yyyyMmDdHhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD_HH_MI().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDdHhMi_getAsStringFormatsDateTime() {
		assertNotNull(new YYYY_MM_DD_HH_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void yyyyMmDdHhMi_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD_HH_MI().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDdHhMi_getAsObjectParsesValid() {
		assertNotNull(new YYYY_MM_DD_HH_MI().getAsObject(null, null, "2023/01/01 12:00 PM"));
	}

	@Test
	void yyyyMmDdHhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD_HH_MI().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD_HH24_MI ----

	@Test
	void yyyyMmDdHh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD_HH24_MI().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDdHh24Mi_getAsStringFormatsDateTime() {
		assertNotNull(new YYYY_MM_DD_HH24_MI().getAsString(null, null, new DateTime(0L)));
	}

	@Test
	void yyyyMmDdHh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD_HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDdHh24Mi_getAsObjectParsesValid() {
		assertNotNull(new YYYY_MM_DD_HH24_MI().getAsObject(null, null, "2023/01/01 14:30"));
	}

	@Test
	void yyyyMmDdHh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD_HH24_MI().getAsObject(null, null, "not-a-date"));
	}
}

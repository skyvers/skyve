package org.skyve.impl.web.faces.converters.timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;

import jakarta.faces.convert.ConverterException;
@SuppressWarnings({"static-method", "java:S5778"})
public class TimestampConverterTest {

	// ---- DD_MM_YYYY ----

	@Test
	void ddMmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MM_YYYY().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void ddMmYyyy_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyy_getAsObjectParsesValid() {
		assertNotNull(new DD_MM_YYYY().getAsObject(null, null, "01/01/2023"));
	}

	@Test
	void ddMmYyyy_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MM_YYYY_HH_MI_SS ----

	@Test
	void ddMmYyyyHhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY_HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyyHhMiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MM_YYYY_HH_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void ddMmYyyyHhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY_HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyyHhMiSs_getAsObjectParsesValid() {
		assertNotNull(new DD_MM_YYYY_HH_MI_SS().getAsObject(null, null, "01/01/2023 12:00:00 PM"));
	}

	@Test
	void ddMmYyyyHhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY_HH_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MM_YYYY_HH24_MI_SS ----

	@Test
	void ddMmYyyyHh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MM_YYYY_HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void ddMmYyyyHh24MiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MM_YYYY_HH24_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void ddMmYyyyHh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MM_YYYY_HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void ddMmYyyyHh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new DD_MM_YYYY_HH24_MI_SS().getAsObject(null, null, "01/01/2023 14:30:00"));
	}

	@Test
	void ddMmYyyyHh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MM_YYYY_HH24_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY ----

	@Test
	void ddMmmYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyy_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MMM_YYYY().getAsString(null, null, new Timestamp(0L)));
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

	// ---- DD_MMM_YYYY_HH_MI_SS ----

	@Test
	void ddMmmYyyyHhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY_HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyyHhMiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MMM_YYYY_HH_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void ddMmmYyyyHhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY_HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyyHhMiSs_getAsObjectParsesValid() {
		assertNotNull(new DD_MMM_YYYY_HH_MI_SS().getAsObject(null, null, "01-Jan-2023 12:00:00 PM"));
	}

	@Test
	void ddMmmYyyyHhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY_HH_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- DD_MMM_YYYY_HH24_MI_SS ----

	@Test
	void ddMmmYyyyHh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new DD_MMM_YYYY_HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void ddMmmYyyyHh24MiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new DD_MMM_YYYY_HH24_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void ddMmmYyyyHh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new DD_MMM_YYYY_HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void ddMmmYyyyHh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new DD_MMM_YYYY_HH24_MI_SS().getAsObject(null, null, "01-Jan-2023 14:30:00"));
	}

	@Test
	void ddMmmYyyyHh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new DD_MMM_YYYY_HH24_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY ----

	@Test
	void mmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyy_getAsStringFormatsTimestamp() {
		assertNotNull(new MM_DD_YYYY().getAsString(null, null, new Timestamp(0L)));
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

	// ---- MM_DD_YYYY_HH_MI_SS ----

	@Test
	void mmDdYyyyHhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY_HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyyHhMiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new MM_DD_YYYY_HH_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void mmDdYyyyHhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY_HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyyHhMiSs_getAsObjectParsesValid() {
		assertNotNull(new MM_DD_YYYY_HH_MI_SS().getAsObject(null, null, "01/01/2023 12:00:00 PM"));
	}

	@Test
	void mmDdYyyyHhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY_HH_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- MM_DD_YYYY_HH24_MI_SS ----

	@Test
	void mmDdYyyyHh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MM_DD_YYYY_HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void mmDdYyyyHh24MiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new MM_DD_YYYY_HH24_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void mmDdYyyyHh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new MM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void mmDdYyyyHh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new MM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, "01/01/2023 14:30:00"));
	}

	@Test
	void mmDdYyyyHh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY ----

	@Test
	void mmmDdYyyy_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyy_getAsStringFormatsTimestamp() {
		assertNotNull(new MMM_DD_YYYY().getAsString(null, null, new Timestamp(0L)));
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

	// ---- MMM_DD_YYYY_HH_MI_SS ----

	@Test
	void mmmDdYyyyHhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY_HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyyHhMiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new MMM_DD_YYYY_HH_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void mmmDdYyyyHhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY_HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyyHhMiSs_getAsObjectParsesValid() {
		assertNotNull(new MMM_DD_YYYY_HH_MI_SS().getAsObject(null, null, "Jan-01-2023 12:00:00 PM"));
	}

	@Test
	void mmmDdYyyyHhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY_HH_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- MMM_DD_YYYY_HH24_MI_SS ----

	@Test
	void mmmDdYyyyHh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new MMM_DD_YYYY_HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void mmmDdYyyyHh24MiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new MMM_DD_YYYY_HH24_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void mmmDdYyyyHh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new MMM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void mmmDdYyyyHh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new MMM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, "Jan-01-2023 14:30:00"));
	}

	@Test
	void mmmDdYyyyHh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new MMM_DD_YYYY_HH24_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD ----

	@Test
	void yyyyMmDd_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDd_getAsStringFormatsTimestamp() {
		assertNotNull(new YYYY_MM_DD().getAsString(null, null, new Timestamp(0L)));
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

	// ---- YYYY_MM_DD_HH_MI_SS ----

	@Test
	void yyyyMmDdHhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD_HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDdHhMiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new YYYY_MM_DD_HH_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void yyyyMmDdHhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD_HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDdHhMiSs_getAsObjectParsesValid() {
		assertNotNull(new YYYY_MM_DD_HH_MI_SS().getAsObject(null, null, "2023/01/01 12:00:00 PM"));
	}

	@Test
	void yyyyMmDdHhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD_HH_MI_SS().getAsObject(null, null, "not-a-date"));
	}

	// ---- YYYY_MM_DD_HH24_MI_SS ----

	@Test
	void yyyyMmDdHh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new YYYY_MM_DD_HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void yyyyMmDdHh24MiSs_getAsStringFormatsTimestamp() {
		assertNotNull(new YYYY_MM_DD_HH24_MI_SS().getAsString(null, null, new Timestamp(0L)));
	}

	@Test
	void yyyyMmDdHh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new YYYY_MM_DD_HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void yyyyMmDdHh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new YYYY_MM_DD_HH24_MI_SS().getAsObject(null, null, "2023/01/01 14:30:00"));
	}

	@Test
	void yyyyMmDdHh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new YYYY_MM_DD_HH24_MI_SS().getAsObject(null, null, "not-a-date"));
	}
}

package org.skyve.impl.web.faces.converters.time;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.TimeOnly;

import jakarta.faces.convert.ConverterException;
@SuppressWarnings({"static-method", "java:S5778"})
public class TimeConverterTest {

	// ---- HH_MI ----

	@Test
	void hhMi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new HH_MI().getAsString(null, null, null));
	}

	@Test
	void hhMi_getAsStringFormatsTime() {
		assertNotNull(new HH_MI().getAsString(null, null, new TimeOnly(12, 30, 0)));
	}

	@Test
	void hhMi_getAsObjectReturnsNullForNull() {
		assertNull(new HH_MI().getAsObject(null, null, null));
	}

	@Test
	void hhMi_getAsObjectParsesValid() {
		assertNotNull(new HH_MI().getAsObject(null, null, "12:30 PM"));
	}

	@Test
	void hhMi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new HH_MI().getAsObject(null, null, "not-a-time"));
	}

	// ---- HH_MI_SS ----

	@Test
	void hhMiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new HH_MI_SS().getAsString(null, null, null));
	}

	@Test
	void hhMiSs_getAsStringFormatsTime() {
		assertNotNull(new HH_MI_SS().getAsString(null, null, new TimeOnly(12, 30, 0)));
	}

	@Test
	void hhMiSs_getAsObjectReturnsNullForNull() {
		assertNull(new HH_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void hhMiSs_getAsObjectParsesValid() {
		assertNotNull(new HH_MI_SS().getAsObject(null, null, "12:30:00 PM"));
	}

	@Test
	void hhMiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new HH_MI_SS().getAsObject(null, null, "not-a-time"));
	}

	// ---- HH24_MI ----

	@Test
	void hh24Mi_getAsStringReturnsEmptyForNull() {
		assertEquals("", new HH24_MI().getAsString(null, null, null));
	}

	@Test
	void hh24Mi_getAsStringFormatsTime() {
		assertNotNull(new HH24_MI().getAsString(null, null, new TimeOnly(14, 30, 0)));
	}

	@Test
	void hh24Mi_getAsObjectReturnsNullForNull() {
		assertNull(new HH24_MI().getAsObject(null, null, null));
	}

	@Test
	void hh24Mi_getAsObjectParsesValid() {
		assertNotNull(new HH24_MI().getAsObject(null, null, "14:30"));
	}

	@Test
	void hh24Mi_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new HH24_MI().getAsObject(null, null, "not-a-time"));
	}

	// ---- HH24_MI_SS ----

	@Test
	void hh24MiSs_getAsStringReturnsEmptyForNull() {
		assertEquals("", new HH24_MI_SS().getAsString(null, null, null));
	}

	@Test
	void hh24MiSs_getAsStringFormatsTime() {
		assertNotNull(new HH24_MI_SS().getAsString(null, null, new TimeOnly(14, 30, 0)));
	}

	@Test
	void hh24MiSs_getAsObjectReturnsNullForNull() {
		assertNull(new HH24_MI_SS().getAsObject(null, null, null));
	}

	@Test
	void hh24MiSs_getAsObjectParsesValid() {
		assertNotNull(new HH24_MI_SS().getAsObject(null, null, "14:30:00"));
	}

	@Test
	void hh24MiSs_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new HH24_MI_SS().getAsObject(null, null, "not-a-time"));
	}
}

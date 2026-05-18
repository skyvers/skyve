package org.skyve.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.concurrent.CopyOnWriteArraySet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

class IPGeolocationTest {

	@AfterEach
	@SuppressWarnings("static-method")
	void resetGeoIpConfig() {
		UtilImpl.GEO_IP_COUNTRY_CODES = null;
		UtilImpl.GEO_IP_WHITELIST = true;
	}

	@Test
	@SuppressWarnings("static-method")
	void emptyConstantHasNullFields() {
		assertNull(IPGeolocation.EMPTY.city());
		assertNull(IPGeolocation.EMPTY.region());
		assertNull(IPGeolocation.EMPTY.countryCode());
		assertNull(IPGeolocation.EMPTY.location());
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsFields() {
		IPGeolocation geo = new IPGeolocation("Melbourne", "Victoria", "AU", null);
		assertThat(geo.city(), is("Melbourne"));
		assertThat(geo.region(), is("Victoria"));
		assertThat(geo.countryCode(), is("AU"));
		assertNull(geo.location());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsFalseWhenCountryCodeNull() {
		// EMPTY has null countryCode so can never be blocked
		assertFalse(IPGeolocation.EMPTY.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsFalseWhenCountryCodesListNotConfigured() {
		// UtilImpl.GEO_IP_COUNTRY_CODES is null by default → not blocked
		IPGeolocation geo = new IPGeolocation("Paris", "Ile-de-France", "FR", null);
		assertFalse(geo.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void emptyInstanceIsSingleton() {
		// Both null-all instances should be the same EMPTY singleton after deserialization
		assertNotNull(IPGeolocation.EMPTY);
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsTrueWhenCountryInBlacklist() {
		CopyOnWriteArraySet<String> codes = new CopyOnWriteArraySet<>();
		codes.add("CN");
		UtilImpl.GEO_IP_COUNTRY_CODES = codes;
		UtilImpl.GEO_IP_WHITELIST = false;
		IPGeolocation geo = new IPGeolocation("Beijing", "Beijing", "CN", null);
		assertTrue(geo.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsFalseWhenCountryNotInBlacklist() {
		CopyOnWriteArraySet<String> codes = new CopyOnWriteArraySet<>();
		codes.add("CN");
		UtilImpl.GEO_IP_COUNTRY_CODES = codes;
		UtilImpl.GEO_IP_WHITELIST = false;
		IPGeolocation geo = new IPGeolocation("Melbourne", "Victoria", "AU", null);
		assertFalse(geo.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsFalseWhenCountryInWhitelist() {
		CopyOnWriteArraySet<String> codes = new CopyOnWriteArraySet<>();
		codes.add("AU");
		UtilImpl.GEO_IP_COUNTRY_CODES = codes;
		UtilImpl.GEO_IP_WHITELIST = true;
		IPGeolocation geo = new IPGeolocation("Melbourne", "Victoria", "AU", null);
		assertFalse(geo.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBlockedReturnsTrueWhenCountryNotInWhitelist() {
		CopyOnWriteArraySet<String> codes = new CopyOnWriteArraySet<>();
		codes.add("AU");
		UtilImpl.GEO_IP_COUNTRY_CODES = codes;
		UtilImpl.GEO_IP_WHITELIST = true;
		IPGeolocation geo = new IPGeolocation("Beijing", "Beijing", "CN", null);
		assertTrue(geo.isBlocked());
	}

	@Test
	@SuppressWarnings("static-method")
	void getCountryReturnsNullWhenCountryCodeIsNull() {
		IPGeolocation geo = new IPGeolocation("City", "Region", null, null);
		assertNull(geo.getCountry());
	}

	@Test
	@SuppressWarnings("static-method")
	void readResolveForAllNullFieldsReturnsSingleton() throws Exception {
		// Serializing an all-null IPGeolocation should deserialise as the EMPTY singleton
		IPGeolocation original = new IPGeolocation(null, null, null, null);
		byte[] bytes;
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ObjectOutputStream oos = new ObjectOutputStream(baos)) {
			oos.writeObject(original);
			bytes = baos.toByteArray();
		}
		Object deserialized;
		try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
			deserialized = ois.readObject();
		}
		assertSame(IPGeolocation.EMPTY, deserialized, "All-null IPGeolocation should resolve to EMPTY singleton");
	}

	@Test
	@SuppressWarnings("static-method")
	void readResolveForNonEmptyFieldsReturnsNewInstance() throws Exception {
		// Non-empty IPGeolocation should not resolve to EMPTY
		IPGeolocation original = new IPGeolocation("Melbourne", "Victoria", "AU", null);
		byte[] bytes;
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ObjectOutputStream oos = new ObjectOutputStream(baos)) {
			oos.writeObject(original);
			bytes = baos.toByteArray();
		}
		Object deserialized;
		try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
			deserialized = ois.readObject();
		}
		assertTrue(deserialized instanceof IPGeolocation);
		assertFalse(deserialized == IPGeolocation.EMPTY, "Non-null IPGeolocation should not resolve to EMPTY");
	}
}


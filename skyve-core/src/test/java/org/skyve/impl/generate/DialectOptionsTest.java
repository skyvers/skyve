package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class DialectOptionsTest {

	@Test
	@SuppressWarnings("static-method")
	void getDataStoreBizKeyLengthReturnsExpectedValue() {
		assertEquals(1024, DialectOptions.H2.getDataStoreBizKeyLength());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDescriptionReturnsNonNull() {
		assertNotNull(DialectOptions.H2.getDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDataStoreIdentifierCharacterLimitReturnsZeroForH2() {
		assertEquals(0, DialectOptions.H2.getDataStoreIdentifierCharacterLimit());
	}
}

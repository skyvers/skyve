package org.skyve.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MetaDataExceptionTest {

	@Test
	void constructWithCause() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException(cause);
		assertNotNull(ex);
		assertEquals(cause, ex.getCause());
	}

	@Test
	void constructWithMessage() {
		MetaDataException ex = new MetaDataException("bad metadata");
		assertNotNull(ex.getMessage());
	}

	@Test
	void constructWithMessageAndCause() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("bad metadata", cause);
		assertEquals(cause, ex.getCause());
	}

	@Test
	void constructWithEnableSuppression() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("msg", cause, true, true);
		assertEquals(cause, ex.getCause());
	}

	@Test
	void constructWithI18nValues() {
		MetaDataException ex = new MetaDataException("msg {0}", "arg");
		assertNotNull(ex);
	}

	@Test
	void constructWithCauseAndI18nValues() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("msg {0}", cause, "arg");
		assertEquals(cause, ex.getCause());
	}

	@Test
	void constructWithEnableSuppressionAndI18nValues() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("msg {0}", cause, true, true, "arg");
		assertEquals(cause, ex.getCause());
	}

	@Test
	void constructWithI18nBooleanTrue() {
		MetaDataException ex = new MetaDataException("msg", true);
		assertNotNull(ex);
	}

	@Test
	void constructWithI18nBooleanFalse() {
		MetaDataException ex = new MetaDataException("raw msg", false);
		assertEquals("raw msg", ex.getMessage());
	}

	@Test
	void constructWithEnableSuppressionAndI18nBoolean() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("raw msg", cause, false, false, false);
		assertEquals("raw msg", ex.getMessage());
	}

	@Test
	void constructWithCauseAndI18nBoolean() {
		RuntimeException cause = new RuntimeException("root");
		MetaDataException ex = new MetaDataException("raw msg", cause, false);
		assertEquals(cause, ex.getCause());
	}
}

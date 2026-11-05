package org.skyve.archive.support;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.archive.support.CorruptArchiveError.Resolution;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

@SuppressWarnings("static-method")
class CorruptArchiveErrorResolutionTest {

	@Test
	void fromCodeReturnsMatchingValue() {
		assertEquals(Resolution.resolved, Resolution.fromCode("resolved"));
		assertEquals(Resolution.unresolved, Resolution.fromCode("unresolved"));
		assertEquals(Resolution.inProgress, Resolution.fromCode("in_progress"));
		assertEquals(Resolution.failed, Resolution.fromCode("failed"));
	}

	@Test
	void fromCodeReturnsNullForUnknownCode() {
		assertNull(Resolution.fromCode("unknown"));
		assertNull(Resolution.fromCode(""));
	}

	@Test
	void fromLocalisedDescriptionReturnsMatchingValue() {
		// toLocalisedDescription() falls back to the key when no i18n bundle is available
		assertEquals(Resolution.resolved, Resolution.fromLocalisedDescription(Resolution.resolved.toLocalisedDescription()));
		assertEquals(Resolution.unresolved, Resolution.fromLocalisedDescription(Resolution.unresolved.toLocalisedDescription()));
	}

	@Test
	void fromLocalisedDescriptionReturnsNullForUnknownDescription() {
		assertNull(Resolution.fromLocalisedDescription("nonexistent_description_value"));
	}

	@Test
	void toDomainValuesReturnsAllFourValues() {
		List<DomainValue> values = Resolution.toDomainValues();
		assertNotNull(values);
		assertEquals(4, values.size());
	}

	@Test
	void toCodeReturnsExpectedCode() {
		assertEquals("resolved", Resolution.resolved.toCode());
		assertEquals("unresolved", Resolution.unresolved.toCode());
		assertEquals("in_progress", Resolution.inProgress.toCode());
		assertEquals("failed", Resolution.failed.toCode());
	}

	@Test
	void toDomainValueReturnsNotNull() {
		assertNotNull(Resolution.resolved.toDomainValue());
		assertEquals("resolved", Resolution.resolved.toDomainValue().getCode());
	}

	@Test
	void toDomainValuesIsUnmodifiable() {
		List<DomainValue> values = Resolution.toDomainValues();
		assertFalse(values.isEmpty());
		try {
			values.add(Resolution.resolved.toDomainValue());
			assertFalse(true, "Expected UnsupportedOperationException");
		} catch (@SuppressWarnings("unused") UnsupportedOperationException e) {
			// expected — list is unmodifiable
		}
	}
}

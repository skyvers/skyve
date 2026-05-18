package org.skyve.archive.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.List;

import org.junit.Test;
import org.skyve.archive.support.CorruptArchiveError.Resolution;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

@SuppressWarnings("static-method")
public class CorruptArchiveErrorResolutionTest {

	@Test
	public void fromCodeReturnsMatchingValue() {
		assertEquals(Resolution.resolved, Resolution.fromCode("resolved"));
		assertEquals(Resolution.unresolved, Resolution.fromCode("unresolved"));
		assertEquals(Resolution.inProgress, Resolution.fromCode("in_progress"));
		assertEquals(Resolution.failed, Resolution.fromCode("failed"));
	}

	@Test
	public void fromCodeReturnsNullForUnknownCode() {
		assertNull(Resolution.fromCode("unknown"));
		assertNull(Resolution.fromCode(""));
	}

	@Test
	public void fromLocalisedDescriptionReturnsMatchingValue() {
		// toLocalisedDescription() falls back to the key when no i18n bundle is available
		assertEquals(Resolution.resolved, Resolution.fromLocalisedDescription(Resolution.resolved.toLocalisedDescription()));
		assertEquals(Resolution.unresolved, Resolution.fromLocalisedDescription(Resolution.unresolved.toLocalisedDescription()));
	}

	@Test
	public void fromLocalisedDescriptionReturnsNullForUnknownDescription() {
		assertNull(Resolution.fromLocalisedDescription("nonexistent_description_value"));
	}

	@Test
	public void toDomainValuesReturnsAllFourValues() {
		List<DomainValue> values = Resolution.toDomainValues();
		assertNotNull(values);
		assertEquals(4, values.size());
	}

	@Test
	public void toCodeReturnsExpectedCode() {
		assertEquals("resolved", Resolution.resolved.toCode());
		assertEquals("unresolved", Resolution.unresolved.toCode());
		assertEquals("in_progress", Resolution.inProgress.toCode());
		assertEquals("failed", Resolution.failed.toCode());
	}

	@Test
	public void toDomainValueReturnsNotNull() {
		assertNotNull(Resolution.resolved.toDomainValue());
		assertEquals("resolved", Resolution.resolved.toDomainValue().getCode());
	}

	@Test
	public void toDomainValuesIsUnmodifiable() {
		List<DomainValue> values = Resolution.toDomainValues();
		assertFalse(values.isEmpty());
		try {
			values.add(Resolution.resolved.toDomainValue());
			assertFalse("Expected UnsupportedOperationException", true);
		} catch (@SuppressWarnings("unused") UnsupportedOperationException e) {
			// expected — list is unmodifiable
		}
	}
}

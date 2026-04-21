package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;

public class MfaConfigurationUtilTest {
	@Test
	public void testParseValidJsonArray() {
		Optional<List<MfaOption>> parsed = MfaConfigurationUtil.parse("[{\"method\":\"EMAIL\",\"enabled\":true}]");

		assertTrue(parsed.isPresent());
		assertEquals(List.of(new MfaOption("EMAIL", true)), parsed.get());
	}

	@Test
	public void testParseNullEmptyAndEmptyArrayFallbackToCustomerDefaults() {
		assertTrue(MfaConfigurationUtil.parse(null).isEmpty());
		assertTrue(MfaConfigurationUtil.parse("").isEmpty());
		assertTrue(MfaConfigurationUtil.parse("[]").isEmpty());
	}

	@Test
	public void testParseMalformedJsonFailsClosed() {
		assertTrue(MfaConfigurationUtil.parse("{not-json").isEmpty());
	}

	@Test
	public void testIsMethodEnabledTrueWhenConfigured() {
		Optional<Boolean> enabled = MfaConfigurationUtil.isMethodEnabled("[{\"method\":\"EMAIL\",\"enabled\":true}]", "EMAIL");

		assertTrue(enabled.isPresent());
		assertTrue(enabled.get().booleanValue());
	}

	@Test
	public void testIsMethodEnabledFalseWhenMethodDisabled() {
		Optional<Boolean> enabled = MfaConfigurationUtil.isMethodEnabled("[{\"method\":\"EMAIL\",\"enabled\":false}]", "EMAIL");

		assertTrue(enabled.isPresent());
		assertFalse(enabled.get().booleanValue());
	}

	@Test
	public void testIsMethodEnabledReturnsFalseWhenMethodMissingFromValidJson() {
		Optional<Boolean> enabled = MfaConfigurationUtil.isMethodEnabled("[{\"method\":\"SMS\",\"enabled\":true}]", "EMAIL");

		assertTrue(enabled.isPresent());
		assertFalse(enabled.get().booleanValue());
	}

	@Test
	public void testIsMethodEnabledReturnsEmptyForNullAndMalformedJson() {
		assertTrue(MfaConfigurationUtil.isMethodEnabled(null, "EMAIL").isEmpty());
		assertTrue(MfaConfigurationUtil.isMethodEnabled("{broken", "EMAIL").isEmpty());
	}

	@Test
	public void testRoundTripSerialiseParse() {
		List<MfaOption> options = List.of(new MfaOption("EMAIL", true), new MfaOption("TOTP", false));

		String json = MfaConfigurationUtil.toJson(options);
		Optional<List<MfaOption>> parsed = MfaConfigurationUtil.parse(json);

		assertTrue(parsed.isPresent());
		assertEquals(options, parsed.get());
	}
}

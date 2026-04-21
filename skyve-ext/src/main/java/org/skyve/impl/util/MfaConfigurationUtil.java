package org.skyve.impl.util;

import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Utility methods for serialising and parsing per-user MFA configuration JSON.
 * <p>
 * The stored JSON is expected to be an array of {@link MfaOption} values.
 * Parsing is intentionally fail-closed: malformed JSON never disables MFA and
 * instead falls back to customer-level MFA configuration.
 */
public class MfaConfigurationUtil {
	private static final Logger LOGGER = LoggerFactory.getLogger(MfaConfigurationUtil.class);
	private static final ObjectMapper MAPPER = new ObjectMapper()
			.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	private static final TypeReference<List<MfaOption>> MFA_OPTION_LIST_TYPE = new TypeReference<>() {
		// type token
	};

	private MfaConfigurationUtil() {
		// static utility
	}

	/**
	 * Parse result distinguishes three states:
	 * - null input / blank / empty array -> returns empty Optional (inherit customer default)
	 * - valid JSON with entries -> returns Optional of the parsed list
	 * - malformed JSON -> logs a warning and returns empty Optional (fail closed)
	 *
	 * @param json the persisted per-user MFA JSON array
	 * @return an {@link Optional} containing parsed options for a valid non-empty array;
	 *         otherwise {@link Optional#empty()} to indicate fallback to customer defaults
	 */
	public static Optional<List<MfaOption>> parse(String json) {
		String jsonToParse = UtilImpl.processStringValue(json);
		if (jsonToParse == null) {
			return Optional.empty();
		}

		try {
			List<MfaOption> options = MAPPER.readValue(jsonToParse, MFA_OPTION_LIST_TYPE);
			if ((options == null) || options.isEmpty()) {
				return Optional.empty();
			}
			return Optional.of(options);
		} catch (JsonProcessingException e) {
			LOGGER.warn("Invalid per-user MFA configuration JSON. Falling back to customer defaults.", e);
			return Optional.empty();
		}
	}

	/**
	 * Serialise MFA options to a JSON array string for persistence.
	 *
	 * @param options the MFA options to serialise; {@code null} is treated as an empty list
	 * @return the JSON array representation
	 * @throws IllegalArgumentException if serialisation fails
	 */
	public static String toJson(List<MfaOption> options) {
		try {
			return MAPPER.writeValueAsString((options == null) ? List.of() : options);
		} catch (JsonProcessingException e) {
			throw new IllegalArgumentException("Unable to serialise MFA configuration to JSON.", e);
		}
	}

	/**
	 * Three-valued result:
	 * - null/blank/empty/malformed JSON -> empty Optional (caller should fall back to customer config)
	 * - valid JSON with method present -> Optional.of(true/false)
	 * - valid JSON without the method -> Optional.of(false)
	 *
	 * @param json the persisted per-user MFA JSON array
	 * @param method the MFA method name to evaluate, for example {@code EMAIL}
	 * @return {@link Optional#empty()} when caller should fall back to customer configuration;
	 *         otherwise {@link Optional#of(Object)} with whether the method is enabled
	 */
	public static Optional<Boolean> isMethodEnabled(String json, String method) {
		Optional<List<MfaOption>> parsedOptions = parse(json);
		if (parsedOptions.isEmpty()) {
			return Optional.empty();
		}

		String requestedMethod = UtilImpl.processStringValue(method);
		if (requestedMethod == null) {
			return Optional.of(Boolean.FALSE);
		}

		for (MfaOption option : parsedOptions.get()) {
			if (option == null) {
				continue;
			}

			String configuredMethod = UtilImpl.processStringValue(option.getMethod());
			if ((configuredMethod != null) && configuredMethod.equalsIgnoreCase(requestedMethod)) {
				return Optional.of(Boolean.valueOf(option.isEnabled()));
			}
		}
		return Optional.of(Boolean.FALSE);
	}
}

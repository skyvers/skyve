package org.skyve.impl.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.skyve.util.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods for serialising and parsing per-user MFA configuration JSON.
 * <p>
 * The stored JSON is expected to be an array of {@link MfaOption} values.
 * Parsing is intentionally fail-closed: malformed JSON never disables MFA and
 * instead falls back to customer-level MFA configuration.
 */
public class MfaConfigurationUtil {
	private static final Logger LOGGER = LoggerFactory.getLogger(MfaConfigurationUtil.class);

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
			Object parsed = JSON.unmarshall(jsonToParse);
			if (! (parsed instanceof List<?>)) {
				LOGGER.warn("Invalid per-user MFA configuration JSON. Expected an array. Falling back to customer defaults.");
				return Optional.empty();
			}

			List<?> rawOptions = (List<?>) parsed;
			if (rawOptions.isEmpty()) {
				return Optional.empty();
			}

			List<MfaOption> options = new ArrayList<>(rawOptions.size());
			for (Object rawOption : rawOptions) {
				if (! (rawOption instanceof Map<?, ?>)) {
					LOGGER.warn("Invalid per-user MFA configuration JSON entry. Falling back to customer defaults.");
					return Optional.empty();
				}

				Map<?, ?> optionMap = (Map<?, ?>) rawOption;
				String method = null;
				Object methodValue = optionMap.get("method");
				if (methodValue != null) {
					method = methodValue.toString();
				}

				boolean enabled = false;
				Object enabledValue = optionMap.get("enabled");
				if (enabledValue instanceof Boolean) {
					enabled = ((Boolean) enabledValue).booleanValue();
				}
				else if (enabledValue instanceof String) {
					enabled = Boolean.parseBoolean((String) enabledValue);
				}

				options.add(new MfaOption(method, enabled));
			}
			return Optional.of(options);
		}
		catch (Exception e) {
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
			List<?> optionsToWrite = (options == null) ? List.of() : options;
			List<Object> jsonOptions = new ArrayList<>(optionsToWrite.size());
			for (Object optionObject : optionsToWrite) {
				if (optionObject == null) {
					jsonOptions.add(null);
					continue;
				}

				MfaOption option = (MfaOption) optionObject;
				Map<String, Object> optionMap = new LinkedHashMap<>(2);
				optionMap.put("method", option.getMethod());
				optionMap.put("enabled", Boolean.valueOf(option.isEnabled()));
				jsonOptions.add(optionMap);
			}
			return JSON.marshall(jsonOptions);
		}
		catch (Exception e) {
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

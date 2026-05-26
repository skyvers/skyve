package org.skyve.nlp.cron.elementprovider.hour;

/**
 * Detects {@code noon} and {@code midday} phrases and contributes 12:00 hour semantics.
 */
public class Noon extends Midnight {

	/**
	 * Matches noon-like keywords.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when noon or midday is referenced.
	 */
	@Override
	public boolean matches(String value) {
		match = value != null &&
				(value.toLowerCase().indexOf("noon") >= 0 || value.toLowerCase().indexOf("midday") >= 0);
		return match;
	}

	/**
	 * Returns the fixed noon hour token.
	 *
	 * @return {@code "12"}.
	 */
	@Override
	public String getHourElement() {
		return "12";
	}
}

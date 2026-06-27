package org.skyve.impl.web.faces.pipeline.component;

/**
 * Carries metadata-owned text with its resolved HTML escape decision through the
 * PrimeFaces component-building layer.
 *
 * @param value metadata text after localisation and expression resolution; may be {@code null}
 * @param escape {@code true} to escape at the output boundary; {@code false}
 *        to allow trusted markup
 */
public record EscapableText(String value, boolean escape) {
	/**
	 * Creates a text value paired with its metadata escape flag.
	 *
	 * @param value metadata text after localisation and expression resolution; may be {@code null}
	 * @param escape {@code true} to escape at the output boundary; {@code false}
	 *        to allow trusted markup
	 * @return an immutable text/escape pair; never {@code null}
	 */
	public static EscapableText of(String value, boolean escape) {
		return new EscapableText(value, escape);
	}

	/**
	 * Returns the raw metadata text.
	 *
	 * @return metadata text, or {@code null} when no text is configured
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Returns the resolved metadata escape decision.
	 *
	 * @return {@code true} for escaped output; {@code false} for trusted markup
	 */
	public boolean getEscape() {
		return escape;
	}

	/**
	 * Returns whether the receiving component should escape the value.
	 *
	 * @return {@code true} for escaped output; {@code false} for trusted markup
	 */
	public boolean shouldEscape() {
		return escape;
	}
}

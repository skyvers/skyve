package org.skyve.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Mixin interface for view elements that output text and support HTML escaping
 * and sanitisation of the rendered value.
 *
 * <p>The two concerns are orthogonal:
 * <ul>
 *   <li>{@link #getEscape()} governs whether special characters are HTML-encoded
 *       before rendering (XSS prevention for plain-text fields).</li>
 *   <li>{@link #getSanitise()} applies an HTML sanitisation policy to rich-text
 *       content, removing or neutralising unsafe elements and attributes.</li>
 * </ul>
 */
public interface TextOutput {
	/**
	 * Returns whether HTML special characters in the rendered value should be escaped.
	 *
	 * @return {@code Boolean.TRUE} to escape, {@code Boolean.FALSE} to render raw HTML,
	 *         or {@code null} to use the widget-type default
	 */
	Boolean getEscape();

	/**
	 * Defines the HTML sanitisation policy applied to a rich-text value before rendering.
	 *
	 * <p>Policies are ordered from least restrictive ({@link #none}) to most restrictive
	 * ({@link #text}). When comparing policies the ordinal may be used to determine
	 * relative restrictiveness.
	 */
	@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are metadata XML values.
	public enum Sanitisation {
		// Note the values are ordered from least restrictive to most restrictive so we can compare the ordinals
		
		/** No sanitisation applied; the raw value is rendered as-is. */
		none,
		/** Permits formatting tags, structural tags, links, images, and CSS styles. */
		relaxed,
		/** Permits formatting tags, structural tags, links, and images (no CSS). */
		simple,
		/** Permits inline formatting tags only (bold, italic, etc.). */
		basic,
		/** Strips all HTML; only plain text is rendered. */
		text
	}
	
	Sanitisation getSanitise();
}

package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;

/**
 * Customer-specific HTML resource overrides.
 *
 * <p>Declared in the customer XML, this interface exposes a relative path to a
 * customer-specific CSS file that Skyve injects into generated pages, allowing
 * per-tenant visual customisation on top of the base theme.
 *
 * @see Customer#getHtmlResources()
 */
public interface HTMLResources extends SerializableMetaData {
	/**
	 * Returns the web-root-relative path to the customer CSS file.
	 *
	 * @return the CSS file path; may be {@code null} if not configured
	 */
	public String getCssRelativeFileName();
}

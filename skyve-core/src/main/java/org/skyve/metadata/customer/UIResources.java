package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;

/**
 * Customer-specific UI resource overrides.
 *
 * <p>Declared in the customer XML, this interface exposes a relative path to a
 * customer logo image that Skyve renders in the application header and other
 * branding locations.
 *
 * @see Customer#getUiResources()
 */
public interface UIResources extends SerializableMetaData {
	/**
	 * Returns the web-root-relative path to the customer logo image file.
	 *
	 * @return the logo file path; may be {@code null} if not configured
	 */
	public String getLogoRelativeFileName();
}

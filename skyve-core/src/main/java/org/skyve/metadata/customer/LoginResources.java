package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;

/**
 * Customer-specific login and logout page URL overrides.
 *
 * <p>When configured, Skyve uses these URLs instead of the built-in login page,
 * allowing customers to provide branded or SSO-integrated authentication screens.
 *
 * @see Customer#getLoginResources()
 */
public interface LoginResources extends SerializableMetaData {
	/**
	 * Returns the URL of the customer-specific login page.
	 *
	 * @return the login page URL; may be {@code null} if using the default Skyve login page
	 */
	public String getLoginPageURL();

	/**
	 * Returns the URL to redirect to after a successful logout.
	 *
	 * @return the logged-out redirect URL; may be {@code null} if using the Skyve default
	 */
	public String getLoggedOutPageURL();
}

package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;
import org.skyve.metadata.customer.LoginResources;

/**
 * Builds customer login resource metadata.
 */
public class FluentLoginResources {
	private LoginResourcesMetaData resources = null;
	
	/**
	 * Creates a builder with new empty login resources metadata.
	 */
	public FluentLoginResources() {
		resources = new LoginResourcesMetaData();
	}

	/**
	 * Creates a builder around existing login resources metadata.
	 *
	 * @param resources backing metadata
	 */
	public FluentLoginResources(LoginResourcesMetaData resources) {
		this.resources = resources;
	}
	
	/**
	 * Copies login resource URLs from the given metadata contract.
	 *
	 * @param resources source login resources
	 * @return this builder
	 */
	public FluentLoginResources from(@SuppressWarnings("hiding") LoginResources resources) {
		loginPageURL(resources.getLoginPageURL());
		loggedOutPageURL(resources.getLoggedOutPageURL());
		return this;
	}
	
	/**
	 * Sets the login page URL.
	 *
	 * @param url login page URL
	 * @return this builder
	 */
	public FluentLoginResources loginPageURL(String url) {
		resources.setLoginPageURL(url);
		return this;
	}

	/**
	 * Sets the logged-out landing page URL.
	 *
	 * @param url logged-out page URL
	 * @return this builder
	 */
	public FluentLoginResources loggedOutPageURL(String url) {
		resources.setLoggedOutPageURL(url);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing login resources metadata
	 */
	public LoginResourcesMetaData get() {
		return resources;
	}
}

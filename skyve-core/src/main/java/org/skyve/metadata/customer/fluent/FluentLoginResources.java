package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;
import org.skyve.metadata.customer.LoginResources;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentLoginResources(@Nonnull LoginResourcesMetaData resources) {
		this.resources = resources;
	}
	
	/**
	 * Copies login resource URLs from the given metadata contract.
	 *
	 * @param resources source login resources
	 * @return this builder
	 */
	public @Nonnull FluentLoginResources from(@SuppressWarnings("hiding") @Nonnull LoginResources resources) {
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
	public @Nonnull FluentLoginResources loginPageURL(@Nullable String url) {
		resources.setLoginPageURL(url);
		return this;
	}

	/**
	 * Sets the logged-out landing page URL.
	 *
	 * @param url logged-out page URL
	 * @return this builder
	 */
	public @Nonnull FluentLoginResources loggedOutPageURL(@Nullable String url) {
		resources.setLoggedOutPageURL(url);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing login resources metadata
	 */
	public @Nonnull LoginResourcesMetaData get() {
		return resources;
	}
}

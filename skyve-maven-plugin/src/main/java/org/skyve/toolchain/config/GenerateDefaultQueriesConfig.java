package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

import jakarta.annotation.Nullable;

/**
 * Stores the parameters for the default query generator mojo.
 */
public class GenerateDefaultQueriesConfig {
	/**
	 * Customer to generate default queries for.
	 */
	@Parameter
	private String customer;

	/**
	 * Module to generate default queries for.
	 */
	@Parameter
	private String module;

	/**
	 * Whether or not to include association bizKeys.
	 */
	@Parameter
	private boolean includeAssociationBizKeys = false;

	/**
	 * Returns the configured customer name.
	 *
	 * @return the customer name, or {@code null}
	 */
	public @Nullable String getCustomer() {
		return customer;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customer the customer name
	 */
	public void setCustomer(@Nullable String customer) {
		this.customer = customer;
	}

	/**
	 * Returns the configured module name.
	 *
	 * @return the module name, or {@code null}
	 */
	public @Nullable String getModule() {
		return module;
	}

	/**
	 * Sets the module name.
	 *
	 * @param module the module name
	 */
	public void setModule(@Nullable String module) {
		this.module = module;
	}

	/**
	 * Returns whether association bizKeys should be included.
	 *
	 * @return {@code true} when association bizKeys should be included
	 */
	public boolean isIncludeAssociationBizKeys() {
		return includeAssociationBizKeys;
	}

	/**
	 * Sets whether association bizKeys should be included.
	 *
	 * @param includeAssociationBizKeys whether to include association bizKeys
	 */
	public void setIncludeAssociationBizKeys(boolean includeAssociationBizKeys) {
		this.includeAssociationBizKeys = includeAssociationBizKeys;
	}
}

package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

/**
 * Stores the parameters for the edit-view generator mojo.
 */
public class GenerateEditViewConfig {
	/**
	 * Customer name.
	 */
	@Parameter(required = true)
	private String customer;

	/**
	 * Module name.
	 */
	@Parameter(required = true)
	private String module;

	/**
	 * Document name.
	 */
	@Parameter(required = true)
	private String document;

	/**
	 * Customer overriden view.
	 */
	@Parameter(required = true, defaultValue = "false")
	private boolean customerOverriden = false;

	/**
	 * UXUI overridden view name.
	 */
	@Parameter
	private String overridenViewName;

	/**
	 * Returns the configured customer name.
	 *
	 * @return the customer name, or {@code null}
	 */
	public String getCustomer() {
		return customer;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customer the customer name
	 */
	public void setCustomer(String customer) {
		this.customer = customer;
	}

	/**
	 * Returns the configured module name.
	 *
	 * @return the module name, or {@code null}
	 */
	public String getModule() {
		return module;
	}

	/**
	 * Sets the module name.
	 *
	 * @param module the module name
	 */
	public void setModule(String module) {
		this.module = module;
	}

	/**
	 * Returns the configured document name.
	 *
	 * @return the document name, or {@code null}
	 */
	public String getDocument() {
		return document;
	}

	/**
	 * Sets the document name.
	 *
	 * @param document the document name
	 */
	public void setDocument(String document) {
		this.document = document;
	}

	/**
	 * Returns whether the generated view should target a customer override.
	 *
	 * @return {@code true} when the generated view should be a customer override
	 */
	public boolean isCustomerOverriden() {
		return customerOverriden;
	}

	/**
	 * Sets whether the generated view should target a customer override.
	 *
	 * @param customerOverriden whether the generated view should be a customer override
	 */
	public void setCustomerOverriden(boolean customerOverriden) {
		this.customerOverriden = customerOverriden;
	}

	/**
	 * Returns the configured overridden view name.
	 *
	 * @return the overridden view name, or {@code null}
	 */
	public String getOverridenViewName() {
		return overridenViewName;
	}

	/**
	 * Sets the overridden view name.
	 *
	 * @param overridenViewName the overridden view name
	 */
	public void setOverridenViewName(String overridenViewName) {
		this.overridenViewName = overridenViewName;
	}
}

package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

public class GenerateDefaultQueriesConfig {
	/**
	 * Module to generate default queries for.
	 */
	@Parameter
	private String customer;

	/**
	 * Customer to generate default queries for.
	 */
	@Parameter
	private String module;

	/**
	 * Whether or not to include association bizKeys.
	 */
	@Parameter
	private boolean includeAssociationBizKeys = false;

	public String getCustomer() {
		return customer;
	}

	public void setCustomer(String customer) {
		this.customer = customer;
	}

	public String getModule() {
		return module;
	}

	public void setModule(String module) {
		this.module = module;
	}

	public boolean isIncludeAssociationBizKeys() {
		return includeAssociationBizKeys;
	}

	public void setIncludeAssociationBizKeys(boolean includeAssociationBizKeys) {
		this.includeAssociationBizKeys = includeAssociationBizKeys;
	}
}

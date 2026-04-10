package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;
import org.skyve.impl.util.UtilImpl;

public class GenerateDomainConfig {
	/**
	 * Debug mode switch.
	 */
	@Parameter(required = true, defaultValue = "false")
	private boolean debug = false;

	/**
	 * Multi-tenant switch.
	 */
	@Parameter(required = true, defaultValue = "false")
	private boolean multiTenant = false;

	/**
	 * Dialect options.
	 */
	@Parameter(required = true, defaultValue = "H2")
	private String dialect = "H2";

	/**
	 * Comma separated list of modules to exclude.
	 */
	@Parameter(property = "excludedModules")
	private String excludedModules = "";

	/**
	 * Fully-qualified class name of a {@link org.skyve.metadata.controller.Customisations} implementation
	 * to register before domain generation.
	 * The class is instantiated, set as the active Customisations singleton, and has its
	 * {@code registerCustomExpressions()} and {@code registerCustomFormatters()} methods invoked.
	 * This mirrors what the server does at startup via SkyveContextListener and is necessary so that
	 * custom expression evaluator prefixes are known at generate-domain time.
	 * Leave unset if the project uses only the built-in Skyve expression evaluators.
	 */
	@Parameter
	private String customisationsClass;

	public boolean isDebug() {
		return debug;
	}

	public void setDebug(boolean debug) {
		this.debug = debug;
	}

	public boolean isMultiTenant() {
		return multiTenant;
	}

	public void setMultiTenant(boolean multiTenant) {
		this.multiTenant = multiTenant;
	}

	public String getDialect() {
		return dialect;
	}

	public void setDialect(String dialect) {
		this.dialect = dialect;
	}

	public String getExcludedModules() {
		return excludedModules;
	}

	public void setExcludedModules(String excludedModules) {
		this.excludedModules = excludedModules;
	}

	public String getCustomisationsClass() {
		return customisationsClass;
	}

	public void setCustomisationsClass(String customisationsClass) {
		this.customisationsClass = UtilImpl.processStringValue(customisationsClass);
	}
}

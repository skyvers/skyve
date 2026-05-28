package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;
import org.skyve.impl.util.UtilImpl;

/**
 * Stores the parameters for the domain generation mojo.
 */
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

	/**
	 * Returns whether the generator should run in debug mode.
	 *
	 * @return {@code true} when debug logging is enabled
	 */
	public boolean isDebug() {
		return debug;
	}

	/**
	 * Sets whether the generator should run in debug mode.
	 *
	 * @param debug whether debug logging is enabled
	 */
	public void setDebug(boolean debug) {
		this.debug = debug;
	}

	/**
	 * Returns whether the generator should produce multi-tenant output.
	 *
	 * @return {@code true} when multi-tenant artefacts should be generated
	 */
	public boolean isMultiTenant() {
		return multiTenant;
	}

	/**
	 * Sets whether the generator should produce multi-tenant output.
	 *
	 * @param multiTenant whether multi-tenant artefacts should be generated
	 */
	public void setMultiTenant(boolean multiTenant) {
		this.multiTenant = multiTenant;
	}

	/**
	 * Returns the configured SQL dialect.
	 *
	 * @return the dialect name
	 */
	public String getDialect() {
		return dialect;
	}

	/**
	 * Sets the SQL dialect.
	 *
	 * @param dialect the dialect name
	 */
	public void setDialect(String dialect) {
		this.dialect = dialect;
	}

	/**
	 * Returns the comma-separated list of excluded modules.
	 *
	 * @return the excluded module list, or an empty string
	 */
	public String getExcludedModules() {
		return excludedModules;
	}

	/**
	 * Sets the comma-separated list of excluded modules.
	 *
	 * @param excludedModules the excluded module list
	 */
	public void setExcludedModules(String excludedModules) {
		this.excludedModules = excludedModules;
	}

	/**
	 * Returns the customisations class name.
	 *
	 * @return the customisations class name, or {@code null}
	 */
	public String getCustomisationsClass() {
		return customisationsClass;
	}

	/**
	 * Sets the customisations class name after normalising blank values.
	 *
	 * @param customisationsClass the customisations class name
	 */
	public void setCustomisationsClass(String customisationsClass) {
		this.customisationsClass = UtilImpl.processStringValue(customisationsClass);
	}
}

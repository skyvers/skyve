package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

/**
 * Stores the parameters for the new-document mojo.
 */
public class NewDocumentConfig {
	/**
	 * Default module.
	 */
	@Parameter
	private String defaultModule;

	/**
	 * Returns the default module name.
	 *
	 * @return the default module name, or {@code null}
	 */
	public String getDefaultModule() {
		return defaultModule;
	}

	/**
	 * Sets the default module name.
	 *
	 * @param defaultModule the default module name
	 */
	public void setDefaultModule(String defaultModule) {
		this.defaultModule = defaultModule;
	}
}

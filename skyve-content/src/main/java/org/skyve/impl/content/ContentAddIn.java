package org.skyve.impl.content;

import org.pf4j.PluginWrapper;
import org.skyve.addin.AddIn;

/**
 * Registers the Skyve content add-in entry point with PF4J.
 *
 * <p>Threading: lifecycle is managed by the plugin container.
 */
public class ContentAddIn extends AddIn {
	/**
	 * Creates the add-in for the supplied PF4J plugin wrapper.
	 *
	 * @param wrapper the plugin metadata and runtime wrapper supplied by PF4J
	 */
	public ContentAddIn(PluginWrapper wrapper) {
		super(wrapper);
	}
}

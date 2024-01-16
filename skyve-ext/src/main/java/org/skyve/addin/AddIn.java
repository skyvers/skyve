package org.skyve.addin;

import org.pf4j.Plugin;
import org.pf4j.PluginWrapper;

/**
 * Abstract class to extend for Skyve AddIns.
 */
public abstract class AddIn extends Plugin {
	// Expose configuration of add-in required
	
	@SuppressWarnings("deprecation")
	public AddIn(PluginWrapper wrapper) {
		super(wrapper);
	}
}

package org.skyve.addin;

import org.pf4j.Plugin;
import org.pf4j.PluginWrapper;

/**
 * Base class for PF4J-based Skyve add-ins.
 *
 * <p>To create an add-in, subclass {@code AddIn} in a separate Maven module packaged as
 * a PF4J plugin JAR. The add-in module declares its extension interfaces using
 * {@link org.pf4j.Extension @Extension} annotations on concrete implementation classes.
 *
 * <p>Extensions are loaded at runtime by {@link AddInManager} and obtained with
 * {@link AddInManager#getExtension}.
 *
 * @see AddInManager
 */
public abstract class AddIn extends Plugin {
	// Expose configuration of add-in required
	
	@SuppressWarnings("deprecation")
	public AddIn(PluginWrapper wrapper) {
		super(wrapper);
	}
}

package org.skyve.addin;

import org.skyve.metadata.controller.ApplicationContextListener;

/**
 * Used to get new Add-ins.
 */
public interface AddInManager extends ApplicationContextListener {
	/**
	 * Returns a new instance of an add-in interface implementation.
	 */
	<T extends Object> T getExtension(Class<T> type);
}

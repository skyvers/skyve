package org.skyve.addin;

import org.skyve.impl.util.SystemObserver;

/**
 * Used to get new Add-ins.
 */
public interface AddInManager extends SystemObserver {
	/**
	 * Returns a new instance of an add-in interface implementation.
	 */
	<T extends Object> T getExtension(Class<T> type);
}

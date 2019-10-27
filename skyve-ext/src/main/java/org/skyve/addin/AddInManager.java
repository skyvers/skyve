package org.skyve.addin;

/**
 * Used to get new Add-ins.
 */
public interface AddInManager {
	/**
	 * Returns a new instance of an add-in interface implementation.
	 */
	<T extends Object> T getAddIn(Class<T> type);
}

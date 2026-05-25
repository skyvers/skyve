package org.skyve.addin;

import org.skyve.impl.util.SystemObserver;

/**
 * Loads and vends PF4J-based Skyve add-in extension implementations.
 *
 * <p>An add-in is a PF4J plugin JAR placed in the Skyve add-in directory.
 * {@code AddInManager} scans and loads those plugins at application startup.
 * Callers request a concrete implementation of an extension interface using
 * {@link #getExtension(Class)}; the manager locates the appropriate loaded plugin
 * extension and returns a fresh instance.
 *
 * <p>Obtain the active {@code AddInManager} via {@link org.skyve.EXT#getAddInManager()}.
 *
 * @see AddIn
 */
public interface AddInManager extends SystemObserver {
	/**
	 * Returns a new instance of the given add-in extension interface from the first
	 * loaded plugin that provides a matching implementation, or {@code null} if no
	 * plugin provides the extension.
	 *
	 * @param <T>  the extension interface type
	 * @param type the extension interface {@link Class}; must not be {@code null}
	 * @return a new extension instance, or {@code null}
	 */
	<T extends Object> T getExtension(Class<T> type);
}

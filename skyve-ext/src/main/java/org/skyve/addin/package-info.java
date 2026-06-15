/**
 * Add-in (plugin) API for extending Skyve at runtime via PF4J.
 *
 * <p>An add-in is a PF4J plugin JAR packaged separately from the Skyve WAR and placed
 * in the configured add-in directory. The add-in declares extension implementations
 * using {@link org.pf4j.Extension @Extension}; the framework discovers and loads them
 * via {@link org.skyve.addin.AddInManager}.
 *
 * <p>To author an add-in, subclass {@link org.skyve.addin.AddIn} and annotate concrete
 * extension implementations. Obtain the active manager via {@link org.skyve.EXT#getAddInManager()}.
 *
 * @see org.skyve.addin.AddIn
 * @see org.skyve.addin.AddInManager
 */
package org.skyve.addin;

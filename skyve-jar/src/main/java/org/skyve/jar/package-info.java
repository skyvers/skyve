/**
 * Provides the standalone JAR bootstrap entry points used to run Skyve with an
 * embedded Undertow and RESTEasy stack.
 *
 * <p>Types in this package wire servlet, CDI and JAX-RS integration for local
 * execution and diagnostics. They are startup-oriented and are typically invoked
 * from a single launcher thread during process initialization.
 */
package org.skyve.jar;

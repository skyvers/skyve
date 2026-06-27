/**
 * Provides Flutter-specific Maven integration for generating a baseline Skyve Flutter application.
 *
 * <p>Threading: the mojo bootstraps CDI, persistence, and repository singletons during execution and therefore
 * must be treated as thread-confined.
 */
package org.skyve.toolchain.flutter;
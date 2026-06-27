/**
 * Provides Maven mojos and shared helpers for Skyve assembly, generation, scripting, and reporting tasks.
 *
 * <p>Threading: mojos are invoked by Maven per execution and must be treated as thread-confined; the helper
 * classes in this package mutate process-wide Skyve bootstrap state and should not be shared across parallel
 * executions.
 */
package org.skyve.toolchain;
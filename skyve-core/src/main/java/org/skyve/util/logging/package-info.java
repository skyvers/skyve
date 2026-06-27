/**
 * Skyve logging helpers and logger factory.
 *
 * <p>{@link org.skyve.util.logging.SkyveLoggerFactory} wraps SLF4J's
 * {@link org.slf4j.LoggerFactory} with Skyve-specific log sanitisation while
 * preserving compatibility with the SLF4J 1.7 API commonly exposed by Maven 3
 * plugin classloaders.
 */
package org.skyve.util.logging;

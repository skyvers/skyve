/**
 * Public utility API for Skyve applications and framework integrations.
 *
 * <p>Key utilities in this package:
 * <ul>
 *   <li>{@link org.skyve.util.Binder} — get, set, and evaluate bean attributes via
 *       dot-separated binding expressions.</li>
 *   <li>{@link org.skyve.util.DataBuilder} — construct realistic random bean instances
 *       from document metadata; used for tests and fixture generation.</li>
 *   <li>{@link org.skyve.util.JSON} — serialise and deserialise Skyve beans to/from JSON.</li>
 *   <li>{@link org.skyve.util.BeanValidator} — validate a bean against its document
 *       metadata constraints and return any validation messages.</li>
 *   <li>{@link org.skyve.util.BeanVisitor} — visitor interface for walking a bean tree.</li>
 *   <li>{@link org.skyve.util.OWASP} — input sanitisation utilities following OWASP guidelines.</li>
 *   <li>{@link org.skyve.util.Util} — miscellaneous helpers (logging, content-type detection,
 *       resource loading).</li>
 *   <li>{@link org.skyve.util.Time} — current-time utilities that are controllable in tests.</li>
 * </ul>
 *
 * <p>All methods in static utility classes are thread-safe unless otherwise documented.
 */
package org.skyve.util;

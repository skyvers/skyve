/**
 * Core framework utility classes shared across Skyve modules.
 *
 * <p>This package provides low-level utilities used throughout the framework:
 * <ul>
 *   <li>{@code UtilImpl} — the primary configuration and utility façade; holds
 *       framework-wide settings (application name, data-store name, environment, etc.)
 *       and miscellaneous helper methods. Most fields are set during application
 *       bootstrap from {@code skyve.json}.
 *   <li>{@code ValidationUtil} — validates domain object fields, constraints, and
 *       uniqueness rules; aggregates {@link org.skyve.domain.messages.ValidationMessage}
 *       instances into a {@link org.skyve.domain.messages.ValidationException}.
 *   <li>{@code VariableExpander} — expands {@code {user.xxx}} and other EL-style
 *       variable expressions in strings against the current user context.
 *   <li>{@code XMLMetaData} — JAXB marshalling and unmarshalling helpers for Skyve
 *       metadata XML files; handles namespace-aware parsing and schema validation.
 *   <li>{@code TimeUtil} — date and time arithmetic utilities.
 *   <li>{@code UUIDv7} — generates time-ordered UUID v7 identifiers for new domain
 *       object biz keys.
 *   <li>{@code SafeFileName} — sanitises strings to produce safe filesystem names.
 *   <li>{@code SecureDom4j} — a Dom4j SAX reader preconfigured to block external
 *       entity injection (XXE).
 *   <li>{@code SystemObserver} — a no-op {@link org.skyve.job.JobDescription.Percentage}
 *       observer used for headless framework operations.
 *   <li>{@code LoggingIteratorAdapter} — wraps an iterator and logs each element as
 *       it is consumed, for diagnostic purposes.
 *   <li>{@code NullTolerantBeanComparator} / {@code NullTolerantComparator} —
 *       comparators that treat {@code null} as less than any non-null value, used for
 *       in-memory sorting of domain collections.
 * </ul>
 *
 * <p>The {@code json} sub-package contains low-level JSON reading and writing utilities.
 *
 * @see org.skyve.impl.util.json
 */
package org.skyve.impl.util;

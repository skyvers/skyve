/**
 * The root Skyve framework API package.
 *
 * <p>This package contains {@link org.skyve.CORE}, the primary static entry point for
 * framework services that are always available in any Skyve runtime. The companion class
 * {@link org.skyve.EXT} (in {@code skyve-ext}) exposes additional services that require
 * a heavier runtime (mail, content management, job scheduling, etc.).
 *
 * <p>All framework services accessed through {@code CORE} are bound to the <em>current
 * thread's execution context</em>. In a servlet or EJB container this context is
 * established by the Skyve security filter and torn down at request end. In batch/job
 * code it is managed by the job scheduler. Never share a {@link org.skyve.persistence.Persistence}
 * or {@link org.skyve.metadata.user.User} obtained here across thread boundaries.
 *
 * @see org.skyve.CORE
 */
package org.skyve;

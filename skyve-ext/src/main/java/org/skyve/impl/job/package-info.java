/**
 * Background job infrastructure for Skyve applications.
 *
 * <p>{@code AbstractSkyveJob} is the base class for all Skyve jobs, bridging Quartz's
 * {@link org.quartz.InterruptableJob} with the Skyve metadata and cancellation contract.
 * {@code QuartzJobScheduler} is the production {@link org.skyve.job.JobScheduler}
 * implementation. Utility jobs include content garbage collection, state eviction,
 * BrowsCap loading, and metadata validation.
 */
package org.skyve.impl.job;

/**
 * CDI producer and injectable proxy implementations for Skyve framework services.
 *
 * <p>Each {@code *Injectable} class implements a Skyve service interface and
 * delegates to the corresponding static singleton held by the framework runtime.
 * This keeps CDI injection points stable while allowing Skyve's static lifecycle
 * management to remain the single source of truth.
 *
 * <p>{@code SkyveCDIProducer} is an {@code @ApplicationScoped} CDI producer bean
 * that exposes all injectable proxies as {@code @Produces} methods so that
 * application code can use standard {@code @Inject} annotations.
 *
 * @see org.skyve.impl.job
 */
package org.skyve.impl.cdi;

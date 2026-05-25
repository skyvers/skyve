/**
 * Metadata repository API for Skyve — the central hub for resolving all framework
 * metadata at runtime.
 *
 * <p>The repository is a layered abstraction:
 * <ol>
 *   <li>{@link org.skyve.metadata.repository.Repository} &mdash; minimal application
 *       API (resource files, router, customers, users).</li>
 *   <li>{@link org.skyve.metadata.repository.CachedRepository} &mdash; adds
 *       cache-eviction to {@code Repository}.</li>
 *   <li>{@link org.skyve.metadata.repository.ProvidedRepository} &mdash; full
 *       resolution API: modules, documents, views, bizlets, actions, models,
 *       reports, permissions, and users.</li>
 *   <li>{@link org.skyve.metadata.repository.MutableRepository} &mdash; programmatic
 *       mutation API for loading/replacing artefacts at runtime.</li>
 *   <li>{@link org.skyve.metadata.repository.OnDemandRepository} &mdash; SPI for
 *       lazy loading raw metadata from the backing store and reporting last-modified
 *       timestamps.</li>
 * </ol>
 *
 * <p>The active singleton is accessed via {@link org.skyve.CORE#getRepository()}.
 */
package org.skyve.metadata.repository;

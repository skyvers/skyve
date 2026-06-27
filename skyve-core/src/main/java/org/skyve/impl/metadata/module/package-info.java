/**
 * Internal implementation of the {@link org.skyve.metadata.module.Module} contract.
 *
 * <p>Contains:
 * <ul>
 *   <li>{@code ModuleImpl} — the full runtime module model; holds documents,
 *       queries, jobs, menu, and roles.
 *   <li>{@code JobMetaDataImpl} — implements
 *       {@link org.skyve.metadata.module.JobMetaData}; describes a scheduled job
 *       class registered in a module.
 * </ul>
 *
 * @see org.skyve.metadata.module.Module
 * @see org.skyve.impl.metadata.module.menu
 * @see org.skyve.impl.metadata.module.query
 */
package org.skyve.impl.metadata.module;

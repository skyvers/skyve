/**
 * Code generators for Skyve domain classes, JPA mappings, and view scaffolding.
 *
 * <p>This package contains the engines invoked by the {@code skyve:generateDomain}
 * Maven goal to produce all domain Java source, JPA entity mappings, and default view
 * XML. The generators read Skyve module/document XML metadata and write
 * deterministic, reproducible output to the {@code src/generated/java} and
 * {@code src/main/webapp} directories.
 *
 * <p>Key classes:
 * <ul>
 *   <li>{@link org.skyve.impl.generate.DomainGenerator} — base domain generation
 *       algorithm; traverses all modules and documents and emits Java source files.
 *   <li>{@link org.skyve.impl.generate.OverridableDomainGenerator} — extends
 *       {@code DomainGenerator} to support the customer-override pattern; generates
 *       an {@code Extension} stub class alongside the generated base.
 *   <li>{@link org.skyve.impl.generate.JPADomainGenerator} — emits JPA
 *       {@code @Entity}/{@code @MappedSuperclass} annotations and Hibernate mappings.
 *   <li>{@link org.skyve.impl.generate.ViewGenerator} — generates default view XML
 *       for documents that do not have explicit view files.
 *   <li>{@link org.skyve.impl.generate.ViewRenderer} — SPI for rendering view XML
 *       from the metadata model.
 *   <li>{@link org.skyve.impl.generate.QueryGenerator} — generates default query XML
 *       for modules.
 *   <li>{@link org.skyve.impl.generate.DialectOptions} — enum of database dialect
 *       options that influence generated DDL and column mappings.
 * </ul>
 *
 * <p><strong>Do not hand-edit generated output</strong> — changes will be overwritten
 * on the next generation run. All customisation must go through the override mechanism.
 *
 * @see org.skyve.impl.generate.client
 */
package org.skyve.impl.generate;

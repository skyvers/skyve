/**
 * Document-level metadata contracts: the Document interface, Bizlet lifecycle, relations,
 * conditions, constraints, and dynamic image support.
 *
 * <h2>Core contracts</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.model.document.Document} — the central contract for a
 *       Skyve business document. Extends {@link org.skyve.metadata.model.Model} with
 *       view resolution, action lookup, Bizlet access, and bean instantiation.</li>
 *   <li>{@link org.skyve.metadata.model.document.Bizlet} — the lifecycle callback class.
 *       Extend this to hook into {@code newInstance}, {@code preSave}, {@code postSave},
 *       {@code preDelete}, {@code validate}, {@code getDomainValues}, and render events.</li>
 *   <li>{@link org.skyve.metadata.model.document.UniqueConstraint} — a named uniqueness
 *       rule applied either to the document table or within a collection joining table.</li>
 * </ul>
 *
 * <h2>Attribute types</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.model.document.Relation} — base contract for relational
 *       attributes ({@link org.skyve.metadata.model.document.Association},
 *       {@link org.skyve.metadata.model.document.Collection},
 *       {@link org.skyve.metadata.model.document.Inverse}).</li>
 *   <li>{@link org.skyve.metadata.model.document.Reference} — extends {@code Relation} for
 *       owning side relationships; carries a reference type and optional default query.</li>
 *   <li>{@link org.skyve.metadata.model.document.Association} — a to-one reference
 *       ({@code composition}, {@code aggregation}, or {@code embedded}).</li>
 *   <li>{@link org.skyve.metadata.model.document.Collection} — a to-many reference
 *       ({@code child}, {@code composition}, or {@code aggregation}).</li>
 *   <li>{@link org.skyve.metadata.model.document.Inverse} — the non-owning side of a
 *       to-one or to-many relationship, navigating back from the referenced document.</li>
 * </ul>
 *
 * <h2>Supporting types</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.model.document.Condition} — a named boolean expression
 *       evaluated against a bean, used for view visibility and disablement rules.</li>
 *   <li>{@link org.skyve.metadata.model.document.DomainType} — classifies whether domain
 *       (drop-down) values are constant, variant, or fully dynamic per bean.</li>
 *   <li>{@link org.skyve.metadata.model.document.DynamicImage} — a programmatically
 *       generated image served to the browser for a specific bean instance.</li>
 *   <li>{@link org.skyve.metadata.model.document.Interface} — a Java interface name that
 *       the generated domain class must implement.</li>
 * </ul>
 */
package org.skyve.metadata.model.document;

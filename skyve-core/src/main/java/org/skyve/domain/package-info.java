/**
 * Core domain type hierarchy for Skyve domain objects.
 *
 * <p>Every Skyve domain object implements {@link org.skyve.domain.Bean}, which carries
 * the standard framework properties (identity, customer, data-group, user, module, and
 * document name) common to all instances. Specialisations add persistence tracking,
 * child-collection membership, and tree-hierarchy navigation:
 *
 * <ul>
 *   <li>{@link org.skyve.domain.Bean} — base interface; all domain objects implement this.
 *   <li>{@link org.skyve.domain.PersistentBean} — adds optimistic locking, versioning,
 *       flag comments, and tagging for database-backed documents.
 *   <li>{@link org.skyve.domain.TransientBean} — marker for non-persisted documents.
 *   <li>{@link org.skyve.domain.ChildBean} — for documents owned as children within a
 *       parent bean's collection; carries ordinal and a typed parent reference.
 *   <li>{@link org.skyve.domain.HierarchicalBean} — for self-referencing tree structures;
 *       carries a parent ID and lazily loaded parent/child bean lists.
 * </ul>
 *
 * <p>Dynamic variants ({@link org.skyve.domain.DynamicBean} and its subclasses) back
 * documents declared as <em>dynamic</em> in metadata, and are also used internally as
 * schema-less result carriers for aggregate and projection queries. They use a
 * {@link org.apache.commons.beanutils.LazyDynaMap} as the backing store and are not
 * generated; the framework instantiates them directly.
 *
 * <p>The {@link org.skyve.domain.PolymorphicPersistentBean} annotation is applied by the
 * domain generator to persistent classes whose document hierarchy uses a joined or
 * single-table inheritance strategy; the persistence layer uses it to determine when to
 * include a {@code THIS} projection for polymorphic dispatch.
 *
 * <p>In generated domain code, every concrete document class extends the generated base
 * class which already implements one of these interfaces. Application code and Bizlet
 * classes should program to the interface types rather than the concrete generated classes
 * where possible to remain portable across document hierarchy changes.
 *
 * @see org.skyve.domain.Bean
 * @see org.skyve.domain.PersistentBean
 * @see org.skyve.impl.domain.AbstractBean
 */
package org.skyve.domain;

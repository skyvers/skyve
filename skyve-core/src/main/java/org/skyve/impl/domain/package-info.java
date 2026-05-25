/**
 * Core domain base classes that all generated Skyve domain objects extend.
 *
 * <p>Every document in a Skyve application is code-generated as a class that ultimately
 * extends one of the abstract bases in this package. The hierarchy mirrors the
 * {@link org.skyve.domain} contract package:
 * <ul>
 *   <li>{@link org.skyve.impl.domain.AbstractBean} — root base for all domain objects.
 *       Implements change-tracking, dynamic attribute support via
 *       {@link org.apache.commons.beanutils.LazyDynaMap}, and the
 *       {@link org.skyve.domain.Bean} contract.
 *   <li>{@link org.skyve.impl.domain.AbstractTransientBean} — extends
 *       {@code AbstractBean} for documents that are never persisted.
 *   <li>{@link org.skyve.impl.domain.AbstractPersistentBean} — extends
 *       {@code AbstractBean} with JPA {@code @MappedSuperclass} support, optimistic
 *       locking, and persistence identity fields.
 *   <li>{@link org.skyve.impl.domain.ChangeTrackingArrayList} — a specialised
 *       {@code ArrayList} that records its original state in the owning bean's
 *       {@code originalValues} map on first mutation; used for transient collection
 *       change-tracking when Hibernate's {@code PersistentCollection} is not available.
 *   <li>{@link org.skyve.impl.domain.ChangedBeanVisitor} — package-private visitor
 *       that recurses the bean graph to determine whether any attribute has changed.
 *   <li>{@link org.skyve.impl.domain.DomainEnum} — internal marker interface shared
 *       by generated enum types.
 * </ul>
 *
 * <p>Threading: {@code AbstractBean} instances are thread-confined (one thread owns
 * the persistence context). Never share a bean instance across threads.
 *
 * @see org.skyve.domain
 * @see org.skyve.impl.domain.AbstractBean
 */
package org.skyve.impl.domain;

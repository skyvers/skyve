package org.skyve.domain;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a persistent bean class whose document hierarchy uses joined or single-table
 * inheritance, making the bean's effective type polymorphic at query time.
 *
 * <p>The domain generator applies this annotation to any persistent document class that
 * has at least one sub-document in its hierarchy using an inheritance strategy of
 * {@code joined} or {@code single}. The Skyve persistence layer detects this annotation
 * and includes a {@code THIS} projection in metadata document queries, enabling Hibernate
 * to correctly instantiate the most-specific concrete subtype rather than the base type.
 *
 * <p>Application code does not use this annotation directly; it is only applied by the
 * generator and read by the persistence infrastructure.
 *
 * @see org.skyve.domain.PersistentBean
 * @see org.skyve.metadata.model.Persistent.InheritanceStrategy
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE) //on class level
public @interface PolymorphicPersistentBean {
	// nothing to see here
}
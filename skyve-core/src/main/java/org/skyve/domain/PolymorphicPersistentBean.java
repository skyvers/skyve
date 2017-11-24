package org.skyve.domain;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This is an annotation used for indicating that the associated metadata document that generated the domain class 
 * has at least one sub-document in its hierarchy that uses an inheritance strategy of joined or single.
 * This is useful to know when executing metadata document queries as the query evaluator needs to include
 * the THIS projection to allow for polymorphic methods.
 * 
 * @author mike
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE) //on class level
public @interface PolymorphicPersistentBean {
	// nothing to see here
}
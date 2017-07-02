package org.skyve.util.test;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME)
@Target(TYPE)
@Inherited
public @interface SkyveFactory {

	/**
	 * Indicates that CRUD tests should be generated for this Factory's document
	 */
	boolean testDomain() default true;

	/**
	 * Indicates that Action tests should be generated for this Factory's document
	 */
	boolean testAction() default true;
}

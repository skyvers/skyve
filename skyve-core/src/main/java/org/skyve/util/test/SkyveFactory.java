package org.skyve.util.test;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import org.skyve.metadata.controller.ServerSideAction;

/**
 * Marks a document factory used by generated CRUD/action test infrastructure.
 *
 * <p>When applied to a factory class, this annotation controls which test families
 * are generated and which actions/attributes are excluded from generated scenarios.
 */
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

	/**
	 * List of Action classes to exclude test generation for. If testAction is set to true,
	 * this will be ignored as all action tests will be excluded.
	 */
	Class<? extends ServerSideAction<?>>[] excludedActions() default {};

	/**
	 * List of Attribute names to exclude in the generated testUpdate. If testDomain
	 * is set to true, this will be ignored as the update test will be excluded.
	 */
	String[] excludedUpdateAttributes() default {};

	/**
	 * <em>Optional</em> array of {@link DataMap} annotations which define filename
	 * overrides for attributes.
	 */
	DataMap[] value() default {};
}

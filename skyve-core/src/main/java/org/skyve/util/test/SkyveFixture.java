package org.skyve.util.test;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Identifies a factory method as a provider for named or typed test fixtures.
 *
 * <p>Fixture methods are discovered reflectively and selected by explicit fixture
 * names or by {@link FixtureType} categories.
 */
@Retention(RUNTIME)
@Target(METHOD)
@Inherited
public @interface SkyveFixture {
	/**
	 * Declares broad fixture categories used to select factory methods.
	 */
	public static enum FixtureType {
		/**
		 * CRUD-oriented fixture generation (create/read/update/delete flows).
		 */
		crud,

		/**
		 * SAIL automation fixture generation.
		 */
		sail,

		/**
		 * Report-oriented fixture generation.
		 */
		report,

		/**
		 * Seed-data fixture generation.
		 */
		seed
	}
	
	/**
	 * The name(s) of a fixture that the method can satisfy
	 */
	public String[] names() default {};
	
	/**
	 * The implicit fixtures that the method can satisfy
	 */
	public FixtureType[] types() default {};
}

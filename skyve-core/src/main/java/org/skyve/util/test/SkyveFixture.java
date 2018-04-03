package org.skyve.util.test;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME)
@Target(METHOD)
@Inherited
public @interface SkyveFixture {
	public static enum FixtureType {
		crud,
		sail,
		report,
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

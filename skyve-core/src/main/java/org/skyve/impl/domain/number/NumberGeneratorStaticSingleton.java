package org.skyve.impl.domain.number;

import org.skyve.domain.number.NumberGenerator;

/**
 * Singleton holder for the active {@link NumberGenerator} implementation.
 *
 * <p>The instance is set once at application startup (either to
 * {@link DocumentNumberGenerator} via {@link #setDefault()} or to a custom
 * implementation via {@link #set(NumberGenerator)}) and read thereafter;
 * no thread-safety coordination is required after initialisation.
 *
 * <p>Threading: the {@code set} / {@code setDefault} methods must be called
 * exclusively during the single-threaded startup phase. {@link #get()} is
 * safe for concurrent reads after that point.
 */
public class NumberGeneratorStaticSingleton {
	private static NumberGenerator instance;
	
	private NumberGeneratorStaticSingleton() {
		// nothing to see here
	}
	
	public static NumberGenerator get() {
		return instance;
	}
	
	public static void set(NumberGenerator instance) {
		NumberGeneratorStaticSingleton.instance = instance;
	}
	
	public static void setDefault() {
		instance = new DocumentNumberGenerator();
	}
}

package org.skyve.impl.domain.number;

import org.skyve.domain.number.NumberGenerator;

/**
 * A singleton for the number generator to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
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

package org.skyve.impl.domain.number;

import org.skyve.domain.number.NumberGenerator;

public class NumberGeneratorStaticSingleton {
	private static NumberGenerator instance;
	
	public static NumberGenerator get() {
		// Set the default
		if (instance == null) {
			instance = new DocumentNumberGenerator();
		}
		return instance;
	}
	
	public static void set(NumberGenerator instance) {
		NumberGeneratorStaticSingleton.instance = instance;
	}
}

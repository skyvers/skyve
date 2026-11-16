package org.skyve.impl.metadata.controller;

import org.skyve.metadata.controller.Customisations;

import jakarta.annotation.Nonnull;

/**
 * A singleton for the customisations to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class CustomisationsStaticSingleton {
	private static Customisations instance;
	
	private CustomisationsStaticSingleton() {
		// nothing to see here
	}
	
	public static @Nonnull Customisations get() {
		return instance;
	}
	
	public static void set(@Nonnull Customisations instance) {
		CustomisationsStaticSingleton.instance = instance;
	}
	
	public static void setDefault() {
		instance = new NoCustomisations();
	}
}

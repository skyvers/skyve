package org.skyve.impl.metadata.controller;

import org.skyve.metadata.controller.Customisations;

public class CustomisationsStaticSingleton {
	private static Customisations instance;
	
	public static Customisations get() {
		// Set the default
		if (instance == null) {
			instance = new NoCustomisations();
		}
		return instance;
	}
	
	public static void set(Customisations instance) {
		CustomisationsStaticSingleton.instance = instance;
	}
}

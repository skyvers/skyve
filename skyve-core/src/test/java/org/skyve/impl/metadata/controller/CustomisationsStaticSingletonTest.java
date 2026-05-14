package org.skyve.impl.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

public class CustomisationsStaticSingletonTest {

	@AfterEach
	@SuppressWarnings("static-method")
	void resetSingleton() {
		CustomisationsStaticSingleton.set(null);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getReturnsNullInitially() {
		CustomisationsStaticSingleton.set(null);
		assertNull(CustomisationsStaticSingleton.get());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDefaultSetsNoCustomisations() {
		CustomisationsStaticSingleton.setDefault();
		assertNotNull(CustomisationsStaticSingleton.get());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetRoundtrip() {
		NoCustomisations nc = new NoCustomisations();
		CustomisationsStaticSingleton.set(nc);
		assertNotNull(CustomisationsStaticSingleton.get());
	}
}

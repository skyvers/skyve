package org.skyve.util.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.test.SkyveFixture.FixtureType;

public class SkyveFixtureFixtureTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsFourTypes() {
		assertEquals(4, FixtureType.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfCrud() {
		assertNotNull(FixtureType.valueOf("crud"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfSail() {
		assertNotNull(FixtureType.valueOf("sail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfReport() {
		assertNotNull(FixtureType.valueOf("report"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfSeed() {
		assertNotNull(FixtureType.valueOf("seed"));
	}
}

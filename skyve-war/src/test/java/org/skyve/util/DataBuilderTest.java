package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

class DataBuilderTest extends AbstractH2Test {

	// field name constants
	private static final String FIELD_DEPTH = "depth";
	private static final String FIELD_FIXTURE = "fixture";
	private static final String FIELD_OPTIONAL_REFERENCES = "optionalReferences";
	private static final String FIELD_OPTIONAL_SCALARS = "optionalScalars";
	private static final String FIELD_REQUIRED_REFERENCES = "requiredReferences";
	private static final String FIELD_REQUIRED_SCALARS = "requiredScalars";

	private DataBuilder db;

	@Test
	void testDefaultConstructorAppliesCrudConfiguration() throws Exception {
		// call the method under test
		db = new DataBuilder();

		// verify the result
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crud");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(),
				"Default depth for crud should be MAX_VALUE");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false after crud configuration");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should remain true after crud configuration");
	}

	@Test
	void testFixtureSailAppliesSailConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crud");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");

		// call the method under test
		db.fixture(FixtureType.sail);

		// Verify SAIL configuration is applied
		assertEquals(0, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be 0 for sail");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should remain true for sail");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should remain true for sail");
	}

	@Test
	void testFixtureCrudExplicitlyAppliesCrudConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crud");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");

		// call the method under test
		db.fixture(FixtureType.crud);

		// Verify CRUD configuration is applied
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE for crud");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false after crud configuration");
	}

	@Test
	void testOverrideCrudDepth() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crud");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");

		// call the method under test
		db.depth(5);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertEquals(5, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be set to 5 (manual override)");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false after crud configuration");
	}

	@Test
	void testOverrideCrudOptionalReferences() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crud");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should be true initially");

		// call the method under test
		db.optional(false, true);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be true (manual override)");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should be false (manual override)");
	}

	@Test
	void testOverrideSailDepth() throws Exception {
		// setup the test data
		db = new DataBuilder().fixture(FixtureType.sail);

		// validate the test data
		assertEquals(FixtureType.sail.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to sail");
		assertEquals(0, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be 0 initially");

		// call the method under test
		db.depth(3);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertEquals(3, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be set to 3 (manual override)");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should remain true for sail");
	}

	@Test
	void testCustomFixtureNameNoEffectOnConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals(FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE), "Fixture should be set to crid");
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should be MAX_VALUE initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");

		// call the method under test
		db.fixture("customFixture");

		// Verify configuration remains at defaults as fixture name doesn't match known types
		assertEquals(Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue(), "Depth should remain MAX_VALUE");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should remain false");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should remain true");
	}

	@Test
	void testDeprecatedConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, "deprecated"), "deprecated should be false initially");

		// call the method under test
		db.deprecated(true);

		// verify the result
		assertTrue(getBooleanFieldValue(db, "deprecated"), "deprecated should be true");
	}

	@Test
	void testDepthConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder().depth(3);

		// validate the test data
		assertEquals(3, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());

		// call the method under test
		db.depth("associationBinding", 2);

		// verify the result
		java.util.Map<String, Integer> depths = getFieldValue(db, "depths");
		assertEquals(Integer.valueOf(2), depths.get("associationBinding"));
	}

	@Test
	void testDomainConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, "domain"), "domain should be true initially");

		// call the method under test
		db.domain(false);

		// verify the result
		assertFalse(getBooleanFieldValue(db, "domain"), "domain should be false");
	}

	@Test
	void testOptionalConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should be true initially");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be false initially");

		// call the method under test
		db.optional(false);

		// verify both optional methods updated
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should be set to false");
		assertFalse(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be set to false");

		// call the other optional method
		db.optional(true, true);

		// verify both optional methods updated again
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_SCALARS), "optionalScalars should be update to true");
		assertTrue(getBooleanFieldValue(db, FIELD_OPTIONAL_REFERENCES), "optionalReferences should be updated to true ");
	}

	@Test
	void testPersistentConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, "persistent"), "persistent should be true initially");

		// call the method under test
		db.persistent(false);

		// verify the result
		assertFalse(getBooleanFieldValue(db, "persistent"), "persistent should be false");
	}

	@Test
	void testRequiredConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, FIELD_REQUIRED_SCALARS), "requiredScalars should be true initially");
		assertTrue(getBooleanFieldValue(db, FIELD_REQUIRED_REFERENCES), "requiredReferences should be true initially");

		// call the method under test
		db.required(false);

		// verify both required methods updated
		assertFalse(getBooleanFieldValue(db, FIELD_REQUIRED_SCALARS), "requiredScalars should be set to false");
		assertFalse(getBooleanFieldValue(db, FIELD_REQUIRED_REFERENCES), "requiredReferences should be set to false");

		// call the other required method
		db.required(true, true);

		// verify both required methods updated again
		assertTrue(getBooleanFieldValue(db, FIELD_REQUIRED_SCALARS), "requiredScalars should be update to true");
		assertTrue(getBooleanFieldValue(db, FIELD_REQUIRED_REFERENCES), "requiredReferences should be updated to true ");
	}

	@Test
	void testTransientsConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, "transients"), "transients should be true initially");

		// call the method under test
		db.transients(false);

		// verify the result
		assertFalse(getBooleanFieldValue(db, "transients"), "transients should be false");
	}

	@Test
	void testViewConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue(getBooleanFieldValue(db, "view"), "view should be true initially");

		// call the method under test
		db.view(false);

		// verify the result
		assertFalse(getBooleanFieldValue(db, "view"), "view should be false");
	}

	/**
	 * Helper method to get private field value using reflection
	 */
	@SuppressWarnings("unchecked")
	private static <T> T getFieldValue(Object obj, String fieldName) throws Exception {
		Field field = obj.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return (T) field.get(obj);
	}

	private static boolean getBooleanFieldValue(Object obj, String fieldName) throws Exception {
		return ((Boolean) getFieldValue(obj, fieldName)).booleanValue();
	}
}

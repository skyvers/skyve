package org.skyve.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.util.test.SkyveFixture.FixtureType;

public class DataBuilderTest {

	// field name constants
	private static final String FIELD_DEPTH = "depth";
	private static final String FIELD_FIXTURE = "fixture";
	private static final String FIELD_OPTIONAL_REFERENCES = "optionalReferences";
	private static final String FIELD_OPTIONAL_SCALARS = "optionalScalars";
	private static final String FIELD_REQUIRED_REFERENCES = "requiredReferences";
	private static final String FIELD_REQUIRED_SCALARS = "requiredScalars";

	private DataBuilder db;

	@Test
	@SuppressWarnings("boxing")
	public void testDefaultConstructorAppliesCrudConfiguration() throws Exception {
		// call the method under test
		db = new DataBuilder();

		// verify the result
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Default depth for crud should be MAX_VALUE", Integer.MAX_VALUE,
				((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
		assertTrue("optionalScalars should remain true after crud configuration", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFixtureSailAppliesSailConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be MAX_VALUE initially", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the method under test
		db.fixture(FixtureType.sail);

		// Verify SAIL configuration is applied
		assertEquals("Depth should be 0 for sail", 0, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertTrue("optionalReferences should remain true for sail", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
		assertTrue("optionalScalars should remain true for sail", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFixtureCrudExplicitlyAppliesCrudConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be MAX_VALUE initially", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the method under test
		db.fixture(FixtureType.crud);

		// Verify CRUD configuration is applied
		assertEquals("Depth should be MAX_VALUE for crud", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOverrideCrudDepth() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be MAX_VALUE initially", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the method under test
		db.depth(5);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertEquals("Depth should be set to 5 (manual override)", 5, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOverrideCrudOptionalReferences() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be MAX_VALUE initially", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
		assertTrue("optionalScalars should be true initially", getFieldValue(db, FIELD_OPTIONAL_SCALARS));

		// call the method under test
		db.optional(false, true);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertTrue("optionalReferences should be true (manual override)", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
		assertFalse("optionalScalars should be false (manual override)", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOverrideSailDepth() throws Exception {
		// setup the test data
		db = new DataBuilder().fixture(FixtureType.sail);

		// validate the test data
		assertEquals("Fixture should be set to sail", FixtureType.sail.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be 0 initially", 0, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());

		// call the method under test
		db.depth(3);

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertEquals("Depth should be set to 3 (manual override)", 3, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertTrue("optionalReferences should remain true for sail", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCustomFixtureNameNoEffectOnConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertEquals("Fixture should be set to crid", FixtureType.crud.toString(), getFieldValue(db, FIELD_FIXTURE));
		assertEquals("Depth should be MAX_VALUE initially", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the method under test
		db.fixture("customFixture");

		// Verify configuration remains at defaults as fixture name doesn't match known types
		assertEquals("Depth should remain MAX_VALUE", Integer.MAX_VALUE, ((Integer) getFieldValue(db, FIELD_DEPTH)).intValue());
		assertFalse("optionalReferences should remain false", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
		assertTrue("optionalScalars should remain true", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDeprecatedConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("deprecated should be false initially", getFieldValue(db, "deprecated"));

		// call the method under test
		db.deprecated(true);

		// verify the result
		assertTrue("deprecated should be true", getFieldValue(db, "deprecated"));
	}

	@Test
	public void testDepthConfiguration() throws Exception {
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
	@SuppressWarnings("boxing")
	public void testDomainConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("domain should be true initially", getFieldValue(db, "domain"));

		// call the method under test
		db.domain(false);

		// verify the result
		assertFalse("domain should be false", getFieldValue(db, "domain"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOptionalConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("optionalScalars should be true initially", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
		assertFalse("optionalReferences should be false initially", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the method under test
		db.optional(false);

		// verify both optional methods updated
		assertFalse("optionalScalars should be set to false", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
		assertFalse("optionalReferences should be set to false", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));

		// call the other optional method
		db.optional(true, true);

		// verify both optional methods updated again
		assertTrue("optionalScalars should be update to true", getFieldValue(db, FIELD_OPTIONAL_SCALARS));
		assertTrue("optionalReferences should be updated to true ", getFieldValue(db, FIELD_OPTIONAL_REFERENCES));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testPersistentConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("persistent should be true initially", getFieldValue(db, "persistent"));

		// call the method under test
		db.persistent(false);

		// verify the result
		assertFalse("persistent should be false", getFieldValue(db, "persistent"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRequiredConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("requiredScalars should be true initially", getFieldValue(db, FIELD_REQUIRED_SCALARS));
		assertTrue("requiredReferences should be true initially", getFieldValue(db, FIELD_REQUIRED_REFERENCES));

		// call the method under test
		db.required(false);

		// verify both required methods updated
		assertFalse("requiredScalars should be set to false", getFieldValue(db, FIELD_REQUIRED_SCALARS));
		assertFalse("requiredReferences should be set to false", getFieldValue(db, FIELD_REQUIRED_REFERENCES));

		// call the other required method
		db.required(true, true);

		// verify both required methods updated again
		assertTrue("requiredScalars should be update to true", getFieldValue(db, FIELD_REQUIRED_SCALARS));
		assertTrue("requiredReferences should be updated to true ", getFieldValue(db, FIELD_REQUIRED_REFERENCES));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testTransientsConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("transients should be true initially", getFieldValue(db, "transients"));

		// call the method under test
		db.transients(false);

		// verify the result
		assertFalse("transients should be false", getFieldValue(db, "transients"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testViewConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();

		// validate the test data
		assertTrue("view should be true initially", getFieldValue(db, "view"));

		// call the method under test
		db.view(false);

		// verify the result
		assertFalse("view should be false", getFieldValue(db, "view"));
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
}

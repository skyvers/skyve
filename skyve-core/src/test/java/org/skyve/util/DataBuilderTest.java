package org.skyve.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.test.SkyveFixture.FixtureType;

public class DataBuilderTest {

	private DataBuilder builder;
	private User mockUser;
	private Customer mockCustomer;
	private Persistence mockPersistence;
	private AutoCloseable mockClosable;

	@Before
	public void setUp() throws Exception {
		mockClosable = MockitoAnnotations.openMocks(this);

		// Mock User and Customer
		mockUser = mock(User.class);
		mockCustomer = mock(Customer.class);
		mockPersistence = mock(AbstractPersistence.class);

		// Define mock behavior
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		when(mockPersistence.getUser()).thenReturn(mockUser);
		// Add other necessary mock behaviors here if needed by DataBuilder internals

		// Use reflection to get the ThreadLocal field
		Field tlField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		tlField.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocalPersistence = (ThreadLocal<AbstractPersistence>) tlField.get(null);

		// Set the mock persistence for the current thread
		threadLocalPersistence.set((AbstractPersistence) mockPersistence); 

		// Now instantiate DataBuilder, it will use the injected mockUser via the mock Persistence
		builder = new DataBuilder();
	}

	@After
	public void tearDown() throws Exception {
		if (mockClosable != null) {
			mockClosable.close();
		}
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDefaultConstructorAppliesCrudConfiguration() throws Exception {
		// Verify initial state (optionalReferences is true by default before config)
		assertTrue("Default optionalReferences should be true initially", getFieldValue(builder, "optionalReferences"));

		// Apply configuration implicitly triggered by build()
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify CRUD configuration is applied
		assertEquals("Default fixture should be crud", FixtureType.crud.toString(), getFieldValue(builder, "fixture"));
		assertEquals("Default depth for crud should be MAX_VALUE", Integer.MAX_VALUE, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(builder, "optionalReferences"));
		assertTrue("optionalScalars should remain true after crud configuration", getFieldValue(builder, "optionalScalars"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFixtureSailAppliesSailConfiguration() throws Exception {
		builder.fixture(FixtureType.sail);

		// Verify state before applying config
		assertEquals("Fixture should be set to sail", FixtureType.sail.toString(), getFieldValue(builder, "fixture"));
		assertEquals("Depth should be 0 initially", 0, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should be true initially", getFieldValue(builder, "optionalReferences"));

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify SAIL configuration is applied
		assertEquals("Depth should be 0 for sail", 0, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should remain true for sail", getFieldValue(builder, "optionalReferences"));
		assertTrue("optionalScalars should remain true for sail", getFieldValue(builder, "optionalScalars"));
	}
	
	@Test
	@SuppressWarnings("boxing")
	public void testFixtureCrudExplicitlyAppliesCrudConfiguration() throws Exception {
		builder.fixture(FixtureType.crud);

		// Verify state before applying config
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(builder, "fixture"));
		assertTrue("optionalReferences should be true initially", getFieldValue(builder, "optionalReferences"));

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify CRUD configuration is applied
		assertEquals("Depth should be MAX_VALUE for crud", Integer.MAX_VALUE, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(builder, "optionalReferences"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOverrideCrudDepth() throws Exception {
		builder.fixture(FixtureType.crud).depth(5);

		// Verify state before applying config
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(builder, "fixture"));
		assertEquals("Depth should be set to 5 manually", 5, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should be true initially", getFieldValue(builder, "optionalReferences"));

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify manual depth overrides fixture default, but other fixture settings apply
		assertEquals("Depth should remain 5 (manual override)", 5, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertFalse("optionalReferences should be false after crud configuration", getFieldValue(builder, "optionalReferences"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOverrideCrudOptionalReferences() throws Exception {
		// Note: optional(true, true) sets it back to the initial default state
		builder.fixture(FixtureType.crud).optional(true, true); 

		// Verify state before applying config
		assertEquals("Fixture should be set to crud", FixtureType.crud.toString(), getFieldValue(builder, "fixture"));
		assertTrue("optionalReferences should be set to true manually", getFieldValue(builder, "optionalReferences"));
		assertTrue("optionalScalars should be set to true manually", getFieldValue(builder, "optionalScalars"));

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify manual optionalReferences overrides fixture default, but other fixture settings apply
		assertEquals("Depth should be MAX_VALUE for crud", Integer.MAX_VALUE, ((Integer)getFieldValue(builder, "depth")).intValue());
		// Check if the logic in applyFixtureConfiguration correctly detects manual setting
		// The current logic checks `if (optionalReferences && optionalScalars)` before setting crud defaults.
		// Since we set them both to true, the condition will be true, and optionalReferences *will* be set to false.
		// This might be desired (fixture always wins unless setting is different from default)
		// Or might be undesired (manual setting should always win). Let's assume current logic is intended.
		assertFalse("optionalReferences should be false (fixture wins as manual setting matched default)", getFieldValue(builder, "optionalReferences"));
		assertTrue("optionalScalars should remain true", getFieldValue(builder, "optionalScalars"));
		
		// Let's add a variation where the manual setting differs from the initial default
		builder = new DataBuilder(); // Reset builder
		builder.optional(false, false); // Set to non-default before fixture
		builder.fixture(FixtureType.crud);
		invokeMethod(builder, "applyFixtureConfiguration");
		assertFalse("optionalReferences should be false (manual override)", getFieldValue(builder, "optionalReferences"));
		assertFalse("optionalScalars should be false (manual override)", getFieldValue(builder, "optionalScalars"));
		assertEquals("Depth should be MAX_VALUE for crud", Integer.MAX_VALUE, ((Integer)getFieldValue(builder, "depth")).intValue());
	}
	
	@Test
	@SuppressWarnings("boxing")
	public void testOverrideSailDepth() throws Exception {
		builder.fixture(FixtureType.sail).depth(3);

		// Verify state before applying config
		assertEquals("Fixture should be set to sail", FixtureType.sail.toString(), getFieldValue(builder, "fixture"));
		assertEquals("Depth should be set to 3 manually", 3, ((Integer)getFieldValue(builder, "depth")).intValue());

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify manual depth overrides fixture default
		assertEquals("Depth should remain 3 (manual override)", 3, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should remain true for sail", getFieldValue(builder, "optionalReferences"));
	}
	
	@Test
	@SuppressWarnings("boxing")
	public void testFixtureNameNoEffectOnConfiguration() throws Exception {
		builder.fixture("customFixture");

		// Verify state before applying config
		assertEquals("Fixture should be set to customFixture", "customFixture", getFieldValue(builder, "fixture"));
		assertEquals("Depth should be 0 initially", 0, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should be true initially", getFieldValue(builder, "optionalReferences"));

		// Apply configuration
		invokeMethod(builder, "applyFixtureConfiguration");

		// Verify configuration remains at defaults as fixture name doesn't match known types
		assertEquals("Depth should remain 0", 0, ((Integer)getFieldValue(builder, "depth")).intValue());
		assertTrue("optionalReferences should remain true", getFieldValue(builder, "optionalReferences"));
		assertTrue("optionalScalars should remain true", getFieldValue(builder, "optionalScalars"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRequiredConfiguration() throws Exception {
		builder.required(false);
		assertFalse("requiredScalars should be false", getFieldValue(builder, "requiredScalars"));
		assertFalse("requiredReferences should be false", getFieldValue(builder, "requiredReferences"));

		builder.required(true, false);
		assertTrue("requiredScalars should be true", getFieldValue(builder, "requiredScalars"));
		assertFalse("requiredReferences should be false", getFieldValue(builder, "requiredReferences"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testOptionalConfiguration() throws Exception {
		builder.optional(false);
		assertFalse("optionalScalars should be false", getFieldValue(builder, "optionalScalars"));
		assertFalse("optionalReferences should be false", getFieldValue(builder, "optionalReferences"));

		builder.optional(false, true);
		assertFalse("optionalScalars should be false", getFieldValue(builder, "optionalScalars"));
		assertTrue("optionalReferences should be true", getFieldValue(builder, "optionalReferences"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testPersistentConfiguration() throws Exception {
		builder.persistent(false);
		assertFalse("persistent should be false", getFieldValue(builder, "persistent"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testTransientsConfiguration() throws Exception {
		builder.transients(false);
		assertFalse("transients should be false", getFieldValue(builder, "transients"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testViewConfiguration() throws Exception {
		builder.view(false);
		assertFalse("view should be false", getFieldValue(builder, "view"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDomainConfiguration() throws Exception {
		builder.domain(false);
		assertFalse("domain should be false", getFieldValue(builder, "domain"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDeprecatedConfiguration() throws Exception {
		builder.deprecated(false);
		assertFalse("deprecated should be false", getFieldValue(builder, "deprecated"));
	}

	@Test
	public void testTypeConfiguration() throws Exception {
		builder.type(AttributeType.integer, false);
		java.util.Map<AttributeType, Boolean> types = getFieldValue(builder, "types");
		assertEquals(Boolean.FALSE, types.get(AttributeType.integer));
	}

	@Test
	public void testNameConfiguration() throws Exception {
		builder.name("attributeName", false);
		java.util.Map<String, Boolean> names = getFieldValue(builder, "names");
		assertEquals(Boolean.FALSE, names.get("attributeName"));
	}

	@Test
	public void testCardinalityConfiguration() throws Exception {
		builder.cardinality("collectionBinding", 5);
		java.util.Map<String, Integer> cardinalities = getFieldValue(builder, "cardinalities");
		assertEquals(Integer.valueOf(5), cardinalities.get("collectionBinding"));
	}

	@Test
	public void testDepthConfiguration() throws Exception {
		builder.depth(3);
		assertEquals(3, ((Integer) getFieldValue(builder, "depth")).intValue());

		builder.depth("associationBinding", 2);
		java.util.Map<String, Integer> depths = getFieldValue(builder, "depths");
		assertEquals(Integer.valueOf(2), depths.get("associationBinding"));
	}

	// Helper method to get private field value using reflection
	@SuppressWarnings("unchecked")
	private static <T> T getFieldValue(Object obj, String fieldName) throws Exception {
		Field field = obj.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return (T) field.get(obj);
	}

	// Helper method to invoke private method using reflection
	private static void invokeMethod(Object obj, String methodName) throws Exception {
		Method method = obj.getClass().getDeclaredMethod(methodName);
		method.setAccessible(true);
		method.invoke(obj);
	}
} 
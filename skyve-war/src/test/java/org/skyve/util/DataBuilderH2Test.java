package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Job.JobExtension;
import modules.admin.Job.JobFactory;
import modules.admin.Tag.TagExtension;
import modules.admin.Tag.TagFactory;
import modules.admin.domain.Contact;
import modules.admin.domain.Job;
import modules.admin.domain.Tag;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * DataBuilder tests which require Skyve's persistence and repositories to be bootstrapped.
 */
class DataBuilderH2Test extends AbstractH2Test {

	private DataBuilder db;

	@Test
	void testCardinalityConfiguration() throws Exception {
		// setup the test data
		final int expectedCardinality = 3;
		db = new DataBuilder();

		// use factoryBuild to ignore anything in the UserFactory
		User result1 = db.optional(true, true).factoryBuild(User.MODULE_NAME, User.DOCUMENT_NAME);

		// validate the test data
		assertNull(getFieldValue(db, "cardinalities"), "cardinality should be null initially");
		assertEquals(1, result1.getGroups().size(), "default cardinality should be 1");
		assertEquals(1, result1.getRoles().size(), "default cardinality should be 1");

		// call the method under test
		db.cardinality(User.groupsPropertyName, expectedCardinality);
		User result2 = db.factoryBuild(User.MODULE_NAME, User.DOCUMENT_NAME);

		// verify the result
		java.util.Map<String, Integer> cardinalities = getFieldValue(db, "cardinalities");
		assertEquals(Integer.valueOf(expectedCardinality), cardinalities.get(User.groupsPropertyName));
		assertEquals(expectedCardinality, result2.getGroups().size(), "groups collection should now have specified cardinality");
		assertEquals(1, result2.getRoles().size(), "roles collection should still have the default cardinality");
	}

	@Test
	void testFixtureCrudBuildConstructsRandomInstance() {
		// call the method under test
		db = new DataBuilder().fixture(FixtureType.crud);

		// call the method under test
		Contact result = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is(notNullValue()));
		assertThat(result.getContactType(), is(notNullValue()));
		assertThat(result.getEmail1(), is(notNullValue()));
		assertThat(result.getMobile(), is(notNullValue()));
		assertThat("random images are not supported", result.getImage(), is(nullValue()));
	}

	@Test
	void testFixtureCrudFactoryBuildConstructsRandomInstance() {
		// call the method under test
		db = new DataBuilder().fixture(FixtureType.crud);

		// call the method under test
		Contact result = db.factoryBuild(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is(notNullValue()));
		assertThat(result.getContactType(), is(notNullValue()));
		assertThat(result.getEmail1(), is(notNullValue()));
		assertThat(result.getMobile(), is(notNullValue()));
		assertThat("random images are not supported", result.getImage(), is(nullValue()));
	}

	@Test
	void testNameConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();
		Contact result1 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		// validate the test data
		assertNull(getFieldValue(db, "names"), "name should be null initially");
		assertThat(result1.getEmail1(), is(notNullValue()));

		// call the method under test
		db.name(Contact.email1PropertyName, false);
		Contact result2 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		// verify the result
		java.util.Map<String, Boolean> types = getFieldValue(db, "names");
		assertEquals(Boolean.FALSE, types.get(Contact.email1PropertyName));
		assertThat("email1 attribute should now be excluded", result2.getEmail1(), is(nullValue()));
	}

	@Test
	void testTypeConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();
		// use factoryBuild to ignore the TagFactory, whose own DataBuilder has no type configuration
		Tag result1 = db.factoryBuild(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		// validate the test data
		assertNull(getFieldValue(db, "types"), "type should be null initially");
		assertThat(result1.getVisible(), is(notNullValue()));

		// call the method under test
		db.type(AttributeType.bool, false);
		Tag result2 = db.factoryBuild(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		// verify the result
		java.util.Map<AttributeType, Boolean> types = getFieldValue(db, "types");
		assertEquals(Boolean.FALSE, types.get(AttributeType.bool));
		assertThat("boolean attributes should now be excluded", result2.getVisible(), is(nullValue()));
	}

	/**
	 * The trace shows that TagFactory's crud fixture made the Tag, as a random build makes a
	 * TagExtension too.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testTagFactoryBuildsTheCrudFixture() {
		StringBuilder trace = new StringBuilder();
		Tag result = tracedCrudBuild(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, trace);

		assertAll(() -> assertTrue(trace.indexOf("Invoking method " + TagFactory.class.getName() + "::crudInstance for fixture crud") >= 0, trace::toString),
					() -> assertInstanceOf(TagExtension.class, result),
					() -> assertEquals(Contact.MODULE_NAME, result.getUploadModuleName()),
					() -> assertEquals(Contact.DOCUMENT_NAME, result.getUploadDocumentName()));
	}

	/**
	 * The trace shows that JobFactory's crud fixture made the Job, as a random build makes a
	 * JobExtension too.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testJobFactoryBuildsTheCrudFixture() {
		StringBuilder trace = new StringBuilder();
		Job result = tracedCrudBuild(Job.MODULE_NAME, Job.DOCUMENT_NAME, trace);

		assertAll(() -> assertTrue(trace.indexOf("Invoking method " + JobFactory.class.getName() + "::crudInstance for fixture crud") >= 0, trace::toString),
					() -> assertInstanceOf(JobExtension.class, result));
	}

	/**
	 * Builds a bean of the document for the crud fixture with DataBuilder tracing on.
	 *
	 * @param trace	Receives what the trace printed.
	 */
	private static <T extends Bean> T tracedCrudBuild(String moduleName, String documentName, StringBuilder trace) {
		PrintStream out = System.out;
		ByteArrayOutputStream traced = new ByteArrayOutputStream();
		System.setOut(new PrintStream(traced, true, StandardCharsets.UTF_8));
		DataBuilder.setTrace(true);
		try {
			return new DataBuilder().fixture(FixtureType.crud).build(moduleName, documentName);
		}
		finally {
			DataBuilder.setTrace(false);
			System.setOut(out);
			trace.append(traced.toString(StandardCharsets.UTF_8));
		}
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

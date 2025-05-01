package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Contact;
import modules.admin.domain.Tag;
import util.AbstractH2Test;

/**
 * DataBuilder tests which require Skyve's persistence and repositories to be bootstrapped.
 */
public class DataBuilderH2Test extends AbstractH2Test {

	private DataBuilder db;

	@Test
	public void testFixtureCrudBuildConstructsRandomInstance() {
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
	public void testFixtureCrudFactoryBuildConstructsRandomInstance() {
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
	public void testNameConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();
		Contact result1 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		// validate the test data
		assertNull("name should be null initially", getFieldValue(db, "names"));
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
	public void testTypeConfiguration() throws Exception {
		// setup the test data
		db = new DataBuilder();
		Tag result1 = db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		// validate the test data
		assertNull("type should be null initially", getFieldValue(db, "types"));
		assertThat(result1.getVisible(), is(notNullValue()));

		// call the method under test
		db.type(AttributeType.bool, false);
		Tag result2 = db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		// verify the result
		java.util.Map<AttributeType, Boolean> types = getFieldValue(db, "types");
		assertEquals(Boolean.FALSE, types.get(AttributeType.bool));
		assertThat("boolean attributes should now be excluded", result2.getVisible(), is(nullValue()));
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

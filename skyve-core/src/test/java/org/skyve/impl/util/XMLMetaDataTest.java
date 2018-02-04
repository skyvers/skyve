package org.skyve.impl.util;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.repository.document.BizKey;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.impl.metadata.repository.module.ModuleDocument;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;

/**
 * This test depends on the schemas being up to data from skyve-ee/src/skyve/schemas into
 * src/test/resources/schemas in this project.
 */
public class XMLMetaDataTest {

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentScalarAttribute() throws Exception {
		// setup the test data
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentAggregatedAssociation() throws Exception {
		// setup the test data
		DocumentMetaData document = createDocument();
		AssociationImpl association = createAssociation();

		document.getAttributes().add(association);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("type=\"aggregation\""), is(true));
		assertThat(result.contains("<displayName>Association 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentComposedAssociation() throws Exception {
		// setup the test data
		DocumentMetaData document = createDocument();

		AssociationImpl association = createAssociation();
		association.setType(AssociationType.composition);

		document.getAttributes().add(association);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("type=\"composition\""), is(true));
		assertThat(result.contains("<displayName>Association 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentChildCollection() throws Exception {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		CollectionImpl collection = createCollection();
		collection.setType(CollectionType.child);

		document.getAttributes().add(collection);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"Test\""), is(true));
		assertThat(result.contains("type=\"child\""), is(true));
		assertThat(result.contains("<displayName>Collection 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentParentDocument() throws Exception {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("Parent");

		document.setParentDocument(parent);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"Test\""), is(true));
		assertThat(result.contains("<parentDocument>Parent</parentDocument>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentRemovesEmptyChildElements() throws Exception {
		// setup the test data
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);
		document.getAttributes().add(createAssociation());

		// validate the test data
		assertThat(document.getAttributes().size(), is(2));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat("XML should not contain 'conditions'", result.contains("conditions"), is(false));
		assertThat("XML should not contain 'implements'", result.contains("implements"), is(false));
		assertThat("XML should not contain 'uniqueConstraints'", result.contains("uniqueConstraints"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentRemovesDefaultAttributes() throws Exception {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		Field field = createAttribute();

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"Test\""), is(true));

		Pattern p1 = Pattern.compile("<boolean.*persistent");
		Matcher m1 = p1.matcher(result);

		Pattern p2 = Pattern.compile("<boolean.*deprecated");
		Matcher m2 = p2.matcher(result);

		Pattern p3 = Pattern.compile("<boolean.*required");
		Matcher m3 = p3.matcher(result);

		assertThat("The persistent attribute should not be present", m1.find(), is(false));
		assertThat("The deprecated attribute should not be present", m2.find(), is(false));
		assertThat("The required attribute should not be present", m3.find(), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalDocumentKeepsNonDefaultAttributes() throws Exception {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		Field field = createAttribute();
		field.setRequired(true);
		field.setPersistent(false);
		field.setDeprecated(true);

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		// System.out.println(result);

		assertThat(result.contains("name=\"Test\""), is(true));

		Pattern p1 = Pattern.compile("<boolean.*persistent");
		Matcher m1 = p1.matcher(result);

		Pattern p2 = Pattern.compile("<boolean.*deprecated");
		Matcher m2 = p2.matcher(result);

		Pattern p3 = Pattern.compile("<boolean.*required");
		Matcher m3 = p3.matcher(result);

		assertThat("The persistent attribute should be present", m1.find(), is(true));
		assertThat("The deprecated attribute should be present", m2.find(), is(true));
		assertThat("The required attribute should be present", m3.find(), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testMarshalModuleRemovesEmptyChildElements() throws Exception {
		// setup the test data
		ModuleMetaData module = createModule();
		DocumentMetaData document = createDocument();

		ModuleDocument md = new ModuleDocument();
		md.setRef(document.getName());
		module.getDocuments().add(md);

		// validate the test data
		assertThat(module.getDocuments().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalModule(module, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		System.out.println(result);

		assertThat(result.contains("name=\"test\""), is(true));
		assertThat("XML should contain 'documents'", result.contains("<documents"), is(true));
		assertThat("XML should contain 'roles'", result.contains("<roles"), is(true));

		assertThat("XML should not contain 'jobs'", result.contains("<jobs"), is(false));
		assertThat("XML should not contain 'queries'", result.contains("<queries"), is(false));
	}

	private static AssociationImpl createAssociation() {
		AssociationImpl association = new AssociationImpl();
		association.setName("att1");
		association.setDisplayName("Association 1");
		association.setDocumentName("TestDocument");
		association.setType(AssociationType.aggregation);
		return association;
	}

	private static Field createAttribute() {
		Field field = new org.skyve.impl.metadata.model.document.field.Boolean();
		field.setName("att1");
		field.setDisplayName("Attribute 1");
		return field;
	}

	private static CollectionImpl createCollection() {
		CollectionImpl collection = new CollectionImpl();
		collection.setName("att1");
		collection.setDisplayName("Collection 1");
		collection.setDocumentName("TestDocument");
		return collection;
	}

	private static DocumentMetaData createDocument() {
		DocumentMetaData document = new DocumentMetaData();
		document.setName("TestDocument");
		document.setSingularAlias("Test Document");
		document.setPluralAlias("Test Document");
		BizKey bizKey = new BizKey();
		bizKey.setExpression("Test Document");
		document.setBizKey(bizKey);

		return document;
	}

	private static ModuleMetaData createModule() {
		ModuleMetaData module = new ModuleMetaData();
		module.setName("test");
		module.setTitle("Test Module");

		return module;
	}

}

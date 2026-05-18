package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.function.Executable;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.HTMLResourcesMetaData;
import org.skyve.impl.metadata.repository.document.BizKey;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.fluent.FluentListGrid;
import org.skyve.metadata.view.fluent.FluentView;

/**
 * This test depends on the schemas being up to date from skyve-war/src/main/java/schemas into skyve-core/src/test/resources/schemas.
 */
class XMLMetaDataTest {
	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentScalarAttribute() {
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

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentIntegerAttributeValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		IntegerValidator v = new IntegerValidator();
		v.setXmlMin("0");
		v.setXmlMax("10");
		v.setValidationMessage("Value must be between 0 and 10");

		field.setValidator(v);

		document.getAttributes().add(field);

		document.convert("Test Document");
		
		assertThat(v.getMin(), is(0));
		assertThat(v.getMax(), is(10));

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<validator max=\"10\" min=\"0\" validationMessage=\"Value must be between 0 and 10\"/>"),
				is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentLongIntegerAttributeValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		LongInteger field = createLongIntegerAttribute();
		LongValidator v = new LongValidator();
		v.setXmlMax("10");
		v.setXmlMin("0");

		field.setValidator(v);

		document.getAttributes().add(field);

		document.convert("TestMetaData");

		assertThat(v.getMin(), is(0L));
		assertThat(v.getMax(), is(10L));

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<validator max=\"10\" min=\"0\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentDateAttributeConverter() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Date field = createDateAttribute();

		field.setConverterName(ConverterName.DD_MM_YYYY);

		document.getAttributes().add(field);

		document.convert("TestDocument");
		
		assertThat(field.getConverter(), is(notNullValue()));

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<converterName>DD_MM_YYYY</converterName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentTextAttributeFormatCase() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextFormat f = new TextFormat();
		f.setCase(TextCase.capital);

		field.setFormat(f);

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<format case=\"capital\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentTextAttributeFormatMask() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextFormat f = new TextFormat();
		f.setMask("AAA");

		field.setFormat(f);

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<format mask=\"AAA\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentTextAttributeRegularExpressionValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setRegularExpression("\\d");

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<validator regularExpression=\"\\d\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentTextAttributeTypeValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setType(ValidatorType.creditCard);

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<validator regularExpression=\"^\\d{15,16}$\" type=\"creditCard\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentTextAttributeValidationMessageValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setValidationMessage("This is required");

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertThat(document.getAttributes().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("<displayName>Attribute 1</displayName>"), is(true));
		assertThat(result.contains("<validator validationMessage=\"This is required\"/>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentAggregatedAssociation() {
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

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("type=\"aggregation\""), is(true));
		assertThat(result.contains("<displayName>Association 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentComposedAssociation() {
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

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat(result.contains("type=\"composition\""), is(true));
		assertThat(result.contains("<displayName>Association 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentChildCollection() {
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

		assertThat(result.contains("name=\"Test\""), is(true));
		assertThat(result.contains("type=\"child\""), is(true));
		assertThat(result.contains("<displayName>Collection 1</displayName>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentParentDocument() {
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

		assertThat(result.contains("name=\"Test\""), is(true));
		assertThat(result.contains("<parentDocument>Parent</parentDocument>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentRemovesEmptyChildElements() {
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

		assertThat(result.contains("name=\"TestDocument\""), is(true));
		assertThat("XML should not contain 'conditions'", result.contains("conditions"), is(false));
		assertThat("XML should not contain 'implements'", result.contains("implements"), is(false));
		assertThat("XML should not contain 'uniqueConstraints'", result.contains("uniqueConstraints"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentRemovesDefaultAttributeAndElements() {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		Field field = createAttribute();

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"Test\""), is(true));

		// transient element should have been removed as it is the default value of false
		assertThat(result.contains("<transient>"), is(false));

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
	void testMarshalDocumentKeepsNonDefaultAttributeAndElements() {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		Field field = createAttribute();
		field.setRequired(true);
		field.setPersistent(false);
		field.setDeprecated(true);
		field.setTransient(true);

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"Test\""), is(true));

		// transient element should not have been removed as it is not the default value of false
		assertThat(result.contains("<transient>true</transient>"), is(true));

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
	void testMarshalViewListGrid() {
		// setup the test data
		FluentView v = new FluentView().title("Test").name(ViewType.edit.toString());
		v.addListGrid(new FluentListGrid().queryName("qQuery"));
		ViewMetaData view = v.get();

		// call the method under test
		String result = XMLMetaData.marshalView(view, false, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"edit\""), is(true));
		assertThat(result.contains("onEditedHandlers"), is(false));
		assertThat(result.contains("onDeletedHandlers"), is(false));
		assertThat(result.contains("onSelectedHandlers"), is(false));
		assertThat(result.contains("newParameters"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalModuleRemovesEmptyChildElements() {
		// setup the test data
		ModuleMetaData module = createModule();
		DocumentMetaData document = createDocument();

		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		md.setRef(document.getName());
		module.getDocuments().add(md);

		// validate the test data
		assertThat(module.getDocuments().size(), is(1));

		// call the method under test
		String result = XMLMetaData.marshalModule(module, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"test\""), is(true));
		assertThat("XML should contain 'documents'", result.contains("<documents"), is(true));
		assertThat("XML should contain 'roles'", result.contains("<roles"), is(true));

		assertThat("XML should not contain 'jobs'", result.contains("<jobs"), is(false));
		assertThat("XML should not contain 'queries'", result.contains("<queries"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalCustomerRemovesEmptyChildElements() {
		// setup the test data
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("test");
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH24_MI);
		customer.setDefaultTimeConverter(ConverterName.HH24_MI);
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH24_MI_SS);
		customer.setHtmlResources(new HTMLResourcesMetaData());
		
		// call the method under test
		String result = XMLMetaData.marshalCustomer(customer);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertThat(result.contains("name=\"test\""), is(true));
		assertThat("XML should not contain 'textSearchRoles'", result.contains("<textSearchRoles"), is(false));
		assertThat("XML should not contain 'flagRoles'", result.contains("<flagRoles"), is(false));
		assertThat("XML should not contain 'switchModeRoles'", result.contains("<switchModeRoles"), is(false));
		assertThat("XML should not contain 'interceptors'", result.contains("<interceptors"), is(false));
		assertThat("XML should not contain 'observers'", result.contains("<observers"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentFieldWithGeneratedInsert() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.insert);

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.contains("<generated>insert</generated>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentFieldWithGeneratedAlways() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.always);

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.contains("<generated>always</generated>"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentFieldWithoutGeneratedOmitsElement() {
		// setup the test data - no generated set
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("generated element should be absent when not set", result.contains("<generated>"), is(false));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testRoundTripFieldGeneratedInsert() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.insert);

		document.getAttributes().add(field);

		// marshal then unmarshal
		String xml = XMLMetaData.marshalDocument(document, false);
		DocumentMetaData roundTripped = XMLMetaData.unmarshalDocumentString(xml);

		// verify the result
		assertThat(roundTripped.getAttributes().size(), is(1));
		org.skyve.impl.metadata.model.document.field.Integer roundTrippedField =
				(org.skyve.impl.metadata.model.document.field.Integer) roundTripped.getAttributes().get(0);
		assertThat(roundTrippedField.getGenerated(), is(GeneratedType.insert));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testRoundTripFieldGeneratedAlways() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.always);

		document.getAttributes().add(field);

		// marshal then unmarshal
		String xml = XMLMetaData.marshalDocument(document, false);
		DocumentMetaData roundTripped = XMLMetaData.unmarshalDocumentString(xml);

		// verify the result
		assertThat(roundTripped.getAttributes().size(), is(1));
		org.skyve.impl.metadata.model.document.field.Integer roundTrippedField =
				(org.skyve.impl.metadata.model.document.field.Integer) roundTripped.getAttributes().get(0);
		assertThat(roundTrippedField.getGenerated(), is(GeneratedType.always));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testConvertThrowsWhenGeneratedSetOnNonPersistentDocument() {
		// setup the test data - document has no <persistent> element (transient document)
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.insert);
		document.getAttributes().add(field);

		// call the method under test and verify it throws
		Executable convert = () -> document.convert("TestDocument");
		MetaDataException ex = assertThrows(MetaDataException.class, convert);
		assertThat(ex.getMessage().contains("[generated]"), is(true));
		assertThat(ex.getMessage().contains("non-persistent"), is(true));
		assertThat(ex.getMessage().contains(field.getName()), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testConvertThrowsWhenGeneratedSetOnNonPersistentField() {
		// setup the test data - document IS persistent, but the field has persistent="false"
		DocumentMetaData document = createDocument();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDocument");
		document.setPersistent(persistent);
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setPersistent(false);
		field.setGenerated(GeneratedType.always);
		document.getAttributes().add(field);

		// call the method under test and verify it throws
		Executable convert = () -> document.convert("TestDocument");
		MetaDataException ex = assertThrows(MetaDataException.class, convert);
		assertThat(ex.getMessage().contains("[generated]"), is(true));
		assertThat(ex.getMessage().contains("non-persistent"), is(true));
		assertThat(ex.getMessage().contains(field.getName()), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConvertSucceedsWhenGeneratedSetOnPersistentField() {
		// setup the test data - document IS persistent, field has default persistence
		DocumentMetaData document = createDocument();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDocument");
		document.setPersistent(persistent);
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.insert);
		document.getAttributes().add(field);

		// convert should succeed without exception
		document.convert("TestDocument");

		assertThat(field.getGenerated(), is(GeneratedType.insert));
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

	private static org.skyve.impl.metadata.model.document.field.Date createDateAttribute() {
		org.skyve.impl.metadata.model.document.field.Date field = new org.skyve.impl.metadata.model.document.field.Date();
		field.setName("att1");
		field.setDisplayName("Attribute 1");
		return field;
	}

	private static LongInteger createLongIntegerAttribute() {
		LongInteger field = new org.skyve.impl.metadata.model.document.field.LongInteger();
		field.setName("att1");
		field.setDisplayName("Attribute 1");
		return field;
	}

	private static Text createTextAttribute() {
		Text field = new org.skyve.impl.metadata.model.document.field.Text();
		field.setName("att1");
		field.setDisplayName("Attribute 1");
		return field;
	}

	private static org.skyve.impl.metadata.model.document.field.Integer createIntegerAttribute() {
		org.skyve.impl.metadata.model.document.field.Integer field = new org.skyve.impl.metadata.model.document.field.Integer();
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
		document.setDocumentation("<&\">");
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

	@Test
	@SuppressWarnings("static-method")
	void testMarshalRouterRoundtrips() throws Exception {
		Router router = new Router();
		router.setUxuiSelectorClassName("org.skyve.impl.metadata.repository.router.TaggingUxUiSelector");
		String xml = XMLMetaData.marshalRouter(router);
		assertThat(xml, is(notNullValue()));
		Router roundTripped = XMLMetaData.unmarshalRouterString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getUxuiSelectorClassName(), is("org.skyve.impl.metadata.repository.router.TaggingUxUiSelector"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalRouterProducesXml() {
		Router router = new Router();
		String xml = XMLMetaData.marshalRouter(router);
		assertThat(xml, is(notNullValue()));
		assertThat(Boolean.valueOf(xml.contains("router")), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalRouterStringReturnsRouter() {
		String xml = XMLMetaData.marshalRouter(new Router());
		Router result = XMLMetaData.unmarshalRouterString(xml);
		assertThat(result, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalBizletRoundtrips() throws Exception {
		BizletMetaData bizlet = new BizletMetaData();
		bizlet.setDocumentation("Test bizlet");
		String xml = XMLMetaData.marshalBizlet(bizlet, false);
		assertThat(xml, is(notNullValue()));
		BizletMetaData roundTripped = XMLMetaData.unmarshalBizletString(xml);
		assertThat(roundTripped, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalBizletCustomerOverriddenRoundtrips() throws Exception {
		BizletMetaData bizlet = new BizletMetaData();
		String xml = XMLMetaData.marshalBizlet(bizlet, true);
		assertThat(xml, is(notNullValue()));
		BizletMetaData roundTripped = XMLMetaData.unmarshalBizletString(xml);
		assertThat(roundTripped, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalActionRoundtrips() throws Exception {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		String xml = XMLMetaData.marshalAction(action, false);
		assertThat(xml, is(notNullValue()));
		ActionMetaData roundTripped = XMLMetaData.unmarshalActionString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("TestAction"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalActionCustomerOverriddenRoundtrips() throws Exception {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		String xml = XMLMetaData.marshalAction(action, true);
		assertThat(xml, is(notNullValue()));
		ActionMetaData roundTripped = XMLMetaData.unmarshalActionString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("TestAction"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalCustomerRoundtrips() throws Exception {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("testcustomer");
		String xml = XMLMetaData.marshalCustomer(customer);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("testcustomer"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalCustomerProducesXml() {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("acme");
		String xml = XMLMetaData.marshalCustomer(customer);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("acme"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalModuleRoundtrips() throws Exception {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("Test Module");
		module.setName("testmodule");
		String xml = XMLMetaData.marshalModule(module, false);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("Test Module"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalModuleCustomerOverriddenProducesXml() throws Exception {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("Overridden Module");
		module.setName("overriddenmodule");
		String xml = XMLMetaData.marshalModule(module, true);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("Overridden Module"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalModuleProducesNameInXml() throws Exception {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("MyModule");
		module.setName("mymodule");
		String xml = XMLMetaData.marshalModule(module, false);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("mymodule"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalViewRoundtrips() throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		String xml = XMLMetaData.marshalView(view, false, false);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("edit"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalViewCustomerOverriddenProducesXml() throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("list");
		String xml = XMLMetaData.marshalView(view, true, false);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("list"), is(true));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalViewUxUiOverriddenProducesXml() throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		String xml = XMLMetaData.marshalView(view, false, true);
		assertThat(xml, is(notNullValue()));
		assertThat(xml.contains("view"), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalCustomerStringRoundTrip() throws Exception {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("acme");
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		customer.setDefaultTimeConverter(ConverterName.HH24_MI);
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH24_MI);
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH24_MI_SS);
		org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData modules = new org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData();
		modules.setHomeModule("admin");
		org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData mod = new org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData();
		mod.setName("admin");
		modules.getModules().add(mod);
		customer.setModules(modules);
		String xml = XMLMetaData.marshalCustomer(customer);
		CustomerMetaData roundTripped = XMLMetaData.unmarshalCustomerString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("acme"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalModuleStringRoundTrip() throws Exception {
		ModuleMetaData module = createModule();
		module.setHomeDocument("Dashboard");
		ModuleDocumentMetaData docRef = new ModuleDocumentMetaData();
		docRef.setRef("Dashboard");
		module.getDocuments().add(docRef);
		org.skyve.impl.metadata.repository.module.ModuleRoleMetaData role = new org.skyve.impl.metadata.repository.module.ModuleRoleMetaData();
		role.setName("Administrator");
		role.setDescription("Admin role");
		module.getRoles().add(role);
		module.setMenu(new org.skyve.impl.metadata.repository.module.MenuMetaData());
		String xml = XMLMetaData.marshalModule(module, false);
		ModuleMetaData roundTripped = XMLMetaData.unmarshalModuleString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalViewStringRoundTrip() throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		view.setTitle("Edit");
		String xml = XMLMetaData.marshalView(view, false, false);
		ViewMetaData roundTripped = XMLMetaData.unmarshalViewString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("edit"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMarshalSailRoundTrip() throws Exception {
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);
		org.skyve.metadata.sail.language.Interaction interaction = new org.skyve.metadata.sail.language.Interaction();
		interaction.setName("test");
		automation.getInteractions().add(interaction);
		String xml = XMLMetaData.marshalSAIL(automation);
		assertThat(xml, is(notNullValue()));
		Automation roundTripped = XMLMetaData.unmarshalSAILString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getUxui(), is("desktop"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalCustomerFile(@TempDir Path tempDir) throws Exception {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("acme");
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		customer.setDefaultTimeConverter(ConverterName.HH24_MI);
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH24_MI);
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH24_MI_SS);
		org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData modules = new org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData();
		modules.setHomeModule("admin");
		org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData mod = new org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData();
		mod.setName("admin");
		modules.getModules().add(mod);
		customer.setModules(modules);
		XMLMetaData.marshalCustomer(customer, tempDir.toString());
		File expected = tempDir.resolve("customers/acme/acme.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalCustomerFile(@TempDir Path tempDir) throws Exception {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("acme");
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		customer.setDefaultTimeConverter(ConverterName.HH24_MI);
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH24_MI);
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH24_MI_SS);
		org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData modules = new org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData();
		modules.setHomeModule("admin");
		org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData mod = new org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData();
		mod.setName("admin");
		modules.getModules().add(mod);
		customer.setModules(modules);
		XMLMetaData.marshalCustomer(customer, tempDir.toString());
		String filePath = tempDir.resolve("customers/acme/acme.xml").toString();
		CustomerMetaData result = XMLMetaData.unmarshalCustomerFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("acme"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalModuleFile(@TempDir Path tempDir) throws Exception {
		ModuleMetaData module = createModule();
		module.setHomeDocument("Dashboard");
		ModuleDocumentMetaData docRef = new ModuleDocumentMetaData();
		docRef.setRef("Dashboard");
		module.getDocuments().add(docRef);
		org.skyve.impl.metadata.repository.module.ModuleRoleMetaData role = new org.skyve.impl.metadata.repository.module.ModuleRoleMetaData();
		role.setName("Administrator");
		role.setDescription("Admin role");
		module.getRoles().add(role);
		module.setMenu(new org.skyve.impl.metadata.repository.module.MenuMetaData());
		XMLMetaData.marshalModule(module, false, tempDir.toString());
		File expected = tempDir.resolve("test/test.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalModuleFile(@TempDir Path tempDir) throws Exception {
		ModuleMetaData module = createModule();
		module.setHomeDocument("Dashboard");
		ModuleDocumentMetaData docRef = new ModuleDocumentMetaData();
		docRef.setRef("Dashboard");
		module.getDocuments().add(docRef);
		org.skyve.impl.metadata.repository.module.ModuleRoleMetaData role = new org.skyve.impl.metadata.repository.module.ModuleRoleMetaData();
		role.setName("Administrator");
		role.setDescription("Admin role");
		module.getRoles().add(role);
		module.setMenu(new org.skyve.impl.metadata.repository.module.MenuMetaData());
		XMLMetaData.marshalModule(module, false, tempDir.toString());
		String filePath = tempDir.resolve("test/test.xml").toString();
		ModuleMetaData result = XMLMetaData.unmarshalModuleFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("test"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalDocumentFile(@TempDir Path tempDir) throws Exception {
		DocumentMetaData document = createDocument();
		XMLMetaData.marshalDocument(document, false, tempDir.toString());
		File expected = tempDir.resolve("TestDocument/TestDocument.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalDocumentFile(@TempDir Path tempDir) throws Exception {
		DocumentMetaData document = createDocument();
		XMLMetaData.marshalDocument(document, false, tempDir.toString());
		String filePath = tempDir.resolve("TestDocument/TestDocument.xml").toString();
		DocumentMetaData result = XMLMetaData.unmarshalDocumentFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("TestDocument"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalBizletFile(@TempDir Path tempDir) throws Exception {
		BizletMetaData bizlet = new BizletMetaData();
		Path docDir = tempDir.resolve("TestDocument");
		XMLMetaData.marshalBizlet(bizlet, false, docDir.toString());
		File expected = docDir.resolve("TestDocumentBizlet.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalBizletFile(@TempDir Path tempDir) throws Exception {
		BizletMetaData bizlet = new BizletMetaData();
		Path docDir = tempDir.resolve("TestDocument");
		XMLMetaData.marshalBizlet(bizlet, false, docDir.toString());
		String filePath = docDir.resolve("TestDocumentBizlet.xml").toString();
		BizletMetaData result = XMLMetaData.unmarshalBizletFile(filePath);
		assertThat(result, is(notNullValue()));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalActionFile(@TempDir Path tempDir) throws Exception {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		XMLMetaData.marshalAction(action, false, tempDir.toString());
		File expected = tempDir.resolve("actions/TestAction.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalActionFile(@TempDir Path tempDir) throws Exception {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		XMLMetaData.marshalAction(action, false, tempDir.toString());
		String filePath = tempDir.resolve("actions/TestAction.xml").toString();
		ActionMetaData result = XMLMetaData.unmarshalActionFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("TestAction"));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	void testMarshalViewFile(@TempDir Path tempDir) throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		XMLMetaData.marshalView(view, false, false, tempDir.toString());
		File expected = tempDir.resolve("views/edit.xml").toFile();
		assertThat(expected.exists(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalViewFile(@TempDir Path tempDir) throws Exception {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		view.setTitle("Edit");
		XMLMetaData.marshalView(view, false, false, tempDir.toString());
		String filePath = tempDir.resolve("views/edit.xml").toString();
		ViewMetaData result = XMLMetaData.unmarshalViewFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("edit"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalRouterFile(@TempDir Path tempDir) throws Exception {
		Router router = new Router();
		String xml = XMLMetaData.marshalRouter(router);
		Path tempFile = tempDir.resolve("router.xml");
		Files.writeString(tempFile, xml);
		Router result = XMLMetaData.unmarshalRouterFile(tempFile.toString());
		assertThat(result, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnmarshalSAILFile(@TempDir Path tempDir) throws Exception {
		Automation automation = new Automation();
		automation.setUxui("desktop");
		automation.setUserAgentType(org.skyve.web.UserAgentType.desktop);
		org.skyve.metadata.sail.language.Interaction interaction = new org.skyve.metadata.sail.language.Interaction();
		interaction.setName("test");
		automation.getInteractions().add(interaction);
		String xml = XMLMetaData.marshalSAIL(automation);
		Path tempFile = tempDir.resolve("test.sail.xml");
		Files.writeString(tempFile, xml);
		Automation result = XMLMetaData.unmarshalSAILFile(tempFile.toString());
		assertThat(result, is(notNullValue()));
		assertThat(result.getUxui(), is("desktop"));
	}

}

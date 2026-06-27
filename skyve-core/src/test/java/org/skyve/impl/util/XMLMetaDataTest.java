package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.function.Executable;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
@SuppressWarnings("static-method")
class XMLMetaDataTest {
	@Test
	void testMarshalDocumentScalarAttribute() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
	}

	@Test
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
		
		assertEquals(0, v.getMin());
		assertEquals(10, v.getMax());

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<validator max=\"10\" min=\"0\" validationMessage=\"Value must be between 0 and 10\"/>"));
	}

	@Test
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

		assertEquals(0L, v.getMin());
		assertEquals(10L, v.getMax());

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<validator max=\"10\" min=\"0\"/>"));
	}

	@Test
	void testMarshalDocumentDateAttributeConverter() {
		// setup the test data
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Date field = createDateAttribute();

		field.setConverterName(ConverterName.DD_MM_YYYY);

		document.getAttributes().add(field);

		document.convert("TestDocument");
		
		assertThat(field.getConverter(), is(notNullValue()));

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<converterName>DD_MM_YYYY</converterName>"));
	}

	@Test
	void testMarshalDocumentTextAttributeFormatCase() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextFormat f = new TextFormat();
		f.setCase(TextCase.capital);

		field.setFormat(f);

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<format case=\"capital\"/>"));
	}

	@Test
	void testMarshalDocumentTextAttributeFormatMask() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextFormat f = new TextFormat();
		f.setMask("AAA");

		field.setFormat(f);

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<format mask=\"AAA\"/>"));
	}

	@Test
	void testMarshalDocumentTextAttributeRegularExpressionValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setRegularExpression("\\d");

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<validator regularExpression=\"\\d\"/>"));
	}

	@Test
	void testMarshalDocumentTextAttributeTypeValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setType(ValidatorType.creditCard);

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<validator regularExpression=\"^\\d{15,16}$\" type=\"creditCard\"/>"));
	}

	@Test
	void testMarshalDocumentTextAttributeValidationMessageValidator() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Text field = createTextAttribute();
		TextValidator v = new TextValidator();
		v.setValidationMessage("This is required");

		field.setValidator(v);

		document.getAttributes().add(field);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("<displayName>Attribute 1</displayName>"));
		assertTrue(result.contains("<validator validationMessage=\"This is required\"/>"));
	}

	@Test
	void testMarshalDocumentAggregatedAssociation() {
		// setup the test data
		DocumentMetaData document = createDocument();
		AssociationImpl association = createAssociation();

		document.getAttributes().add(association);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("type=\"aggregation\""));
		assertTrue(result.contains("<displayName>Association 1</displayName>"));
	}

	@Test
	void testMarshalDocumentComposedAssociation() {
		// setup the test data
		DocumentMetaData document = createDocument();

		AssociationImpl association = createAssociation();
		association.setType(AssociationType.composition);

		document.getAttributes().add(association);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertTrue(result.contains("type=\"composition\""));
		assertTrue(result.contains("<displayName>Association 1</displayName>"));
	}

	@Test
	void testMarshalDocumentChildCollection() {
		// setup the test data
		DocumentMetaData document = new DocumentMetaData();
		document.setName("Test");

		CollectionImpl collection = createCollection();
		collection.setType(CollectionType.child);

		document.getAttributes().add(collection);

		// validate the test data
		assertEquals(1, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"Test\""));
		assertTrue(result.contains("type=\"child\""));
		assertTrue(result.contains("<displayName>Collection 1</displayName>"));
	}

	@Test
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

		assertTrue(result.contains("name=\"Test\""));
		assertTrue(result.contains("<parentDocument>Parent</parentDocument>"));
	}

	@Test
	void testMarshalDocumentRemovesEmptyChildElements() {
		// setup the test data
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);
		document.getAttributes().add(createAssociation());

		// validate the test data
		assertEquals(2, document.getAttributes().size());

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"TestDocument\""));
		assertFalse(result.contains("conditions"), "XML should not contain 'conditions'");
		assertFalse(result.contains("implements"), "XML should not contain 'implements'");
		assertFalse(result.contains("uniqueConstraints"), "XML should not contain 'uniqueConstraints'");
	}

	@Test
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

		assertTrue(result.contains("name=\"Test\""));

		// transient element should have been removed as it is the default value of false
		assertFalse(result.contains("<transient>"));

		Pattern p1 = Pattern.compile("<boolean.*persistent");
		Matcher m1 = p1.matcher(result);

		Pattern p2 = Pattern.compile("<boolean.*deprecated");
		Matcher m2 = p2.matcher(result);

		Pattern p3 = Pattern.compile("<boolean.*required");
		Matcher m3 = p3.matcher(result);

		assertFalse(m1.find(), "The persistent attribute should not be present");
		assertFalse(m2.find(), "The deprecated attribute should not be present");
		assertFalse(m3.find(), "The required attribute should not be present");
	}

	@Test
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

		assertTrue(result.contains("name=\"Test\""));

		// transient element should not have been removed as it is not the default value of false
		assertTrue(result.contains("<transient>true</transient>"));

		Pattern p1 = Pattern.compile("<boolean.*persistent");
		Matcher m1 = p1.matcher(result);

		Pattern p2 = Pattern.compile("<boolean.*deprecated");
		Matcher m2 = p2.matcher(result);

		Pattern p3 = Pattern.compile("<boolean.*required");
		Matcher m3 = p3.matcher(result);

		assertTrue(m1.find(), "The persistent attribute should be present");
		assertTrue(m2.find(), "The deprecated attribute should be present");
		assertTrue(m3.find(), "The required attribute should be present");
	}

	@Test
	void testMarshalViewListGrid() {
		// setup the test data
		FluentView v = new FluentView().title("Test").name(ViewType.edit.toString());
		v.addListGrid(new FluentListGrid().queryName("qQuery"));
		ViewMetaData view = v.get();

		// call the method under test
		String result = XMLMetaData.marshalView(view, false, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"edit\""));
		assertFalse(result.contains("onEditedHandlers"));
		assertFalse(result.contains("onDeletedHandlers"));
		assertFalse(result.contains("onSelectedHandlers"));
		assertFalse(result.contains("newParameters"));
	}

	@Test
	void testMarshalModuleRemovesEmptyChildElements() {
		// setup the test data
		ModuleMetaData module = createModule();
		DocumentMetaData document = createDocument();

		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		md.setRef(document.getName());
		module.getDocuments().add(md);

		// validate the test data
		assertEquals(1, module.getDocuments().size());

		// call the method under test
		String result = XMLMetaData.marshalModule(module, false);

		// verify the result
		assertThat(result, is(notNullValue()));

		assertTrue(result.contains("name=\"test\""));
		assertTrue(result.contains("<documents"), "XML should contain 'documents'");
		assertTrue(result.contains("<roles"), "XML should contain 'roles'");

		assertFalse(result.contains("<jobs"), "XML should not contain 'jobs'");
		assertFalse(result.contains("<queries"), "XML should not contain 'queries'");
	}

	@Test
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

		assertTrue(result.contains("name=\"test\""));
		assertFalse(result.contains("<textSearchRoles"), "XML should not contain 'textSearchRoles'");
		assertFalse(result.contains("<flagRoles"), "XML should not contain 'flagRoles'");
		assertFalse(result.contains("<switchModeRoles"), "XML should not contain 'switchModeRoles'");
		assertFalse(result.contains("<interceptors"), "XML should not contain 'interceptors'");
		assertFalse(result.contains("<observers"), "XML should not contain 'observers'");
	}

	@Test
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
		assertTrue(result.contains("<generated>insert</generated>"));
	}

	@Test
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
		assertTrue(result.contains("<generated>always</generated>"));
	}

	@Test
	void testMarshalDocumentFieldWithoutGeneratedOmitsElement() {
		// setup the test data - no generated set
		DocumentMetaData document = createDocument();
		Field field = createAttribute();

		document.getAttributes().add(field);

		// call the method under test
		String result = XMLMetaData.marshalDocument(document, false);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertFalse(result.contains("<generated>"), "generated element should be absent when not set");
	}

	@Test
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
		assertEquals(1, roundTripped.getAttributes().size());
		org.skyve.impl.metadata.model.document.field.Integer roundTrippedField =
				(org.skyve.impl.metadata.model.document.field.Integer) roundTripped.getAttributes().get(0);
		assertThat(roundTrippedField.getGenerated(), is(GeneratedType.insert));
	}

	@Test
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
		assertEquals(1, roundTripped.getAttributes().size());
		org.skyve.impl.metadata.model.document.field.Integer roundTrippedField =
				(org.skyve.impl.metadata.model.document.field.Integer) roundTripped.getAttributes().get(0);
		assertThat(roundTrippedField.getGenerated(), is(GeneratedType.always));
	}

	@Test
	void testConvertThrowsWhenGeneratedSetOnNonPersistentDocument() {
		// setup the test data - document has no <persistent> element (transient document)
		DocumentMetaData document = createDocument();
		org.skyve.impl.metadata.model.document.field.Integer field = createIntegerAttribute();
		field.setGenerated(GeneratedType.insert);
		document.getAttributes().add(field);

		// call the method under test and verify it throws
		Executable convert = () -> document.convert("TestDocument");
		MetaDataException ex = assertThrows(MetaDataException.class, convert);
		assertTrue(ex.getMessage().contains("[generated]"));
		assertTrue(ex.getMessage().contains("non-persistent"));
		assertTrue(ex.getMessage().contains(field.getName()));
	}

	@Test
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
		assertTrue(ex.getMessage().contains("[generated]"));
		assertTrue(ex.getMessage().contains("non-persistent"));
		assertTrue(ex.getMessage().contains(field.getName()));
	}

	@Test
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
	void testMarshalRouterRoundtrips() {
		Router router = new Router();
		router.setUxuiSelectorClassName("org.skyve.impl.metadata.repository.router.TaggingUxUiSelector");
		String xml = XMLMetaData.marshalRouter(router);
		assertThat(xml, is(notNullValue()));
		Router roundTripped = XMLMetaData.unmarshalRouterString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getUxuiSelectorClassName(), is("org.skyve.impl.metadata.repository.router.TaggingUxUiSelector"));
	}

	@Test
	void testMarshalRouterProducesXml() {
		Router router = new Router();
		String xml = XMLMetaData.marshalRouter(router);
		assertThat(xml, is(notNullValue()));
		assertThat(Boolean.valueOf(xml.contains("router")), is(Boolean.TRUE));
	}

	@Test
	void testUnmarshalRouterStringReturnsRouter() {
		String xml = XMLMetaData.marshalRouter(new Router());
		Router result = XMLMetaData.unmarshalRouterString(xml);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void testMarshalBizletRoundtrips() {
		BizletMetaData bizlet = new BizletMetaData();
		bizlet.setDocumentation("Test bizlet");
		String xml = XMLMetaData.marshalBizlet(bizlet, false);
		assertThat(xml, is(notNullValue()));
		BizletMetaData roundTripped = XMLMetaData.unmarshalBizletString(xml);
		assertThat(roundTripped, is(notNullValue()));
	}

	@Test
	void testMarshalBizletCustomerOverriddenRoundtrips() {
		BizletMetaData bizlet = new BizletMetaData();
		String xml = XMLMetaData.marshalBizlet(bizlet, true);
		assertThat(xml, is(notNullValue()));
		BizletMetaData roundTripped = XMLMetaData.unmarshalBizletString(xml);
		assertThat(roundTripped, is(notNullValue()));
	}

	@Test
	void testMarshalActionRoundtrips() {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		String xml = XMLMetaData.marshalAction(action, false);
		assertThat(xml, is(notNullValue()));
		ActionMetaData roundTripped = XMLMetaData.unmarshalActionString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("TestAction"));
	}

	@Test
	void testMarshalActionCustomerOverriddenRoundtrips() {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		String xml = XMLMetaData.marshalAction(action, true);
		assertThat(xml, is(notNullValue()));
		ActionMetaData roundTripped = XMLMetaData.unmarshalActionString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("TestAction"));
	}

	@Test
	void testMarshalCustomerRoundtrips() {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("testcustomer");
		String xml = XMLMetaData.marshalCustomer(customer);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("testcustomer"));
	}

	@Test
	void testMarshalCustomerProducesXml() {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("acme");
		String xml = XMLMetaData.marshalCustomer(customer);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("acme"));
	}

	@Test
	void testMarshalModuleRoundtrips() {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("Test Module");
		module.setName("testmodule");
		String xml = XMLMetaData.marshalModule(module, false);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("Test Module"));
	}

	@Test
	void testMarshalModuleCustomerOverriddenProducesXml() {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("Overridden Module");
		module.setName("overriddenmodule");
		String xml = XMLMetaData.marshalModule(module, true);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("Overridden Module"));
	}

	@Test
	void testMarshalModuleProducesNameInXml() {
		ModuleMetaData module = new ModuleMetaData();
		module.setTitle("MyModule");
		module.setName("mymodule");
		String xml = XMLMetaData.marshalModule(module, false);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("mymodule"));
	}

	@Test
	void testMarshalViewRoundtrips() {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		String xml = XMLMetaData.marshalView(view, false, false);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("edit"));
	}

	@Test
	void testMarshalViewCustomerOverriddenProducesXml() {
		ViewMetaData view = new ViewMetaData();
		view.setName("list");
		String xml = XMLMetaData.marshalView(view, true, false);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("list"));
	}

	@Test
	void testMarshalViewUxUiOverriddenProducesXml() {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		String xml = XMLMetaData.marshalView(view, false, true);
		assertThat(xml, is(notNullValue()));
		assertTrue(xml.contains("view"));
	}

	@Test
	void testUnmarshalCustomerStringRoundTrip() {
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
	void testUnmarshalModuleStringRoundTrip() {
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
	void testUnmarshalViewStringRoundTrip() {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		view.setTitle("Edit");
		String xml = XMLMetaData.marshalView(view, false, false);
		ViewMetaData roundTripped = XMLMetaData.unmarshalViewString(xml);
		assertThat(roundTripped, is(notNullValue()));
		assertThat(roundTripped.getName(), is("edit"));
	}

	@Test
	void testMarshalSailRoundTrip() {
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
	void testMarshalCustomerFile(@TempDir Path tempDir) {
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
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalCustomerFile(@TempDir Path tempDir) {
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
	void testMarshalModuleFile(@TempDir Path tempDir) {
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
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalModuleFile(@TempDir Path tempDir) {
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
	void testMarshalDocumentFile(@TempDir Path tempDir) {
		DocumentMetaData document = createDocument();
		XMLMetaData.marshalDocument(document, false, tempDir.toString());
		File expected = tempDir.resolve("TestDocument/TestDocument.xml").toFile();
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalDocumentFile(@TempDir Path tempDir) {
		DocumentMetaData document = createDocument();
		XMLMetaData.marshalDocument(document, false, tempDir.toString());
		String filePath = tempDir.resolve("TestDocument/TestDocument.xml").toString();
		DocumentMetaData result = XMLMetaData.unmarshalDocumentFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("TestDocument"));
	}

	@Test
	void testMarshalBizletFile(@TempDir Path tempDir) {
		BizletMetaData bizlet = new BizletMetaData();
		Path docDir = tempDir.resolve("TestDocument");
		XMLMetaData.marshalBizlet(bizlet, false, docDir.toString());
		File expected = docDir.resolve("TestDocumentBizlet.xml").toFile();
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalBizletFile(@TempDir Path tempDir) {
		BizletMetaData bizlet = new BizletMetaData();
		Path docDir = tempDir.resolve("TestDocument");
		XMLMetaData.marshalBizlet(bizlet, false, docDir.toString());
		String filePath = docDir.resolve("TestDocumentBizlet.xml").toString();
		BizletMetaData result = XMLMetaData.unmarshalBizletFile(filePath);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void testMarshalActionFile(@TempDir Path tempDir) {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		XMLMetaData.marshalAction(action, false, tempDir.toString());
		File expected = tempDir.resolve("actions/TestAction.xml").toFile();
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalActionFile(@TempDir Path tempDir) {
		ActionMetaData action = new ActionMetaData();
		action.setName("TestAction");
		XMLMetaData.marshalAction(action, false, tempDir.toString());
		String filePath = tempDir.resolve("actions/TestAction.xml").toString();
		ActionMetaData result = XMLMetaData.unmarshalActionFile(filePath);
		assertThat(result, is(notNullValue()));
		assertThat(result.getName(), is("TestAction"));
	}

	@Test
	void testMarshalViewFile(@TempDir Path tempDir) {
		ViewMetaData view = new ViewMetaData();
		view.setName("edit");
		XMLMetaData.marshalView(view, false, false, tempDir.toString());
		File expected = tempDir.resolve("views/edit.xml").toFile();
		assertTrue(expected.exists());
	}

	@Test
	void testUnmarshalViewFile(@TempDir Path tempDir) {
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
	void testUnmarshalRouterFile(@TempDir Path tempDir) throws IOException {
		Router router = new Router();
		String xml = XMLMetaData.marshalRouter(router);
		Path tempFile = tempDir.resolve("router.xml");
		Files.writeString(tempFile, xml);
		Router result = XMLMetaData.unmarshalRouterFile(tempFile.toString());
		assertThat(result, is(notNullValue()));
	}

	@Test
	void testUnmarshalSAILFile(@TempDir Path tempDir) throws IOException {
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

	// ---- Exception path coverage for unmarshalXxxString methods ----

	@Test
	void unmarshalRouterStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalRouterString("not valid xml"));
	}

	@Test
	void unmarshalCustomerStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalCustomerString("not valid xml"));
	}

	@Test
	void unmarshalModuleStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalModuleString("not valid xml"));
	}

	@Test
	void unmarshalDocumentStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalDocumentString("not valid xml"));
	}

	@Test
	void unmarshalBizletStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalBizletString("not valid xml"));
	}

	@Test
	void unmarshalActionStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalActionString("not valid xml"));
	}

	@Test
	void unmarshalViewStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalViewString("not valid xml"));
	}

	@Test
	void unmarshalSAILStringThrowsOnInvalidXml() {
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> XMLMetaData.unmarshalSAILString("not valid xml"));
	}

	// ---- File unmarshal exception-path tests (covers catch blocks) ----

	@Test
	void unmarshalRouterFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalRouterFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalCustomerFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalCustomerFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalModuleFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalModuleFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalDocumentFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalDocumentFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalBizletFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalBizletFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalActionFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalActionFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalViewFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalViewFile("/nonexistent/no_such_file_12345.xml"));
	}

	@Test
	void unmarshalSAILFileThrowsMetaDataExceptionForNonexistentFile() {
		assertThrows(MetaDataException.class,
				() -> XMLMetaData.unmarshalSAILFile("/nonexistent/no_such_file_12345.xml"));
	}

}

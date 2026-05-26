package org.skyve.impl.metadata.repository.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.InterfaceImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;

class DocumentMetaDataTest {

	private DocumentMetaData doc;

	@BeforeEach
	void setUp() {
		doc = new DocumentMetaData();
	}

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(doc);
	}

	@Test
	void nameRoundTrip() {
		doc.setName("Contact");
		assertEquals("Contact", doc.getName());
	}

	@Test
	void singularAliasRoundTrip() {
		doc.setSingularAlias("Contact");
		assertEquals("Contact", doc.getSingularAlias());
	}

	@Test
	void singularAliasBlankBecomesNull() {
		doc.setSingularAlias("  ");
		assertNull(doc.getSingularAlias());
	}

	@Test
	void pluralAliasRoundTrip() {
		doc.setPluralAlias("Contacts");
		assertEquals("Contacts", doc.getPluralAlias());
	}

	@Test
	void pluralAliasBlankBecomesNull() {
		doc.setPluralAlias("  ");
		assertNull(doc.getPluralAlias());
	}

	@Test
	void descriptionRoundTrip() {
		doc.setDescription("A contact record");
		assertEquals("A contact record", doc.getDescription());
	}

	@Test
	void descriptionBlankBecomesNull() {
		doc.setDescription("   ");
		assertNull(doc.getDescription());
	}

	@Test
	void documentationRoundTrip() {
		doc.setDocumentation("Contact documentation");
		assertEquals("Contact documentation", doc.getDocumentation());
	}

	@Test
	void iconStyleClassRoundTrip() {
		doc.setIconStyleClass("fa fa-user");
		assertEquals("fa fa-user", doc.getIconStyleClass());
	}

	@Test
	void icon16x16RoundTrip() {
		doc.setIcon16x16RelativeFilePath("icons/contact16.png");
		assertEquals("icons/contact16.png", doc.getIcon16x16RelativeFilePath());
	}

	@Test
	void icon32x32RoundTrip() {
		doc.setIcon32x32RelativeFilePath("icons/contact32.png");
		assertEquals("icons/contact32.png", doc.getIcon32x32RelativeFilePath());
	}

	@Test
	void auditedRoundTrip() {
		doc.setAudited(Boolean.TRUE);
		assertEquals(Boolean.TRUE, doc.getAudited());
	}

	@Test
	void auditedNullByDefault() {
		assertNull(doc.getAudited());
	}

	@Test
	void abstractNullByDefault() {
		assertNull(doc.getAbstract());
	}

	@Test
	void abstractRoundTrip() {
		doc.setAbstract(Boolean.TRUE);
		assertEquals(Boolean.TRUE, doc.getAbstract());
	}

	@Test
	void extendsRoundTrip() {
		Extends ext = new Extends();
		ext.setDocumentName("BaseDocument");
		doc.setExtends(ext);
		assertEquals(ext, doc.getExtends());
	}

	@Test
	void extendsNullByDefault() {
		assertNull(doc.getExtends());
	}

	@Test
	void persistentNullByDefault() {
		assertNull(doc.getPersistent());
	}

	@Test
	void persistentRoundTrip() {
		Persistent persistent = new Persistent();
		persistent.setName("Contact");
		doc.setPersistent(persistent);
		assertEquals(persistent, doc.getPersistent());
	}

	@Test
	void dynamicNullByDefault() {
		assertNull(doc.getDynamic());
	}

	@Test
	void dynamicRoundTrip() {
		Dynamic dynamic = new Dynamic();
		doc.setDynamic(dynamic);
		assertEquals(dynamic, doc.getDynamic());
	}

	@Test
	void parentDocumentNullByDefault() {
		assertNull(doc.getParentDocument());
	}

	@Test
	void parentDocumentRoundTrip() {
		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("Organisation");
		doc.setParentDocument(parent);
		assertEquals(parent, doc.getParentDocument());
	}

	@Test
	void bizKeyNullByDefault() {
		assertNull(doc.getBizKey());
	}

	@Test
	void bizKeyRoundTrip() {
		BizKey bizKey = new BizKey();
		bizKey.setExpression("{name}");
		doc.setBizKey(bizKey);
		assertEquals(bizKey, doc.getBizKey());
	}

	@Test
	void implementsListNotNull() {
		assertNotNull(doc.getImplements());
	}

	@Test
	void attributesListNotNull() {
		assertNotNull(doc.getAttributes());
		assertTrue(doc.getAttributes().isEmpty());
	}

	@Test
	void conditionsListNotNull() {
		assertNotNull(doc.getConditions());
		assertTrue(doc.getConditions().isEmpty());
	}

	@Test
	void uniqueConstraintsListNotNull() {
		assertNotNull(doc.getUniqueConstraints());
		assertTrue(doc.getUniqueConstraints().isEmpty());
	}

	@Test
	void propertiesMapNotNull() {
		assertNotNull(doc.getProperties());
	}

	@Test
	void lastModifiedMillisDefaultIsMaxValue() {
		assertEquals(Long.MAX_VALUE, doc.getLastModifiedMillis());
	}

	@Test
	void lastModifiedMillisRoundTrip() {
		doc.setLastModifiedMillis(12345L);
		assertEquals(12345L, doc.getLastModifiedMillis());
	}

	// ── convert() ──────────────────────────────────────────────────────────

	private static DocumentMetaData minimalTransientDoc() {
		DocumentMetaData d = new DocumentMetaData();
		d.setName("TestDoc");
		d.setSingularAlias("Test Doc");
		d.setPluralAlias("Test Docs");
		return d;
	}

	@Test
	void convertThrowsWhenNameIsNull() {
		doc.setSingularAlias("Test");
		doc.setPluralAlias("Tests");
		assertThrows(MetaDataException.class, () -> doc.convert("test"));
	}

	@Test
	void convertThrowsWhenSingularAliasIsNull() {
		doc.setName("TestDoc");
		doc.setPluralAlias("Tests");
		assertThrows(MetaDataException.class, () -> doc.convert("test"));
	}

	@Test
	void convertThrowsWhenPluralAliasIsNull() {
		doc.setName("TestDoc");
		doc.setSingularAlias("Test");
		assertThrows(MetaDataException.class, () -> doc.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenPersistentAndBizKeyIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		d.setPersistent(new Persistent());
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenPersistentAndBizKeyHasNoCodeOrExpression() {
		DocumentMetaData d = minimalTransientDoc();
		d.setPersistent(new Persistent());
		d.setBizKey(new BizKey());
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenParentDatabaseIndexTrueAndTransient() {
		DocumentMetaData d = minimalTransientDoc();
		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("Parent");
		parent.setDatabaseIndex(Boolean.TRUE);
		d.setParentDocument(parent);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenInterfaceNameIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		d.getImplements().add(new InterfaceImpl());
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDynamicActionKeyIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("", "com.example.Action");
		d.setDynamic(dynamic);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDynamicActionValueIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("myAction", null);
		d.setDynamic(dynamic);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDynamicImageKeyIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Dynamic dynamic = new Dynamic();
		dynamic.getImages().put("", "com.example.Image");
		d.setDynamic(dynamic);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDynamicModelKeyIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Dynamic dynamic = new Dynamic();
		dynamic.getModels().put("", "com.example.Model");
		d.setDynamic(dynamic);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsForMinimalTransientDocument() {
		DocumentMetaData d = minimalTransientDoc();
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsDocumentNameAndAliases() {
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.document.Document result = d.convert("test");
		assertEquals("TestDoc", result.getName());
		assertEquals("Test Doc", result.getSingularAlias());
		assertEquals("Test Docs", result.getPluralAlias());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsDefaultIconWhenNoIconsProvided() {
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.document.Document result = d.convert("test");
		assertNotNull(result.getIconStyleClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWhenIcon16IsProvided() {
		DocumentMetaData d = minimalTransientDoc();
		d.setIcon16x16RelativeFilePath("icons/doc16.png");
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWhenBizKeyExpressionIsSet() {
		DocumentMetaData d = minimalTransientDoc();
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWhenBizKeyCodeIsSet() {
		DocumentMetaData d = minimalTransientDoc();
		BizKey bk = new BizKey();
		bk.setCode("return \"test\";");
		d.setBizKey(bk);
		assertNotNull(d.convert("test"));
	}

	// ── condition convert() branches ─────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenConditionNameIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		ConditionMetaData condition = new ConditionMetaData();
		condition.setExpression("bean.active");
		// name is null
		d.getConditions().add(condition);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenConditionNameStartsWithIs() {
		DocumentMetaData d = minimalTransientDoc();
		ConditionMetaData condition = new ConditionMetaData();
		condition.setName("isActive");
		condition.setExpression("bean.active");
		d.getConditions().add(condition);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenConditionNameStartsWithNot() {
		DocumentMetaData d = minimalTransientDoc();
		ConditionMetaData condition = new ConditionMetaData();
		condition.setName("notActive");
		condition.setExpression("bean.active");
		d.getConditions().add(condition);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenConditionExpressionIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		ConditionMetaData condition = new ConditionMetaData();
		condition.setName("active");
		// expression is null
		d.getConditions().add(condition);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithValidCondition() {
		DocumentMetaData d = minimalTransientDoc();
		ConditionMetaData condition = new ConditionMetaData();
		condition.setName("active");
		condition.setExpression("bean.enabled");
		d.getConditions().add(condition);
		assertNotNull(d.convert("test"));
	}

	// ── unique constraint convert() branches ─────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenUniqueConstraintOnTransientDocument() {
		DocumentMetaData d = minimalTransientDoc();
		UniqueConstraint constraint = new UniqueConstraint();
		constraint.setName("uq1");
		constraint.setMessage("Must be unique");
		d.getUniqueConstraints().add(constraint);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenUniqueConstraintNameIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDoc");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		UniqueConstraint constraint = new UniqueConstraint();
		// name is null
		constraint.setMessage("Must be unique");
		d.getUniqueConstraints().add(constraint);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenUniqueConstraintMessageIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDoc");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		UniqueConstraint constraint = new UniqueConstraint();
		constraint.setName("uq1");
		// message is null
		d.getUniqueConstraints().add(constraint);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenUniqueConstraintFieldRefIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDoc");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		UniqueConstraint constraint = new UniqueConstraint();
		constraint.setName("uq1");
		constraint.setMessage("Must be unique");
		FieldReference ref = new FieldReference();
		// ref name is null
		constraint.getFieldReferences().add(ref);
		d.getUniqueConstraints().add(constraint);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsDuplicateUniqueConstraintName() {
		DocumentMetaData d = minimalTransientDoc();
		Persistent persistent = new Persistent();
		persistent.setName("TST_TestDoc");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		UniqueConstraint constraint1 = new UniqueConstraint();
		constraint1.setName("uq1");
		constraint1.setMessage("Must be unique");
		UniqueConstraint constraint2 = new UniqueConstraint();
		constraint2.setName("uq1"); // duplicate
		constraint2.setMessage("Also must be unique");
		d.getUniqueConstraints().add(constraint1);
		d.getUniqueConstraints().add(constraint2);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithInterface() {
		DocumentMetaData d = minimalTransientDoc();
		InterfaceImpl iface = new InterfaceImpl();
		iface.setInterfaceName("java.io.Serializable");
		d.getImplements().add(iface);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenAttributeNameIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		Text text = new Text();
		text.setDisplayName("Test Field");
		text.setLength(200);
		// name is null - should throw
		d.getAttributes().add(text);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDuplicateAttributeName() {
		DocumentMetaData d = minimalTransientDoc();
		Text text1 = new Text();
		text1.setName("myField");
		text1.setDisplayName("My Field");
		text1.setLength(200);
		Text text2 = new Text();
		text2.setName("myField"); // duplicate
		text2.setDisplayName("My Field 2");
		text2.setLength(200);
		d.getAttributes().add(text1);
		d.getAttributes().add(text2);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenTextValidatorHasNoTypeAndNoRegex() {
		DocumentMetaData d = minimalTransientDoc();
		Text text = new Text();
		text.setName("myField");
		text.setDisplayName("My Field");
		text.setLength(200);
		TextValidator validator = new TextValidator();
		// neither type nor regularExpression set - should throw
		text.setValidator(validator);
		d.getAttributes().add(text);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithTextFieldAndValidatorType() {
		DocumentMetaData d = minimalTransientDoc();
		Text text = new Text();
		text.setName("myField");
		text.setDisplayName("My Field");
		text.setLength(200);
		TextValidator validator = new TextValidator();
		validator.setType(TextValidator.ValidatorType.email);
		text.setValidator(validator);
		d.getAttributes().add(text);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithTextFieldAndValidatorRegex() {
		DocumentMetaData d = minimalTransientDoc();
		Text text = new Text();
		text.setName("myField");
		text.setDisplayName("My Field");
		text.setLength(200);
		TextValidator validator = new TextValidator();
		validator.setRegularExpression("[A-Z]+");
		text.setValidator(validator);
		d.getAttributes().add(text);
		assertNotNull(d.convert("test"));
	}

	// ---- Date / DateTime / Time / Timestamp fields ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDateField() {
		DocumentMetaData d = minimalTransientDoc();
		Date date = new Date();
		date.setName("myDate");
		date.setDisplayName("My Date");
		d.getAttributes().add(date);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDateTimeField() {
		DocumentMetaData d = minimalTransientDoc();
		DateTime dt = new DateTime();
		dt.setName("myDateTime");
		dt.setDisplayName("My DateTime");
		d.getAttributes().add(dt);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithTimeField() {
		DocumentMetaData d = minimalTransientDoc();
		Time time = new Time();
		time.setName("myTime");
		time.setDisplayName("My Time");
		d.getAttributes().add(time);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithTimestampField() {
		DocumentMetaData d = minimalTransientDoc();
		Timestamp ts = new Timestamp();
		ts.setName("myTimestamp");
		ts.setDisplayName("My Timestamp");
		d.getAttributes().add(ts);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDateValidatorHasNoMinOrMax() {
		DocumentMetaData d = minimalTransientDoc();
		Date date = new Date();
		date.setName("myDate");
		date.setDisplayName("My Date");
		DateValidator validator = new DateValidator();
		// neither min nor max — must throw
		date.setValidator(validator);
		d.getAttributes().add(date);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDateValidatorMin() {
		DocumentMetaData d = minimalTransientDoc();
		Date date = new Date();
		date.setName("myDate");
		date.setDisplayName("My Date");
		DateValidator validator = new DateValidator();
		validator.setXmlMin("2000-01-01");
		date.setValidator(validator);
		d.getAttributes().add(date);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDateValidatorMax() {
		DocumentMetaData d = minimalTransientDoc();
		Date date = new Date();
		date.setName("myDate");
		date.setDisplayName("My Date");
		DateValidator validator = new DateValidator();
		validator.setXmlMax("2099-12-31");
		date.setValidator(validator);
		d.getAttributes().add(date);
		assertNotNull(d.convert("test"));
	}

	// ---- Integer / LongInteger fields ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithIntegerField() {
		DocumentMetaData d = minimalTransientDoc();
		Integer intField = new Integer();
		intField.setName("myInt");
		intField.setDisplayName("My Int");
		d.getAttributes().add(intField);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenIntegerValidatorHasNoMinOrMax() {
		DocumentMetaData d = minimalTransientDoc();
		Integer intField = new Integer();
		intField.setName("myInt");
		intField.setDisplayName("My Int");
		IntegerValidator validator = new IntegerValidator();
		intField.setValidator(validator);
		d.getAttributes().add(intField);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithIntegerValidatorMinMax() {
		DocumentMetaData d = minimalTransientDoc();
		Integer intField = new Integer();
		intField.setName("myInt");
		intField.setDisplayName("My Int");
		IntegerValidator validator = new IntegerValidator();
		validator.setXmlMin("1");
		validator.setXmlMax("100");
		intField.setValidator(validator);
		d.getAttributes().add(intField);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithLongIntegerField() {
		DocumentMetaData d = minimalTransientDoc();
		LongInteger longField = new LongInteger();
		longField.setName("myLong");
		longField.setDisplayName("My Long");
		d.getAttributes().add(longField);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenLongValidatorHasNoMinOrMax() {
		DocumentMetaData d = minimalTransientDoc();
		LongInteger longField = new LongInteger();
		longField.setName("myLong");
		longField.setDisplayName("My Long");
		LongValidator validator = new LongValidator();
		longField.setValidator(validator);
		d.getAttributes().add(longField);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithLongValidatorMin() {
		DocumentMetaData d = minimalTransientDoc();
		LongInteger longField = new LongInteger();
		longField.setName("myLong");
		longField.setDisplayName("My Long");
		LongValidator validator = new LongValidator();
		validator.setXmlMin("1");
		longField.setValidator(validator);
		d.getAttributes().add(longField);
		assertNotNull(d.convert("test"));
	}

	// ---- Decimal2 / Decimal5 / Decimal10 fields ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDecimal2Field() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal2 dec = new Decimal2();
		dec.setName("myDec2");
		dec.setDisplayName("My Dec2");
		d.getAttributes().add(dec);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDecimal2ValidatorPrecisionExceedsTwo() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal2 dec = new Decimal2();
		dec.setName("myDec2");
		dec.setDisplayName("My Dec2");
		DecimalValidator validator = new DecimalValidator();
		validator.setXmlMin("1.0");
		validator.setPrecision(java.lang.Integer.valueOf(3));
		dec.setValidator(validator);
		d.getAttributes().add(dec);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDecimal5Field() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal5 dec = new Decimal5();
		dec.setName("myDec5");
		dec.setDisplayName("My Dec5");
		d.getAttributes().add(dec);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDecimal5ValidatorPrecisionExceedsFive() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal5 dec = new Decimal5();
		dec.setName("myDec5");
		dec.setDisplayName("My Dec5");
		DecimalValidator validator = new DecimalValidator();
		validator.setXmlMin("1.0");
		validator.setPrecision(java.lang.Integer.valueOf(6));
		dec.setValidator(validator);
		d.getAttributes().add(dec);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDecimal10Field() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal10 dec = new Decimal10();
		dec.setName("myDec10");
		dec.setDisplayName("My Dec10");
		d.getAttributes().add(dec);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDecimal10ValidatorPrecisionExceedsTen() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal10 dec = new Decimal10();
		dec.setName("myDec10");
		dec.setDisplayName("My Dec10");
		DecimalValidator validator = new DecimalValidator();
		validator.setXmlMin("1.0");
		validator.setPrecision(java.lang.Integer.valueOf(11));
		dec.setValidator(validator);
		d.getAttributes().add(dec);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenDecimalValidatorHasNoMinOrMax() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal2 dec = new Decimal2();
		dec.setName("myDec2");
		dec.setDisplayName("My Dec2");
		DecimalValidator validator = new DecimalValidator();
		// no min or max set
		dec.setValidator(validator);
		d.getAttributes().add(dec);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithDecimal2ValidatorMax() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal2 dec = new Decimal2();
		dec.setName("myDec2");
		dec.setDisplayName("My Dec2");
		DecimalValidator validator = new DecimalValidator();
		validator.setXmlMax("99.99");
		dec.setValidator(validator);
		d.getAttributes().add(dec);
		assertNotNull(d.convert("test"));
	}

	// ---- Association relation ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithAssociation() {
		DocumentMetaData d = minimalTransientDoc();
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("myAssoc");
		assoc.setDisplayName("My Assoc");
		assoc.setDocumentName("OtherDoc");
		assoc.setType(AssociationType.aggregation);
		d.getAttributes().add(assoc);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenAssociationDocumentNameIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("myAssoc");
		assoc.setDisplayName("My Assoc");
		assoc.setType(AssociationType.aggregation);
		// documentName is null — must throw
		d.getAttributes().add(assoc);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenAssociationTypeIsNull() {
		DocumentMetaData d = minimalTransientDoc();
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("myAssoc");
		assoc.setDisplayName("My Assoc");
		assoc.setDocumentName("OtherDoc");
		// type is null — must throw
		d.getAttributes().add(assoc);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	// ---- Collection relation ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithCollection() {
		DocumentMetaData d = minimalTransientDoc();
		CollectionImpl coll = new CollectionImpl();
		coll.setName("myCollection");
		coll.setDisplayName("My Collection");
		coll.setDocumentName("OtherDoc");
		coll.setType(CollectionType.aggregation);
		d.getAttributes().add(coll);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenCollectionOrderedAndOrderingsBothSet() {
		DocumentMetaData d = minimalTransientDoc();
		CollectionImpl coll = new CollectionImpl();
		coll.setName("myCollection");
		coll.setDisplayName("My Collection");
		coll.setDocumentName("OtherDoc");
		coll.setType(CollectionType.aggregation);
		coll.setOrdered(Boolean.TRUE);
		OrderingImpl ordering = new OrderingImpl();
		ordering.setBy("name");
		coll.getOrdering().add(ordering);
		d.getAttributes().add(coll);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithCollectionComplexOrdering() {
		DocumentMetaData d = minimalTransientDoc();
		CollectionImpl coll = new CollectionImpl();
		coll.setName("myCollection");
		coll.setDisplayName("My Collection");
		coll.setDocumentName("OtherDoc");
		coll.setType(CollectionType.aggregation);
		OrderingImpl ordering = new OrderingImpl();
		ordering.setBy("related.name");
		coll.getOrdering().add(ordering);
		d.getAttributes().add(coll);
		assertNotNull(d.convert("test"));
	}

	// ---- InverseMany relation ----

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithInverseMany() {
		DocumentMetaData d = minimalTransientDoc();
		InverseMany inv = new InverseMany();
		inv.setName("myInverse");
		inv.setDisplayName("My Inverse");
		inv.setDocumentName("OtherDoc");
		d.getAttributes().add(inv);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithInverseManyComplexOrdering() {
		DocumentMetaData d = minimalTransientDoc();
		InverseMany inv = new InverseMany();
		inv.setName("myInverse");
		inv.setDisplayName("My Inverse");
		inv.setDocumentName("OtherDoc");
		OrderingImpl ordering = new OrderingImpl();
		ordering.setBy("related.name");
		inv.getOrdering().add(ordering);
		d.getAttributes().add(inv);
		assertNotNull(d.convert("test"));
	}

	// ---- Enumeration attribute ----

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenEnumerationHasNoValues() {
		DocumentMetaData d = minimalTransientDoc();
		Enumeration enumAttr = new Enumeration();
		enumAttr.setName("myEnum");
		enumAttr.setDisplayName("My Enum");
		// no values defined — must throw
		d.getAttributes().add(enumAttr);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithEnumerationValues() {
		DocumentMetaData d = minimalTransientDoc();
		Enumeration enumAttr = new Enumeration();
		enumAttr.setName("myEnum");
		enumAttr.setDisplayName("My Enum");
		Enumeration.EnumeratedValue v = new Enumeration.EnumeratedValue();
		v.setCode("active");
		enumAttr.getXmlValues().add(v);
		d.getAttributes().add(enumAttr);
		assertNotNull(d.convert("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertThrowsWhenEnumerationReferenceHasNoAttributeRef() {
		DocumentMetaData d = minimalTransientDoc();
		Enumeration enumAttr = new Enumeration();
		enumAttr.setName("myEnum");
		enumAttr.setDisplayName("My Enum");
		enumAttr.setDocumentRef("OtherDoc");
		// attributeRef is null with documentRef set — must throw
		d.getAttributes().add(enumAttr);
		assertThrows(MetaDataException.class, () -> d.convert("test"));
	}

	// -------------------------------------------------------------------------
	// Happy-path convert() tests
	// -------------------------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void convertSucceedsWithMinimalTransientDocument() {
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertEquals("TestDoc", result.getName());
		assertEquals("Test Doc", result.getSingularAlias());
		assertEquals("Test Docs", result.getPluralAlias());
		// audited defaults to true
		assertTrue(result.isAudited());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsDefaultIconStyleClassWhenAllIconFieldsNull() {
		DocumentMetaData d = minimalTransientDoc();
		// all icon fields are null by default → Icons.FONT_DOCUMENT is assigned
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result.getIconStyleClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsIcon16FromIcon32WhenIcon16IsNull() {
		DocumentMetaData d = minimalTransientDoc();
		d.setIcon32x32RelativeFilePath("icons/doc32.png");
		// icon16 is null → it should copy icon32 value
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertEquals("icons/doc32.png", result.getIcon16x16RelativeFileName());
		assertEquals("icons/doc32.png", result.getIcon32x32RelativeFileName());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsIcon32FromIcon16WhenIcon32IsNull() {
		DocumentMetaData d = minimalTransientDoc();
		d.setIcon16x16RelativeFilePath("icons/doc16.png");
		// icon32 is null → it should copy icon16 value
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertEquals("icons/doc16.png", result.getIcon16x16RelativeFileName());
		assertEquals("icons/doc16.png", result.getIcon32x32RelativeFileName());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertPreservesExplicitIconValues() {
		DocumentMetaData d = minimalTransientDoc();
		d.setIcon16x16RelativeFilePath("icons/doc16.png");
		d.setIcon32x32RelativeFilePath("icons/doc32.png");
		d.setIconStyleClass("fa fa-file");
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertEquals("icons/doc16.png", result.getIcon16x16RelativeFileName());
		assertEquals("icons/doc32.png", result.getIcon32x32RelativeFileName());
		assertEquals("fa fa-file", result.getIconStyleClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertAuditedFalseWhenExplicitlySetFalse() {
		DocumentMetaData d = minimalTransientDoc();
		d.setAudited(Boolean.FALSE);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertFalse(result.isAudited());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsDescription() {
		DocumentMetaData d = minimalTransientDoc();
		d.setDescription("A useful document");
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertEquals("A useful document", result.getDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertSetsParentDocumentName() {
		DocumentMetaData d = minimalTransientDoc();
		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("ParentDoc");
		d.setParentDocument(parent);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertEquals("ParentDoc", result.getParentDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertTransientDocumentUsesBizKeyToStringWhenBizKeyNull() {
		// transient doc with no bizKey: default bizKey code should be return toString()
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		// just verify it doesn't throw and the document is valid
		assertNotNull(result.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertPersistentDocumentWithBizKeyExpression() {
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.Persistent persistent = new org.skyve.metadata.model.Persistent();
		persistent.setName("TST_TEST_DOC");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setExpression("{name}");
		d.setBizKey(bk);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertEquals("TestDoc", result.getName());
		// bizKey expression generates code containing the expression
		assertNotNull(result.getBizKeyExpression());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertPersistentDocumentWithBizKeyCode() {
		DocumentMetaData d = minimalTransientDoc();
		org.skyve.metadata.model.Persistent persistent = new org.skyve.metadata.model.Persistent();
		persistent.setName("TST_TEST_DOC");
		d.setPersistent(persistent);
		BizKey bk = new BizKey();
		bk.setCode("return \"static key\";");
		d.setBizKey(bk);
		org.skyve.impl.metadata.model.document.DocumentImpl result =
				(org.skyve.impl.metadata.model.document.DocumentImpl) d.convert("test.TestDoc");
		assertNotNull(result);
		assertEquals("return \"static key\";", result.getBizKeyMethodCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertAddsInterfaceToDocument() {
		DocumentMetaData d = minimalTransientDoc();
		InterfaceImpl iface = new InterfaceImpl();
		iface.setInterfaceName("com.example.MyInterface");
		d.getImplements().add(iface);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertFalse(result.getInterfaces().isEmpty());
		assertEquals("com.example.MyInterface", result.getInterfaces().get(0).getInterfaceName());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertWithValidTextAttributeSucceeds() {
		DocumentMetaData d = minimalTransientDoc();
		Text textAttr = new Text();
		textAttr.setName("myField");
		textAttr.setDisplayName("My Field");
		textAttr.setLength(50);
		d.getAttributes().add(textAttr);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertNotNull(result.getAttribute("myField"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertWithDateAttributeSucceeds() {
		DocumentMetaData d = minimalTransientDoc();
		Date dateAttr = new Date();
		dateAttr.setName("myDate");
		dateAttr.setDisplayName("My Date");
		d.getAttributes().add(dateAttr);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertNotNull(result.getAttribute("myDate"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertWithIntegerAttributeSucceeds() {
		DocumentMetaData d = minimalTransientDoc();
		Integer intAttr = new Integer();
		intAttr.setName("myInt");
		intAttr.setDisplayName("My Integer");
		d.getAttributes().add(intAttr);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertNotNull(result.getAttribute("myInt"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertWithDecimalAttributeSucceeds() {
		DocumentMetaData d = minimalTransientDoc();
		Decimal2 dec2 = new Decimal2();
		dec2.setName("myDecimal");
		dec2.setDisplayName("My Decimal");
		d.getAttributes().add(dec2);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertNotNull(result.getAttribute("myDecimal"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertWithAssociationAttributeSucceeds() {
		DocumentMetaData d = minimalTransientDoc();
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("myAssoc");
		assoc.setDisplayName("My Association");
		assoc.setDocumentName("OtherDoc");
		assoc.setType(AssociationType.aggregation);
		d.getAttributes().add(assoc);
		org.skyve.metadata.model.document.Document result = d.convert("test.TestDoc");
		assertNotNull(result);
		assertNotNull(result.getAttribute("myAssoc"));
	}
}

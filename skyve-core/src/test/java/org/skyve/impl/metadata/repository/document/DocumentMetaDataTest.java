package org.skyve.impl.metadata.repository.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.InterfaceImpl;
import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.impl.metadata.repository.document.FieldReference;
import org.skyve.impl.metadata.repository.document.UniqueConstraint;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;

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
		assertTrue(Boolean.TRUE.equals(doc.getAudited()));
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
		assertTrue(Boolean.TRUE.equals(doc.getAbstract()));
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
}

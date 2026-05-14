package org.skyve.impl.metadata.repository.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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
}

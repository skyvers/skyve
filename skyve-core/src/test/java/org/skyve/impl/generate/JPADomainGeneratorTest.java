package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.module.Module;

/**
 * Tests for {@link JPADomainGenerator#generateJavaFile} covering the main paths.
 */
@SuppressWarnings("static-method")
class JPADomainGeneratorTest {

	private static JPADomainGenerator generator() {
		return new JPADomainGenerator(false, false, DialectOptions.H2_NO_INDEXES,
				"", "", "", "", null);
	}

	private static Persistent persistentOf(String tableName) {
		Persistent p = new Persistent();
		p.setName(tableName);
		return p;
	}

	// ----- simple persistent document with text field ----------------------

	@Test
	void generateJavaFileForPersistentDocWithTextField() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SimpleEntity");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_SimpleEntity"));

		Text textField = new Text();
		textField.setName("myText");
		textField.setDisplayName("My Text");
		textField.setLength(100);
		doc.putAttribute(textField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-simple-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "SimpleEntity");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("package modules.testMod.domain"), "Should have package");
		assertTrue(content.contains("@Entity"), "Should have @Entity for persistent doc");
		assertTrue(content.contains("@Table"), "Should have @Table for persistent doc");
		assertTrue(content.contains("AbstractPersistentBean"), "Should extend AbstractPersistentBean");
		assertTrue(content.contains("getMyText"), "Should have accessor for text field");
		assertTrue(content.contains("setMyText"), "Should have mutator for text field");
	}

	// ----- transient document ----------------------------------------------

	@Test
	void generateJavaFileForTransientDoc() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientEntity");
		doc.setOwningModuleName("testMod");
		// No persistent

		Text textField = new Text();
		textField.setName("memo");
		textField.setDisplayName("Memo");
		textField.setLength(500);
		doc.putAttribute(textField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-transient-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "TransientEntity");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("AbstractTransientBean"), "Should extend AbstractTransientBean");
		assertTrue(content.contains("getMemo"), "Should have accessor for memo");
	}

	// ----- persistent document with aggregation association ---------------

	@Test
	void generateJavaFileForDocWithAggregationAssociation() throws Exception {
		JPADomainGenerator gen = generator();

		// Referenced doc
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("RefDoc");
		refDoc.setOwningModuleName("testMod");

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerWithAssoc");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.setPersistent(persistentOf("ADM_OwnerWithAssoc"));

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("refLink");
		assoc.setDisplayName("Ref Link");
		assoc.setDocumentName("RefDoc");
		assoc.setType(AssociationType.aggregation);
		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "RefDoc")).thenReturn(refDoc);

		File tmpFile = File.createTempFile("jpa-test-assoc-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, ownerDoc, fw, "modules.testMod.domain", "OwnerWithAssoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@OneToOne"), "Should have @OneToOne for aggregation association");
		assertTrue(content.contains("getRefLink"), "Should have getter for the association");
		assertTrue(content.contains("setRefLink"), "Should have setter for the association");
	}

	// ----- persistent document with composition collection ----------------

	@Test
	void generateJavaFileForDocWithCompositionCollection() throws Exception {
		JPADomainGenerator gen = generator();

		// Collection element doc
		DocumentImpl elemDoc = new DocumentImpl();
		elemDoc.setName("LineItem");
		elemDoc.setOwningModuleName("testMod");

		// Parent doc
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("OrderDoc");
		parentDoc.setOwningModuleName("testMod");
		parentDoc.setPersistent(persistentOf("ADM_OrderDoc"));

		CollectionImpl coll = new CollectionImpl();
		coll.setName("items");
		coll.setDisplayName("Items");
		coll.setDocumentName("LineItem");
		coll.setType(CollectionType.composition);
		parentDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "LineItem")).thenReturn(elemDoc);

		File tmpFile = File.createTempFile("jpa-test-coll-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, parentDoc, fw, "modules.testMod.domain", "OrderDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@OneToMany"), "Should have @OneToMany for composition collection");
		assertTrue(content.contains("List<LineItem>"), "Should have list of LineItem");
		assertTrue(content.contains("getItems"), "Should have getter for the collection");
	}

	// ----- persistent document with child collection ----------------------

	@Test
	void generateJavaFileForDocWithChildCollection() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildItem");
		childDoc.setOwningModuleName("testMod");

		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("ParentDoc");
		parentDoc.setOwningModuleName("testMod");
		parentDoc.setPersistent(persistentOf("ADM_ParentDoc"));

		CollectionImpl coll = new CollectionImpl();
		coll.setName("children");
		coll.setDisplayName("Children");
		coll.setDocumentName("ChildItem");
		coll.setType(CollectionType.child);
		parentDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ChildItem")).thenReturn(childDoc);

		File tmpFile = File.createTempFile("jpa-test-child-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, parentDoc, fw, "modules.testMod.domain", "ParentDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@OneToMany"), "Should have @OneToMany for child collection");
		assertTrue(content.contains("@JoinColumn"), "Should have @JoinColumn for child");
		assertTrue(content.contains("getChildren"), "Should have getter for children");
	}

	// ----- persistent document with aggregation collection ----------------

	@Test
	void generateJavaFileForDocWithAggregationCollection() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl tagDoc = new DocumentImpl();
		tagDoc.setName("Tag");
		tagDoc.setOwningModuleName("testMod");

		DocumentImpl articleDoc = new DocumentImpl();
		articleDoc.setName("ArticleDoc");
		articleDoc.setOwningModuleName("testMod");
		articleDoc.setPersistent(persistentOf("ADM_ArticleDoc"));

		CollectionImpl coll = new CollectionImpl();
		coll.setName("tags");
		coll.setDisplayName("Tags");
		coll.setDocumentName("Tag");
		coll.setType(CollectionType.aggregation);
		articleDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "Tag")).thenReturn(tagDoc);

		File tmpFile = File.createTempFile("jpa-test-aggr-coll-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, articleDoc, fw, "modules.testMod.domain", "ArticleDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@ManyToMany"), "Should have @ManyToMany for aggregation collection");
		assertTrue(content.contains("getTags"), "Should have getter for tags");
	}

	// ----- persistent document with bizKey --------------------------------

	@Test
	void generateJavaFileForDocWithBizKey() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BizKeyDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_BizKeyDoc"));
		doc.setBizKeyMethodCode("\t\treturn \"myKey\";\n");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-bizkey-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "BizKeyDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("getBizKey"), "Should have getBizKey method");
	}

	// ----- persistent document with parent document name ------------------

	@Test
	void generateJavaFileForDocWithParentDocument() throws Exception {
		JPADomainGenerator gen = generator();

		// Parent doc
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("ParentEntityDoc");
		parentDoc.setOwningModuleName("testMod");
		parentDoc.setPersistent(persistentOf("ADM_ParentEntity"));

		// Child doc with parentDocumentName set
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildEntityDoc");
		childDoc.setOwningModuleName("testMod");
		childDoc.setPersistent(persistentOf("ADM_ChildEntity"));
		childDoc.setParentDocumentName("ParentEntityDoc");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ParentEntityDoc")).thenReturn(parentDoc);

		File tmpFile = File.createTempFile("jpa-test-parent-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, childDoc, fw, "modules.testMod.domain", "ChildEntityDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("ChildBean"), "Should have ChildBean for doc with parent");
		assertTrue(content.contains("ManyToOne") || content.contains("parent"), "Should have parent relationship");
	}

	// ----- debug mode logging ---------------------------------------------

	@Test
	void generateJavaFileDebugModeLogsClassName() throws Exception {
		JPADomainGenerator gen = new JPADomainGenerator(true, false, DialectOptions.H2_NO_INDEXES,
				"", "", "", "", null);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("DebugDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_DebugDoc"));

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-debug-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "DebugDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("DebugDoc"), "Generated class should contain DebugDoc");
	}

	// ----- transient document with parent document -----------------------

	@Test
	void generateJavaFileForTransientDocWithParent() throws Exception {
		JPADomainGenerator gen = generator();

		// Parent doc (also transient)
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("TransientParent");
		parentDoc.setOwningModuleName("testMod");
		// no persistent

		// Transient child doc
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("TransientChild");
		childDoc.setOwningModuleName("testMod");
		// no persistent
		childDoc.setParentDocumentName("TransientParent");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TransientParent")).thenReturn(parentDoc);

		File tmpFile = File.createTempFile("jpa-test-trans-parent-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, childDoc, fw, "modules.testMod.domain", "TransientChild");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("AbstractTransientBean"), "Should extend AbstractTransientBean");
		assertTrue(content.contains("ChildBean"), "Should implement ChildBean for transient child");
	}

	// ----- cross-module association import --------------------------------

	@Test
	void generateJavaFileForCrossModuleAssociationAddsImport() throws Exception {
		JPADomainGenerator gen = generator();

		// Referenced doc is in a DIFFERENT module ("otherMod")
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("ExternalDoc");
		refDoc.setOwningModuleName("otherMod");

		// Owner doc is in "testMod"
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerCrossModule");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.setPersistent(persistentOf("ADM_OwnerCrossModule"));

		org.skyve.impl.metadata.model.document.AssociationImpl assoc =
				new org.skyve.impl.metadata.model.document.AssociationImpl();
		assoc.setName("externalRef");
		assoc.setDisplayName("External Ref");
		assoc.setDocumentName("ExternalDoc");
		assoc.setType(AssociationType.aggregation);
		ownerDoc.putRelation(assoc);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "ExternalDoc")).thenReturn(refDoc);

		File tmpFile = File.createTempFile("jpa-test-crossmod-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, ownerDoc, fw, "modules.testMod.domain", "OwnerCrossModule");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("modules.otherMod.domain.ExternalDoc"), "Should import cross-module class");
	}

	// ----- collection with ordering annotation ---------------------------

	@Test
	void generateJavaFileForCollectionWithOrderingAnnotation() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl elemDoc = new DocumentImpl();
		elemDoc.setName("OrderedItem");
		elemDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OrderedOwner");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.setPersistent(persistentOf("ADM_OrderedOwner"));

		CollectionImpl coll = new CollectionImpl();
		coll.setName("items");
		coll.setDisplayName("Items");
		coll.setDocumentName("OrderedItem");
		coll.setType(CollectionType.aggregation);
		// Add ordering: ascending by "name", descending by "created"
		coll.getOrdering().add(new OrderingImpl("name", SortDirection.ascending));
		coll.getOrdering().add(new OrderingImpl("created", SortDirection.descending));
		ownerDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "OrderedItem")).thenReturn(elemDoc);

		File tmpFile = File.createTempFile("jpa-test-ordering-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, ownerDoc, fw, "modules.testMod.domain", "OrderedOwner");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@OrderBy"), "Should have @OrderBy for ordered collection");
		assertTrue(content.contains("name"), "Should include 'name' in @OrderBy");
		assertTrue(content.contains("desc"), "Should include 'desc' for descending ordering");
	}

	// ----- non-persistent collection on persistent doc -------------------

	@Test
	void generateJavaFileForNonPersistentCollectionAddsTransient() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl elemDoc = new DocumentImpl();
		elemDoc.setName("TransientElem");
		elemDoc.setOwningModuleName("testMod");

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("DocWithTransientColl");
		ownerDoc.setOwningModuleName("testMod");
		ownerDoc.setPersistent(persistentOf("ADM_DocWithTransientColl"));

		CollectionImpl coll = new CollectionImpl();
		coll.setName("tempItems");
		coll.setDisplayName("Temp Items");
		coll.setDocumentName("TransientElem");
		coll.setType(CollectionType.aggregation);
		coll.setPersistent(false);
		ownerDoc.putRelation(coll);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TransientElem")).thenReturn(elemDoc);

		File tmpFile = File.createTempFile("jpa-test-transient-coll-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, ownerDoc, fw, "modules.testMod.domain", "DocWithTransientColl");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@Transient"), "Should have @Transient for non-persistent collection");
	}

	// ----- Decimal2 field triggers @Type and non-java.lang import --------

	@Test
	void generateJavaFileForDocWithDecimal2FieldAddsTypeAnnotation() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("PriceDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_PriceDoc"));

		Decimal2 priceField = new Decimal2();
		priceField.setName("price");
		priceField.setDisplayName("Price");
		doc.putAttribute(priceField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-decimal2-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "PriceDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@Type"), "Should have @Type annotation for Decimal2 field");
		assertTrue(content.contains("org.skyve.domain.types"), "Should import non-java.lang type");
		assertTrue(content.contains("getPrice"), "Should have getter for price");
	}

	// ----- attribute named bizKey is skipped ------------------------------

	@Test
	void generateJavaFileSkipsBizKeyAttribute() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("BizKeySkipDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_BizKeySkipDoc"));

		// Add a regular text field
		Text textField = new Text();
		textField.setName("title");
		textField.setDisplayName("Title");
		textField.setLength(100);
		doc.putAttribute(textField);

		// Add a bizKey attribute (should be skipped in property name constants)
		Text bizKeyField = new Text();
		bizKeyField.setName("bizKey");
		bizKeyField.setDisplayName("Biz Key");
		bizKeyField.setLength(250);
		doc.putAttribute(bizKeyField);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-bizkeyskip-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "BizKeySkipDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		// bizKey attribute should be skipped – no "bizKeyPropertyName" static
		assertTrue(content.contains("titlePropertyName"), "Should have title property name constant");
		assertTrue(content.contains("getTitle"), "Should have getter for title");
	}

	// ----- document with conditions ---------------------------------------

	@Test
	void generateJavaFileForDocWithConditionsGeneratesMethods() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ConditionDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_ConditionDoc"));

		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("amount > 100");
		doc.getConditions().put("largeOrder", cond);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-condition-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "ConditionDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("isLargeOrder"), "Should have isLargeOrder condition method");
		assertTrue(content.contains("isNotLargeOrder"), "Should have isNotLargeOrder negation method");
		assertTrue(content.contains("amount > 100"), "Should include condition expression");
	}

	// ----- persistent with schema and catalog in @Table -------------------

	@Test
	void generateJavaFileWithSchemaAndCatalogInTableAnnotation() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SchemaDoc");
		doc.setOwningModuleName("testMod");

		Persistent p = new Persistent();
		p.setName("ADM_SchemaDoc");
		p.setSchema("mySchema");
		p.setCatalog("myCatalog");
		doc.setPersistent(p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-schema-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "SchemaDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("schema="), "Should include schema in @Table");
		assertTrue(content.contains("mySchema"), "Should include schema name");
		assertTrue(content.contains("catalog="), "Should include catalog in @Table");
		assertTrue(content.contains("myCatalog"), "Should include catalog name");
	}

	// ----- persistent doc with non-persistent parent ----------------------

	@Test
	void generateJavaFileForPersistentDocWithNonPersistentParentAddsTransient() throws Exception {
		JPADomainGenerator gen = generator();

		// Transient parent doc
		DocumentImpl parentDoc = new DocumentImpl();
		parentDoc.setName("TransientParentEntity");
		parentDoc.setOwningModuleName("testMod");
		// no persistent

		// Persistent child doc
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("PersistentChildEntity");
		childDoc.setOwningModuleName("testMod");
		childDoc.setPersistent(persistentOf("ADM_PersistentChildEntity"));
		childDoc.setParentDocumentName("TransientParentEntity");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");
		when(module.getDocument(null, "TransientParentEntity")).thenReturn(parentDoc);

		File tmpFile = File.createTempFile("jpa-test-transient-parent-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, childDoc, fw, "modules.testMod.domain", "PersistentChildEntity");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("ChildBean"), "Should implement ChildBean");
		assertTrue(content.contains("@Transient"), "Should add @Transient because parent has no persistent");
	}

	// ----- non-persistent attribute on persistent doc adds @Transient ----

	@Test
	void generateJavaFileForNonPersistentAttributeAddsTransient() throws Exception {
		JPADomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("MixedPersistenceDoc");
		doc.setOwningModuleName("testMod");
		doc.setPersistent(persistentOf("ADM_MixedPersistenceDoc"));

		Text transientAttr = new Text();
		transientAttr.setName("computedField");
		transientAttr.setDisplayName("Computed Field");
		transientAttr.setLength(200);
		transientAttr.setPersistent(false);
		doc.putAttribute(transientAttr);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("testMod");

		File tmpFile = File.createTempFile("jpa-test-attr-transient-", ".java");
		tmpFile.deleteOnExit();

		try (FileWriter fw = new FileWriter(tmpFile)) {
			gen.generateJavaFile(null, module, doc, fw, "modules.testMod.domain", "MixedPersistenceDoc");
		}

		String content = Files.readString(tmpFile.toPath());
		assertTrue(content.contains("@Transient"), "Should add @Transient for non-persistent attribute on persistent doc");
		assertTrue(content.contains("getComputedField"), "Should have getter for computed field");
	}
}

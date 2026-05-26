package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.module.Module;

/**
 * Tests for {@link OverridableDomainGenerator} ORM-generation private methods:
 * {@code generateORM}, {@code generateAttributeMappings}.
 */
@SuppressWarnings("static-method")
class OverridableDomainGeneratorORMTest {

	/** Instantiate a generator with no extended-context requirements. */
	private static OverridableDomainGenerator generator() {
		return new OverridableDomainGenerator(false, false, false,
				DialectOptions.H2_NO_INDEXES, "", "", "", "", null);
	}

	/** Set the private {@code repository} field via reflection. */
	private static void setRepository(OverridableDomainGenerator gen,
			ProvidedRepository repo) throws Exception {
		Field f = org.skyve.impl.generate.DomainGenerator.class.getDeclaredField("repository");
		f.setAccessible(true);
		f.set(gen, repo);
	}

	/** Populate {@code persistentPropertyLengths} with one entry for a document. */
	@SuppressWarnings("unchecked")
	private static void seedPropertyLengths(OverridableDomainGenerator gen,
			Persistent persistent, String fieldName, int length) throws Exception {
		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		TreeMap<String, TreeMap<String, Integer>> ppl =
				(TreeMap<String, TreeMap<String, Integer>>) f.get(gen);
		TreeMap<String, Integer> docLengths = ppl.computeIfAbsent(
				persistent.getPersistentIdentifier(), k -> new TreeMap<>());
		docLengths.put(fieldName, length);
	}

	/** Get the private 9-arg {@code generateORM} method via reflection. */
	private static Method ormMethod() throws Exception {
		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateORM",
				StringBuilder.class,     // contents
				Module.class,            // module
				Document.class,          // document
				String.class,            // packagePathPrefix
				boolean.class,           // forExt
				boolean.class,           // recursive
				org.skyve.metadata.customer.Customer.class, // customer
				StringBuilder.class,     // filterDefinitions
				String.class);           // indentation
		m.setAccessible(true);
		return m;
	}

	/** Get the private {@code generateAttributeMappings} method via reflection. */
	private static Method attrMappingsMethod() throws Exception {
		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateAttributeMappings",
				StringBuilder.class,     // contents
				org.skyve.metadata.customer.Customer.class, // customer
				Module.class,            // module
				Document.class,          // document
				Persistent.class,        // persistent
				String.class,            // columnPrefix
				Set.class,               // columnNames
				String.class,            // owningDocumentName
				boolean.class,           // forExt
				String.class);           // indentation
		m.setAccessible(true);
		return m;
	}

	// ---- generateAttributeMappings ----------------------------------------

	@Test
	void generateAttributeMappingsWithTextFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("AttrDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_AttrDoc");
		doc.setPersistent(p);

		Text textField = new Text();
		textField.setName("title");
		textField.setDisplayName("Title");
		textField.setLength(200);
		doc.putAttribute(textField);

		seedPropertyLengths(gen, p, "title", 200);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<property name=\"title\""), "Should have property element");
		assertTrue(result.contains("length=\"200\""), "Should have length");
	}

	@Test
	void generateAttributeMappingsWithTransientFieldSkipsProperty() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientAttrDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_TransientAttrDoc");
		doc.setPersistent(p);

		Text transientField = new Text();
		transientField.setName("computed");
		transientField.setDisplayName("Computed");
		transientField.setLength(100);
		transientField.setPersistent(false);
		doc.putAttribute(transientField);

		seedPropertyLengths(gen, p, "computed", 100);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertFalse(result.contains("<property name=\"computed\""), "Transient field should be skipped");
	}

	@Test
	void generateAttributeMappingsWithChildCollectionProducesBagElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Referenced child doc
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildItem");
		childDoc.setOwningModuleName("myMod");
		Persistent childPersistent = new Persistent();
		childPersistent.setName("MOD_ChildItem");
		childDoc.setPersistent(childPersistent);

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_OwnerDoc");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("children");
		coll.setDisplayName("Children");
		coll.setDocumentName("ChildItem");
		coll.setType(CollectionType.child);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "ChildItem")).thenReturn(childDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<bag name=\"children\""), "Should have bag element for child collection");
		assertTrue(result.contains("<key column=\"parent_id\""), "Should have parent_id key");
		assertTrue(result.contains("<one-to-many entity-name=\"myModChildItem\""), "Should have one-to-many");
	}

	@Test
	void generateAttributeMappingsWithAggregationCollectionProducesManyToManyElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Tag");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Tag");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("ArticleDoc");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_ArticleDoc");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("tags");
		coll.setDisplayName("Tags");
		coll.setDocumentName("Tag");
		coll.setType(CollectionType.aggregation);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Tag")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("MOD_ArticleDoc_tags"), "Should have join table name for aggregation collection");
		assertTrue(result.contains("<many-to-many"), "Should have many-to-many element");
	}

	// ---- generateORM ------------------------------------------------------

	@Test
	void generateORMForPersistentDocProducesClassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ORMTestDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_ORMTestDoc");
		doc.setPersistent(p);

		Text textField = new Text();
		textField.setName("description");
		textField.setDisplayName("Description");
		textField.setLength(500);
		doc.putAttribute(textField);

		seedPropertyLengths(gen, p, "description", 500);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<class name="), "Should have class element");
		assertTrue(result.contains("table=\"MOD_ORMTestDoc\""), "Should have table name");
		assertTrue(result.contains("entity-name=\"myModORMTestDoc\""), "Should have entity name");
		assertTrue(result.contains("bizId"), "Should have bizId property");
		assertTrue(result.contains("</class>"), "Should close class element");
		assertTrue(result.contains("<property name=\"description\""), "Should have description property");
	}

	@Test
	void generateORMForTransientDocProducesNoClassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Transient doc (no persistent)
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientORM");
		doc.setOwningModuleName("myMod");
		// no persistent

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.isEmpty(), "Transient doc should produce no ORM output");
	}

	@Test
	void generateORMWithSchemaAndCatalogIncludesThem() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SchemaORMDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_SchemaORMDoc");
		p.setSchema("mySchema");
		p.setCatalog("myCatalog");
		doc.setPersistent(p);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("schema=\"mySchema\""), "Should include schema in class definition");
		assertTrue(result.contains("catalog=\"myCatalog\""), "Should include catalog in class definition");
	}

	@Test
	void generateORMForSubDocWithSingleStrategyProducesSubclassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Sub-document with single table strategy
		DocumentImpl subDoc = new DocumentImpl();
		subDoc.setName("SubDoc");
		subDoc.setOwningModuleName("myMod");
		Persistent subPersistent = new Persistent();
		subPersistent.setName("MOD_BaseDoc"); // uses base table
		subPersistent.setStrategy(ExtensionStrategy.single);
		subDoc.setPersistent(subPersistent);

		org.skyve.metadata.model.Extends inherits = new org.skyve.metadata.model.Extends();
		inherits.setDocumentName("BaseDoc");
		subDoc.setExtends(inherits);

		// Base document
		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseDoc");
		baseDoc.setOwningModuleName("myMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setName("MOD_BaseDoc");
		// no strategy = default (not single)
		baseDoc.setPersistent(basePersistent);

		seedPropertyLengths(gen, subPersistent);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		// findNearest returns baseDoc to indicate SubDoc is a subclass
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(baseDoc);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, subDoc, "modules.", false,
				true, // recursive=true to process subclass
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<subclass name="), "Should have subclass element for single strategy");
		assertTrue(result.contains("</subclass>"), "Should close subclass element");
	}

	@Test
	void generateAttributeMappingsWithAssociationAggregationProducesManyToOneElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Referenced doc
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Category");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Category");
		refDoc.setPersistent(refPersistent);

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Product");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Product");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("category");
		assoc.setDisplayName("Category");
		assoc.setDocumentName("Category");
		assoc.setType(AssociationType.aggregation);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Category")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<many-to-one name=\"category\""), "Should have many-to-one for aggregation association");
		assertTrue(result.contains("entity-name=\"myModCategory\""), "Should reference entity name");
		assertTrue(result.contains("cascade=\"persist,save-update,refresh,merge\""), "Aggregation cascade");
	}

	@Test
	void generateAttributeMappingsWithAssociationCompositionProducesUniqueManyToOne() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Address");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Address");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Contact");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Contact");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("homeAddress");
		assoc.setDisplayName("Home Address");
		assoc.setDocumentName("Address");
		assoc.setType(AssociationType.composition);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Address")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<many-to-one name=\"homeAddress\""), "Should have many-to-one for composition association");
		assertTrue(result.contains("unique=\"true\""), "Composition should be unique");
		assertTrue(result.contains("cascade=\"persist,save-update,refresh,delete-orphan,merge\""), "Composition cascade");
	}

	@Test
	void generateAttributeMappingsWithCompositionCollectionProducesAllDeleteOrphanCascade() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("LineItem");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_LineItem");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Order");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Order");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("lineItems");
		coll.setDisplayName("Line Items");
		coll.setDocumentName("LineItem");
		coll.setType(CollectionType.composition);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "LineItem")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("cascade=\"all-delete-orphan\""), "Composition collection uses all-delete-orphan");
		assertTrue(result.contains("MOD_Order_lineItems"), "Should have join table");
		assertTrue(result.contains("<many-to-many"), "Should have many-to-many element for composition collection");
	}

	@Test
	void generateAttributeMappingsWithEnumerationFieldProducesEnumTypeElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("StatusDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_StatusDoc");
		doc.setPersistent(p);

		Enumeration enumField = new Enumeration();
		enumField.setName("status");
		enumField.setDisplayName("Status");
		enumField.setOwningDocument(doc);
		EnumeratedValue val1 = new EnumeratedValue();
		val1.setCode("active");
		EnumeratedValue val2 = new EnumeratedValue();
		val2.setCode("inactive");
		enumField.getValues().add(val1);
		enumField.getValues().add(val2);
		doc.putAttribute(enumField);

		seedPropertyLengths(gen, p, "status", 30);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<property name=\"status\""), "Should have property element for enumeration");
		assertTrue(result.contains("<type name=\"Enum\">"), "Should have Enum type element");
	}

	@Test
	void generateAttributeMappingsWithTransientAssociationIsSkipped() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Ref");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Ref");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Owner");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Owner");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("transientRef");
		assoc.setDisplayName("Transient Ref");
		assoc.setDocumentName("Ref");
		assoc.setType(AssociationType.aggregation);
		assoc.setPersistent(false);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Ref")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertFalse(result.contains("<many-to-one"), "Transient association should be skipped");
	}

	@Test
	void generateORMForDocWithAssociationIncludesManyToOne() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Dept");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Dept");
		refDoc.setPersistent(refPersistent);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("Employee");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Employee");
		doc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("department");
		assoc.setDisplayName("Department");
		assoc.setDocumentName("Dept");
		assoc.setType(AssociationType.aggregation);
		doc.putRelation(assoc);

		seedPropertyLengths(gen, p);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Dept")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false, null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<class name="), "Should have class element");
		assertTrue(result.contains("<many-to-one name=\"department\""), "Should have many-to-one for association");
	}

	// ---- helper: seed persistentPropertyLengths with empty inner map ------

	@SuppressWarnings("unchecked")
	private static void seedPropertyLengths(OverridableDomainGenerator gen,
			Persistent persistent) throws Exception {
		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		TreeMap<String, TreeMap<String, Integer>> ppl =
				(TreeMap<String, TreeMap<String, Integer>>) f.get(gen);
		ppl.computeIfAbsent(persistent.getPersistentIdentifier(), k -> new TreeMap<>());
	}
}
